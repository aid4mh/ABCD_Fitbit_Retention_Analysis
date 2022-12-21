## Functions for formatting data ############################################

# Libraries necessary for functions
require(dplyr)
require(tidyr)
require(tibble)
require(magrittr)
require(lubridate)
require(stringr)

create_start_date <- function(data, participant, obs_period) {
  
  participant_data <- data.table(data)
  setkey(participant_data, participant_id)
  participant_data <- participant_data[J(participant)] %>%
    as.data.frame() %>%
    mutate(date = as_datetime(wear_date)) %>%
    arrange(date)
  
  start_date <- participant_data %>%
    group_by(eventname) %>%
    filter(row_number() == 1) %>%
    rename(start_date = date) %>%
    select(start_date, eventname)
  
  df <- participant_data %>%
    left_join(start_date, by = "eventname") %>%
    mutate(protocol_date = date - start_date) %>%
    mutate(protocol_date = (protocol_date/(86400))+1) %>%
    mutate(protocol_date = as.integer(protocol_date)) %>%
    filter(protocol_date <= obs_period)
  
  return(df)
}

# Create sleep or physical activity-specific Fitbit data
create_activitySpecific_data <- function(activity_type, daily_summary_data, obs_period = 21) {
  #' Creates activity-specific Fitbit data from daily summary data
  #' 
  #' @activity_type (string) Type of activity, can only equal "sleep" or "physical"
  #' @daily_summary_data Daily summary data of either sleep or physical
  #' @obs_period (numeric) Number of days for observation period, default = 21
  
  # Check activity_type
  if (!(activity_type == "sleep" || activity_type == "physical")) {
    stop(paste0('You entered ', activity_type, ' as activity_type. It must equal sleep or physical. Please try again.'))
    
  } else if (activity_type == "sleep") {
    
    df <- daily_summary_data %>%
      # Select sleep-specific features
      select(participant_id, sleepdate, total_sleep_minutes:avg_hr_rem)
    
  } else if (activity_type == "physical") {
    
    df <- daily_summary_data %>%
      select(participant_id, wear_date:dayt_very_active_min) %>%
      select(-weekday, -wkno, -weekend_ind, -first_hr_date) %>%
      # remove any sleep or night-specific data; should be included in the sleep data
      select(-contains("sleep"), -contains("night"), -contains("gt_600"))
    
  }
  # Normalize start date; select days to observation period
  df %<>% normalizeStartDate() %>% filter(modified_date <= obs_period)
  
  return(df)
}

# Function to create total # of days of activity per phase, per activity type
# per participant
create_timepoint_count <- function(fitbit_daily_sleep_data,
                                   fitbit_daily_activity_data) {
  
  # Creates a dataframe that has the counts for the number of 
  # days of recorded activity per activity type, phase and participant
  create_count_dfs <- function(data, data_type) {
    
    # Filter for the baseline phase and count days
    baseline_df <- data %>%
      filter(eventname == "baseline_year_1_arm_1") %>%
      group_by(participant_id) %>%
      summarize(n()) 
    
    # Filter for Year 2 follow-up phase and count days
    followup_df <- data %>%
      filter(eventname == "2_year_follow_up_y_arm_1") %>%
      group_by(participant_id) %>%
      summarize(n()) 
    
    # If the activity type is physical
    if (data_type == "physical") {
      baseline_df %<>%
        rename(baseline_count_physical = 'n()')
      followup_df %<>%
        rename(followup_count_physical = 'n()')
      # If the activity type if sleep
    } else if (data_type == "sleep") {
      baseline_df %<>%
        rename(baseline_count_sleep = 'n()')
      followup_df %<>%
        rename(followup_count_sleep = 'n()')
    }
    
    # Join the follow-up and baseline dataframes by participant
    count_df <- full_join(followup_df, baseline_df,
                          by = "participant_id") %>%
      # replace NA values with 0
      mutate(across(everything(), ~ replace_na(.x, 0)))
    
    # Create total # of recorded activity across phases per activity type
    if (data_type == "physical") {
      count_df %<>%
        mutate(total_days_physical = followup_count_physical + baseline_count_physical)
    } else if (data_type == "sleep") {
      count_df %<>%
        mutate(total_days_sleep = followup_count_sleep + baseline_count_sleep)
    }
    
    # Replace 0 with NA to represent no recorded values
    count_df %<>%
      mutate(across(everything(), ~ na_if(.x, 0)))
    
    return(count_df)
  }
  
  # Create sleep-specific count dataframe
  sleep_df <- create_count_dfs(fitbit_daily_sleep_data, "sleep")
  # Create activity-specific count dataframe
  physical_df <- create_count_dfs(fitbit_daily_activity_data, "physical")
  
  # Create total count dataframe across activities
  count_df <- full_join(sleep_df, physical_df,
                        by = "participant_id")
  
  return(count_df)
}

# Create binarized compliance data
create_perDay_activity <- function(activity_type, data, pID_list) {
  #' Creates per-day Fitbit activity data
  #' 
  #' @activity_type Type of Fitbit activity
  #' @data Created by normalizeStartDate
  #' @pID_list List of Fitbit participant ID's
  
  # Check activity_type
  if (!(activity_type == "sleep" || activity_type == "physical")) {
    stop(paste0('You entered ', activity_type, ' as activity_type. It must equal sleep or physical. Please try again.'))
    
  } else if (activity_type == "sleep" || activity_type == "physical") {
    print(paste0("Creating per day ", activity_type, " activity data..."))
    # Format data
    activity_df <- data %>%
      # filter by Fitbit participants
      filter(participant_id %in% pID_list) %>%
      group_by(participant_id) %>%
      # select relevant columns
      select(participant_id, modified_date) %>%
      rename(date = modified_date) %>%
      # create pID column to assign numeric participant ID
      mutate(pID = cur_group_id()) %>%
      # Pivot to wider format:
      # each day is a column, each participant is a row
      pivot_wider(names_from = date, values_from = pID) %>%
      # mutate such that matrix is binary representation
      mutate(across(where(is.numeric), ~ case_when(
        !is.na(.) ~ 1,
        is.na(.) ~ 0))) %>%
      # change to data frame to allow rowname assignment
      as.data.frame()
    # assign participant_id to rownames
    rownames(activity_df) <- activity_df$participant_id
    # remove participant_id column
    activity_df$participant_id <- NULL
    # create ordered date vector
    date_order <- as.character(sort(as.numeric(colnames(activity_df))))
    # Order columns in dataframe to the ordered vector
    activity_df %<>% select(all_of(date_order)) %>% as.matrix()
    
    # Format according to activity type
    if (activity_type == "sleep") {
      activity_df %<>% as.data.frame() %>%
        rownames_to_column(var = "participant_id")
      names(activity_df) <- paste0("sleep_", names(activity_df))
      activity_df %<>% rename(participant_id = sleep_participant_id)
      return(activity_df)
    } else if (activity_type == "physical") {
      activity_df %<>% as.data.frame() %>%
        rownames_to_column(var = "participant_id")
      names(activity_df) <- paste0("phys_", names(activity_df))
      activity_df %<>% rename(participant_id = phys_participant_id)
      return(activity_df)
    }
  }
}
# Combines activity date columns
combine_activityDate_columns <- function(combined_data, sleep_col, phys_col, combined_col) {
  #' Combines per-day sleep and physical activity data 
  #' 
  #' @combined_data 
  #' @sleep_col
  #' @phys_col
  #' @combined_col
  
  cols_to_select <- c(sleep_col, phys_col)
  combined_data %<>%
    unite(!!combined_col, all_of(cols_to_select), remove = T,
          sep = ",") %>%
    select(!!combined_col)
  return(combined_data)
} 
# Create daily compliance data based on sleep and physical activity
create_dailyCompliance_data <- function(sleep_activity_data, physical_activity_data) {
  #' Creates daily compliance data using sleep and physical activity data;
  #' any activity is counted as a valid point.
  #' 
  #' @sleep_activity_data Created from create_activitySpecific_data with activity_type == sleep
  #' @physical_activity_data Created from create_activitySpecific_data with activity_type == physical

  compliance_data <- inner_join(sleep_activity_data, physical_activity_data,
                   by = "participant_id") %>%
    mutate(across(everything(), ~ as.character(.)))
  
  ## Loop to create binary combined compliance data
  for (i in as.character(c(1:21))) {
    # create column reference for sleep data
    sleep_col <- paste0("sleep_", i)
    # create column reference for physical activity data
    phys_col <- paste0("phys_", i)
    # create combined column of sleep, physical activity data
    combined_col <- combine_activityDate_columns(
      compliance_data, sleep_col, phys_col, i
    )
    # Add column to combined daily compliance data
    compliance_data %<>%
      add_column(combined_col)
  }
  
  ### Format daily compliance data;
  ## Columns contain strings of either (1,1), (0,1), (1,0) or (0,0);
  ## If a cell has a 1 in any combination, indicates user provided data;
  ## so, we convert any combination with a 1 into a singular 1;
  ## However, a cell with (0,0) indicates no data for either activity;
  ## so, we convert those cells to 0
  
  pID_compliance <- compliance_data %>%
    select(participant_id) %>% pull()
  compliance_data %<>%
    # Select the combined columns
    select(`1`:`21`) %>%
    mutate(across(everything(), ~ str_replace_all(., "1,1", "1"))) %>%
    mutate(across(everything(), ~ str_replace_all(., "0,1", "1"))) %>%
    mutate(across(everything(), ~ str_replace_all(., "1,0", "1"))) %>%
    mutate(across(everything(), ~ str_replace_all(., "0,0", "0"))) %>%
    # mutate all columns to numeric, as they're string prior
    mutate(across(everything(), ~ as.numeric(.))) %>%
    add_column(participant_id = pID_compliance) %>% select(participant_id, everything())
  
  return(compliance_data)
}
# Create lists of activity-specific participant ID's
create_activitySpecificPID_list <- function(sleep_activity_data, physical_activity_data) {
  #' Creates a dataframe of participant ID's for each type of Fitbit data;
  #' 
  #' @sleep_activity_data Created from create_activitySpecific_data with activity_type == sleep
  #' @physical_activity_data Created from create_activitySpecific_data with activity_type == physical
  
  # Create df of participants who gave physical activity data
  participant_id_phys <- physical_activity_data %>%
    select(participant_id) %>%
    distinct(participant_id, .keep_all = T) %>%
    add_column(type = "activity")
  
  # Create df of participants who gave sleep activity data
  participant_id_sleep <- sleep_activity_data %>%
    select(participant_id) %>%
    distinct(participant_id, .keep_all = T) %>%
    add_column(type = "sleep")
  
  # Create df overall df of Fitbit data compliance from participants
  participant_id_overall <- full_join(participant_id_phys,
                                      participant_id_sleep,
                                      by = "participant_id",
                                      suffix = c("_phys", "_sleep"))
  
  activity_agnostic_pID <- participant_id_overall %>%
    filter(!is.na(type_phys)) %>% 
    filter(!is.na(type_sleep)) %>%
    select(participant_id) %>%
    # this participant does not have sleep data
    filter(participant_id != "NDAR_INV3PNCV5A7") %>%
    pull()
  
  sleep_only_pID <- participant_id_overall %>%
    filter(!is.na(type_sleep) & is.na(type_phys)) %>%
    select(participant_id) %>%
    pull()
  
  phys_only_pID <- participant_id_overall %>%
    filter(!is.na(type_phys) & is.na(type_sleep)) %>%
    select(participant_id) %>%
    pull()
  
  pID_list <- list(
    "activity_agnostic" = activity_agnostic_pID, 
    "sleep_only" = sleep_only_pID,
    "physical_only" = phys_only_pID
  )
  
  return(pID_list)
}

# Raw Fitbit data summarized by day
summarize_raw_fitbit_wear <- function(dataset) {
  df <- dataset %>%
    group_by(participant_id, protocol_date) %>%
    summarize(total_wear_min = sum(total_minutes)) %>%
    group_by(participant_id) %>%
    pivot_wider(names_from = protocol_date,
                values_from = total_wear_min) %>%
    mutate(across(everything(), ~replace_na(.,0))) %>%
    as.data.frame() %>%
    column_to_rownames(var ="participant_id") %>%
    as.matrix()
  return(df)
}

format_qa_qc_datasets <- function(data_type, fitbit_participant_list) {
  
  filePath <- paste0("data/processed_fitbit/",data_type,".csv")
  df <- fread(filePath) %>%
    filter(participant_id %in% fitbit_participant_list)
  if (data_type == "cohortDailyHrOutliers") {
    df %<>% mutate(value = value/1440*100)
  } 
  df %<>%
    pivot_wider(names_from = protocol_date,
                values_from = value) %>%
    as.data.frame() %>%
    column_to_rownames(var = "participant_id") %>%
    select('2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12',
           '13', '14', '15', '16', '17', '18', '19', '20', '21') %>%
    rename('2*' = '2') %>%
    as.matrix()
  return(df)
}

select_qaqc_feature <- function(qaqc_data, feature_to_select) {
  print(paste0("Selecting ", feature_to_select))
  df <- qaqc_data %>%
    select(modified_date, participant_id, {{ feature_to_select }}) %>%
    rename(protocol_date = modified_date) %>%
    mutate(!!feature_to_select := (.data[[feature_to_select]]/1440)*24)
  print(paste0("Creating day-to-day matrix..."))
  colName <- paste0("total_min_", feature_to_select)
  df %<>%
    group_by(participant_id, protocol_date) %>%
    summarize(!!colName := sum(.data[[feature_to_select]])) %>%
    group_by(participant_id) %>%
    pivot_wider(names_from = protocol_date,
                values_from = {{colName}}) %>%
    mutate(across(everything(), ~replace_na(.,0))) %>%
    as.data.frame() %>%
    column_to_rownames(var ="participant_id") %>%
    as.matrix()
  return(df)
}

bmi_percentile <- function(value, age, sex) {
  # Values found here: https://www-cdc-gov.myaccess.library.utoronto.ca/growthcharts/percentile_data_files.htm
  if (sex == "M") {
    if (age == 9) {
      coeffs <- list("L" = -2.971148225, "M" = 16.16712234, "S" = 0.111720691)
    } else if (age == 10) {
      coeffs <- list("L" = -2.765648008, "M" = 16.64613844, "S" = 0.120112464)
    } else if (age == 11) {
      coeffs <- list("L" = -2.590560148, "M" = 17.20088732, "S" = 0.126734613)
    } else if (age == 12) {
      coeffs <- list("L" = -2.447426113, "M" = 17.81463359, "S" = 0.131389042)
    } else if (age == 13) {
      coeffs <- list("L" = -2.3294571, "M" = 18.47179706, "S" = 0.13414147)
    } else if (age == 14) {
      coeffs <- list("L" = -2.227362173, "M" = 19.15758672, "S" = 0.135251083)
    }
  } else if (sex == "F") {
    if (age == 9) {
      coeffs <- list("L" = -2.360920527, "M" = 16.30609316, "S" = 0.128013515)
    } else if (age == 10) {
      coeffs <- list("L" = -2.171295888, "M" = 16.86231366, "S" = 0.137057004)
    } else if (age == 11) {
      coeffs <- list("L" = -2.045235058, "M" = 17.46906585, "S" = 0.143868341)
    } else if (age == 12) {
      coeffs <- list("L" = -1.975521156, "M" = 18.10148804, "S" = 0.148361495)
    } else if (age == 13) {
      coeffs <- list("L" = -1.954977947, "M" = 18.73643338, "S" = 0.150705138)
    } else if (age == 14) {
      coeffs <- list("L" = -1.977073595, "M" = 19.35257209, "S" = 0.151255713)
    }
  }
  z_score <- ((value/coeffs$M)^coeffs$L-1)/(coeffs$L*coeffs$S)
  return(z_score)
}

filter_duplicate_timepoints <- function(heartrate_data) {
  
  doubled_participants <- heartrate_data %>%
    group_by(participant_id, timepoint) %>%
    summarize(count = n()) %>%
    filter(n() > 1) %>%
    filter(timepoint == "2_year_follow_up_y_arm_1") %>%
    select(-count)
  
  single_participants <- heartrate_data %>%
    group_by(participant_id, timepoint) %>%
    summarize(count = n()) %>%
    filter(n() < 2) %>%
    select(-count)
  
  doubled_participants <- heartrate_data %>%
    inner_join(doubled_participants,
               by = c("timepoint" = "timepoint", 
                      "participant_id" = "participant_id"))
  
  single_participants <- heartrate_data %>%
    inner_join(single_participants, 
               by = c("timepoint" = "timepoint", 
                      "participant_id" = "participant_id"))
  
  filtered_heartrate_data <- rbind(
    single_participants, doubled_participants)
  
  return(filtered_heartrate_data)
}

