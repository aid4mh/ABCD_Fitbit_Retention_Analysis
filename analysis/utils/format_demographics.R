### Functions for demographic data cleaning #############################

## Summarize demographic columns ----

# Summarize East Asian ethnicities
summarize_eastAsian_cols <- function(demog_data) {
  #' Summarizes East Asian-related ethnicities in demographics data
  #' @demog_data Demographics data to summarize
  
  summ_eastAsian_cols <- demog_data %>%
    mutate(race = str_replace_all(race, "Chinese", "East Asian")) %>%
    mutate(race = str_replace_all(race, "Japanese", "East Asian")) %>%
    mutate(race = str_replace_all(race, "Korean", "East Asian")) %>%
    mutate(race = str_replace_all(race, "Filipino", "Southeast Asian")) %>%
    mutate(race = str_replace_all(race, "Vietnamese", "Southeast Asian")) %>%
    mutate(race = str_replace_all(race, "East Asian,East Asian", "East Asian")) %>%
    mutate(race = str_replace_all(race, "_", "Not provided"))
  return(summ_eastAsian_cols)
}

# Summarize Polynesian ethnicities
summarize_polynesion_cols <- function(demog_data) {
  #' Summarizes Polynesian-related ethnicities in demographics data
  #' @demog_data Demographics data to summarize
  
  summ_polynesion_cols <- demog_data %>%
    mutate(race = str_replace_all(race, "Other Pacific Islander", "Polynesian")) %>%
    mutate(race = str_replace_all(race, "Guamanian", "Polynesian")) %>%
    mutate(race = str_replace_all(race, "Samoan", "Polynesian")) %>%
    mutate(race = str_replace_all(race, "Native Hawaiian", "Polynesian"))
  return(summ_polynesion_cols)
}

# Summarize ethnicities to the top four of each major ethnicity
summarize_to_topFour_ethnicity <- function(demog_data) {
  #' Summarizes ethnicities to the top four
  #' @demog_data Demographics data to summarize
  
  summ_topFour_ethnicity <- demog_data %>%
    mutate(race = case_when(
      ## White
      race == "White" ~ "White",
      race == "White,Native American Indian" ~ "White, Native American Indian",
      race == "White,Black/African American" ~ "White, Black/African American",
      race == "White,East Asian" ~ "White, East Asian",
      grepl("White,", race) ~ "White, Other",
      ## Black
      race == "Black/African American" ~ "Black",
      race == "Black/African American,Native American Indian" ~ "Black/African American, Native American Indian",
      grepl("Black/African American,", race) ~ "Black/African American, Other",
      ## East Asian
      race == "East Asian" ~ "East Asian",
      grepl("East Asian,", race) ~ "East Asian, Other",
      ## Native American
      race == "Native American Indian" ~ "Native American Indian",
      grepl("Native American Indian,", race) ~ "Native American Indian, Other",
      ## Asian Indian
      race == "Asian Indian" ~ "Asian Indian",
      grepl("Asian Indian,", race) ~ "Asian Indian, Other",
      ## Other Asian
      race == "Other Asian" ~ "Other Asian",
      grepl("Other Asian,", race) ~ "Other Asian, Other",
      ## Southeast Asian 
      race == "Southeast Asian" ~ "Southeast Asian",
      grepl("Southeast Asian,", race) ~ "Southeast Asian, Other",
      ## Polynesian
      race == "Polynesian" ~ "Polynesian",
      grepl("Polynesian,", race) ~ "Polynesian, Other",
      ## Other race
      race == "Other race" ~ "Other race",
      grepl("Other race,", race) ~ "Other race",
      ## Not provided/refused to answer/did not know
      race == "Not provided" ~ "Not provided/Refused to answer/Did not know",
      race == "Refused to answer" ~ "Not provided/Refused to answer/Did not know",
      race == "Refused to answer,Don't know" ~ "Not provided/Refused to answer/Did not know",
      race == "Don't know" ~ "Not provided/Refused to answer/Did not know",
      race == "NA" ~ "Not provided/Refused to answer/Did not know"
    ))
  return(summ_topFour_ethnicity)
}
# Compresses ethnicity values
compress_ethnicity_col <- function(demog_data) {
  #' Compresses ethnicity values
  #' @demog_data Demographics data to summarize
  
  compressed_df <- demog_data %>%
    mutate(ethnicity = case_when(
      race == "White" ~ "White",
      race == "White, Black/African American" ~ "Multiracial/ethnic",
      race == "White, East Asian" ~ "Multiracial/ethnic",
      race == "White, Other" ~ "Multiracial/ethnic",
      race == "Black" ~ "Black",
      race == "Black/African American, Other" ~ "Multiracial/ethnic",
      race == "Native American Indian" ~ "AIAN/P",
      race == "White, Native American Indian" ~ "AIAN/P",
      race == "Native American Indian, Other" ~ "AIAN/P",
      race == "Black/African American, Native American Indian" ~ "AIAN/P",
      race == "East Asian" ~ "Asian",
      race == "East Asian, Other" ~ "Multiracial/ethnic",
      race == "Southeast Asian" ~ "Asian",
      race == "Southeast Asian, Other" ~ "Multiracial/ethnic",
      race == "Asian Indian" ~ "Asian",
      race == "Asian Indian, Other" ~ "Multiracial/ethnic",
      race == "Other Asian" ~ "Asian",
      race == "Other Asian, Other" ~ "Multiracial/ethnic",
      race == "Other race" ~ "Other",
      race == "Not provided/Refused to answer/Did not know" ~ "Other",
      race == "Polynesian" ~ "AIAN/P",
      race == "Polynesian, Other" ~ "AIAN/P"
    )) %>%
    select(-race)
  return(compressed_df)
}
# Summarizes ethnicity column using above functions
summarize_ethnicity_col <- function(demog_data) {
  summarized_ethnicity_df <- demog_data %>%
    ## Summarize ethnicity to simplify 
    # Polynesian summary
    summarize_polynesion_cols() %>%
    # East Asian summary
    summarize_eastAsian_cols() %>%
    # Summarize top 4 of each major ethnicity 
    summarize_to_topFour_ethnicity() %>%
    # Create uni/bi/multi-racial column
    create_multiracial_col() %>%
    ## Compress ethnicity column
    compress_ethnicity_col()
  return(summarized_ethnicity_df)
}
# Calculates uni/bi/multiracial values
create_multiracial_col <- function(demog_data) {
  #' Calculates multiracial stat for participants
  #' @demog_data Demographics data to summarize
  
  multiracial_cols <- demog_data %>%
    mutate(racial_combination = case_when(
      str_count(race, ",") == 0 ~ "uniracial",
      str_count(race, ",") == 1 ~ "biracial",
      str_count(race, ",") > 2 ~ "multiracial"
    ))
  return(multiracial_cols)
}
# Summarizes the birth regions 
summarize_birthRegion_cols <- function(demog_data) {
  #' Summarizes birth region cols
  #' @demog_data Demographics data to summarize
  summ_birth_region_cols <- demog_data %>%
    mutate(birth_region = case_when(
      grepl(" Europe", birth_region) ~ "Europe",
      !grepl(" Europe", birth_region) ~ birth_region
    ))
  if ("biofather_birth_region" %in% colnames(data)) {
    df %<>%
      mutate(biofather_birth_region = case_when(
        grepl(" Europe", biofather_birth_region) ~ "Europe",
        !grepl(" Europe", biofather_birth_region) ~ biofather_birth_region
      )) %>%
      mutate(biomother_birth_region = case_when(
        grepl(" Europe", biomother_birth_region) ~ "Europe",
        !grepl(" Europe", biomother_birth_region) ~ biomother_birth_region
      ))
  }
  return(summ_birth_region_cols)
}
# Summarize gender col
summarize_gender_col <- function(demog_data) {
  #' Summarizes gender col
  #' @demog_data Demographics data to summarize
  
  summ_gender_df <- demog_data %>%
    mutate(gender_summ = case_when(
      gender_id == "Different" ~ "Different/Gender Queer/Trans",
      gender_id == "Gender queer" ~ "Different/Gender Queer/Trans",
      gender_id == "Trans female" ~ "Different/Gender Queer/Trans",
      gender_id == "Trans male" ~ "Different/Gender Queer/Trans",
      gender_id == "Don't know" ~ "Don't know/Refused to answer",
      gender_id == "Refused" ~ "Don't know/Refused to answer",
      gender_id == "Female" ~ "Female",
      gender_id == "Male" ~ "Male"
    )) %>%
    select(-gender_id)
  return(summ_gender_df)
}
# Summarize education for parents
summarize_education_cols <- function(demog_data) {
  #' Summarizes education col for parents
  #' @demog_data Demographics data to summarize
  
  summ_education_df <- demog_data %>%
    mutate(education_summary = case_when(
      edu_level == "High school graduate" ~ "ISCED 3",
      edu_level == "12th grade" ~ "ISCED 3",
      edu_level == "11th grade" ~ "ISCED 3",
      edu_level == "10th grade" ~ "ISCED 3",
      edu_level == "9th grade" ~ "ISCED 3",
      edu_level == "8th grade" ~ "ISCED 2",
      edu_level == "7th grade" ~ "ISCED 2",
      edu_level == "6th grade" ~ "ISCED 2",
      edu_level == "5th grade" ~ "ISCED 1",
      edu_level == "4th grade" ~ "ISCED 1",
      edu_level == "3rd grade" ~ "ISCED 1",
      edu_level == "2nd grade" ~ "ISCED 1",
      edu_level == "1st grade" ~ "ISCED 1",
      edu_level == "Some college" ~ "ISCED 5",
      edu_level == "GED or equivalent" ~ "ISCED 3",
      edu_level == "Bachelor's degree (e.g., BA)" ~ "ISCED 6",
      edu_level == "Associate degree: Academic" ~ "ISCED 5",
      edu_level == "Associate degree: Occupational" ~ "ISCED 5",
      edu_level == "Master's degree (e.g., MA)" ~ "ISCED 7",
      edu_level == "Doctoral degree (e.g., PhD)" ~ "ISCED 8",
      edu_level == "Professional school degree (e.g., MD)" ~ "ISCED 8",
      edu_level == "Refused to answer" ~ "Refused to answer"))
  return(summ_education_df)
}
# Identify Fitbit participants
identify_fitbit_participant <- function(demog_data, fitbit_participant_list) {
  
  fitbit_participant_df <- demog_data %>%
    mutate(fitbit_participant = case_when(
      participant_id %in% fitbit_participant_list == TRUE ~ "Yes",
      participant_id %in% fitbit_participant_list == FALSE ~ "No"))
  
  return(fitbit_participant_df)
}
# Summarize income levels for parents
summarize_income_cols <- function(demog_data) {
  #' Summarizes income col for parents
  #' @demog_data Demographics data to summarize
  
  summ_income_df <- demog_data %>%
    mutate(fam_income_summ = case_when(
      combined_fam_income == "< $5000" ~ "< $11,999",
      combined_fam_income == "$5000 - $11,999" ~ "< $11,999",
      combined_fam_income == "$12,000 - $15,999" ~ "$12,000 - $24,999",
      combined_fam_income == "$16,000 - $24,999" ~ "$12,000 - $24,999",
      combined_fam_income == "$25,000 - $34,999" ~ "$25,000 - $49,999",
      combined_fam_income == "$35,000 - $49,999" ~ "$25,000 - $49,999",
      combined_fam_income == "$50,000 - $74,999" ~ "$50,000 - $74,999",
      combined_fam_income == "$75,000 - $99,999" ~ "$75,000 - $99,999",
      combined_fam_income == "$100,000 - $199,999" ~ "$100,000 - $199,999",
      combined_fam_income == "> $200,000" ~ "> $200,000",
      combined_fam_income == "Don't know" ~ "Don't know/Refused to answer",
      combined_fam_income == "Refused to answer" ~ "Don't know/Refused to answer")) %>%
    mutate(indiv_income_summ = case_when(
      past_year_income == "< $5000" ~ "< $11,999",
      past_year_income == "$5000 - $11,999" ~ "< $11,999",
      past_year_income == "$12,000 - $15,999" ~ "$12,000 - $24,999",
      past_year_income == "$16,000 - $24,999" ~ "$12,000 - $24,999",
      past_year_income == "$25,000 - $34,999" ~ "$25,000 - $49,999",
      past_year_income == "$35,000 - $49,999" ~ "$25,000 - $49,999",
      past_year_income == "$50,000 - $74,999" ~ "$50,000 - $74,999",
      past_year_income == "$75,000 - $99,999" ~ "$75,000 - $99,999",
      past_year_income == "$100,000 - $199,999" ~ "$100,000 - $199,999",
      past_year_income == "> $200,000" ~ "> $200,000",
      past_year_income == "Don't know" ~ "Don't know/Refused to answer",
      past_year_income == "Refused to answer" ~ "Don't know/Refused to answer"
    )) %>%
    select(-past_year_income, -combined_fam_income)
  return(summ_income_df)
}
# Summarize income levels for parents
summarize_employment_col <- function(demog_data) {
  #' Summarizes income col for parents
  #' @demog_data Demographics data to summarize
  
  summ_income_df <- demog_data %>%
    mutate(employment_summ = case_when(
      employment_status == "Other" ~ "Other/Refused to answer",
      employment_status == "Refused to answer" ~ "Other/Refused to answer",
      employment_status == "Disabled: permanently or temporarily" ~ employment_status,
      employment_status == "Unemployed, not looking for work" ~ employment_status,
      employment_status == "Student" ~ employment_status,
      employment_status == "Maternity leave" ~ "Working now, on leave",
      employment_status == "Sick leave" ~ "Working now, on leave",
      employment_status == "Temporarily laid off" ~ employment_status,
      employment_status == "Looking for work" ~ employment_status,
      employment_status == "Stay at home parent" ~ employment_status,
      employment_status == "Retired" ~ employment_status,
      employment_status == "Working now, FULL TIME/PART TIME" ~ employment_status)
    ) 
  return(summ_income_df)
}
# Format for demographic comparative analysis
format_demog_data <- function(demog_data, demog_type, fitbit_participant_list,
                                        filter_for_fitbit = FALSE) {
  #' Formats demographic data for comparative analysis
  #' @demog_data Data to be formatted
  #' @demog_type (string) The type of demographics; can only be parent or child
  #' @fitbit_participant_list (chr vector) Vector of participants who had fitbit data
  
  # Summarize ethnicity, birth region
  formatted_demog_data <- demog_data %>%
    summarize_ethnicity_col() %>%
    summarize_birthRegion_cols() %>%
    # Create fitbit_participant column
    identify_fitbit_participant(fitbit_participant_list) %>%
    # Create Yes/No values for hispanic ethnicity
    mutate(hispanic_ethnicity = case_when(
      hispanic_ethnicity == "Don't know" ~ "Don't know",
      !is.na(hispanic_ethnicity) ~ "Yes",
      is.na(hispanic_ethnicity) ~ "No")) %>%
    mutate(ethnicity = case_when(
      hispanic_ethnicity == "Yes" ~ "Hispanic",
      hispanic_ethnicity == "No" ~ ethnicity,
      hispanic_ethnicity == "Don't know" ~ ethnicity)) %>%
    summarize_gender_col()
  
  if (demog_type == "parent") {
    formatted_demog_data %<>%
      summarize_education_cols() %>%
      summarize_income_cols() %>%
      summarize_employment_col() %>%
      select(-contains("birth_region"), -contains("bio"),
             -partner_adoptive_parent, -partner_employment) %>%
      mutate(ppl_in_household = replace_na(ppl_in_household, 999)) %>%
      select(-ppl_in_household_refuse) %>%
      column_to_rownames(var = "participant_id") 
  } else if (demog_type == "child") {
    formatted_demog_data %<>%
      select(-interview_date) %>%
      select(-birth_region, -religion_pref)
  }
  
  if (filter_for_fitbit == "Yes") {
    formatted_demog_data %<>%
      filter(fitbit_participant == "Yes")
  } else if (filter_for_fitbit == "No") {
    formatted_demog_data %<>%
      filter(fitbit_participant == "No")
  } else if (filter_for_fitbit == "NA") {
    return(formatted_demog_data)
  }
  return(formatted_demog_data)
}
# Create income bracket column
divide_income_bracket <- function(parent_demog_data) {
  #' Labels parental income based on poverty line/median income 
  #' @parent_demog_data
  # Based on US 2018 poverty guidelines
  parent_ses_df <- parent_demog_data %>%
    select(participant_id, ethnicity, education, employment_status, combined_fam_income, 
           marital_status, partner, ppl_in_household) %>%
    mutate(income_bracket = case_when(
      (combined_fam_income == "< $5000" & is.numeric(ppl_in_household)) ~ "Below poverty line",
      (combined_fam_income == "$5000 - $11,999" & ppl_in_household == 1) ~ "Below poverty line",
      (combined_fam_income == "$5000 - $11,999" & ppl_in_household > 1) ~ "Below poverty line",
      (combined_fam_income == "$12,000 - $15,999" & ppl_in_household < 2) ~ "Low income",
      (combined_fam_income == "$12,000 - $15,999" & ppl_in_household >= 2) ~ "Below poverty line",
      (combined_fam_income == "$16,000 - $24,999" & ppl_in_household < 4) ~ "Low income",
      (combined_fam_income == "$16,000 - $24,999" & ppl_in_household >= 4) ~ "Below poverty line",
      (combined_fam_income == "$25,000 - $34,999" & ppl_in_household < 6) ~ "Low income",
      (combined_fam_income == "$25,000 - $34,999" & ppl_in_household >= 6) ~ "Below poverty line",
      (combined_fam_income == "$35,000 - $49,999" & ppl_in_household < 8) ~ "Low income",
      (combined_fam_income == "$35,000 - $49,999" & ppl_in_household >= 8) ~ "Below poverty line",
      (combined_fam_income == "$50,000 - $74,999") ~ "Median income",
      (combined_fam_income == "$75,000 - $99,999") ~ "Above median income",
      (combined_fam_income == "$100,000 - $199,999") ~ "Top 35 percentile",
      (combined_fam_income == "> $200,000") ~ "Top 10% percentile",
      (combined_fam_income == "Refused to answer") ~ "Unknown",
      (combined_fam_income == "Don't know") ~ "Unknown"
    )) %>%
    rename(parent_education = education, parent_employment_status = employment_status) %>%
    select(participant_id, parent_education, marital_status, parent_employment_status, 
           combined_fam_income, income_bracket)
  return(parent_ses_df)
}

## Create qa/qc feature-based differential demographic analysis ----
create_qa_qc_demogs <- function(cohort_type, cohort_demog_data, 
                                cohort_demog_variables, 
                                qa_qc_feature_data, qa_qc_feature) {
  
  qa_qc_feature_data %<>%
    rename(participant_id = pID)
  
  demog_data <- cohort_demog_data %>%
    inner_join(qa_qc_feature_data, by = "participant_id")
  
  demog_tableOne <- CreateTableOne(vars = cohort_demog_variables,
                                   strata = "cluster",
                                   data = demog_data)
  
  demog_tableOne <- print(demog_tableOne, quote = F,
                          noSpaces = T, printToggle = F)
  
  cat("Creating TableOne for:", qa_qc_feature,"\n")
  write.csv(demog_tableOne, file = paste0('analysis/results/',qa_qc_feature,
                                          '_',cohort_type,'_demogs.csv'))
  
}

## Determine enrolment in study -----
get_timepoint_participant_list <- function(data) {
  
  df <- data %>%
    group_by(participant_id, eventname) %>% summarize(count = n()) %>%
    select(participant_id, eventname)
  
  # Gets any participant who has participated in the baseline
  baseline <- df %>%
    filter(eventname == "baseline_year_1_arm_1") %>%
    distinct(participant_id, .keep_all = TRUE)
  # Filters out any participant who participated in baseline
  year_2_only <- df %>%
    filter(!participant_id %in% baseline$participant_id) %>%
    distinct(participant_id, .keep_all = TRUE)
  # Gets all participants in the 2-year follow up and 
  # filters out people who participated only in year 2
  overlap <- df %>%
    filter(eventname == "2_year_follow_up_y_arm_1") %>%
    filter(!participant_id %in% year_2_only$participant_id) %>%
    distinct(participant_id, .keep_all = TRUE)
  overall_distinct <- bind_rows(
    baseline, year_2_only
  )
  
  participant_list = list(
    baseline = baseline,
    year_2_only = year_2_only,
    overlap = overlap,
    overall_distinct = overall_distinct
  )
  
  return(participant_list)
}
determine_enrolment <- function(cohort_count_data) {
  
  # Function to create column detailing enrolment
  create_enrolment_col <- function(data, activity_type) {
    
    ## Make columns general for creating enrolment column
    # Select sleep-specific data
    if (activity_type == "sleep") {
      df <- data %<>%
        select(participant_id:total_days_sleep) %>%
        rename_with(~str_replace_all(., "_sleep", ""))
      
      # Select physical-specific data
    } else if (activity_type == "physical") {
      df <- data %<>%
        select(participant_id, followup_count_physical,
               baseline_count_physical, total_days_physical) %>%
        rename_with(~str_replace_all(., "_physical", ""))
      
    }
    
    # Create enrolment column
    df %<>%
      mutate(enrolment = case_when(
        # If no values for follow-up AND contains values for baseline:
        (is.na(followup_count) & !is.na(baseline_count)) ~ "Baseline",
        # If contains values for follow-up AND contains values for baseline:
        (!is.na(followup_count) & !is.na(baseline_count)) ~ "Baseline & Year 2",
        # If values for both follow-up AND values for baseline:
        (!is.na(followup_count) & is.na(baseline_count)) ~ "Year 2",
        # If no values for either follow-up AND baseline:
        (is.na(followup_count) & is.na(baseline_count)) ~ "No enrolment"
      ))
    
    # Rename enrolment column dependent on activity type
    if (activity_type == "sleep") {
      df %<>%
        rename(enrolment_sleep = enrolment)
    } else if (activity_type == "physical") {
      df %<>%
        rename(enrolment_physical = enrolment)
    }
    
    
    return(df)
  }
  
  # Create sleep enrolment column
  sleep_df <- cohort_count_data %>%
    create_enrolment_col("sleep")
  # Create physical enrolment column
  physical_df <- cohort_count_data %>%
    create_enrolment_col("physical")
  
  # Create joined column and give suffixes
  count_df <- full_join(sleep_df, physical_df,
                        by = "participant_id",
                        suffix = c("_sleep", "_physical"))
  
  return(count_df)
}

## Determine count
# Function to create total # of days of activity per phase, per activity type
# per participant
create_phaseSpecific_count <- function(fitbit_daily_sleep_data,
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
    
    # Create total # of recorded activity across phases
    # per activity type
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

## Other metadata-cleaning functions ----

# Map site ID to geographical location
map_site_id <- function(site_data) {
  site_data %<>%
    rename(participant_id = src_subject_id, site_id = site_id_l) %>%
    mutate(site_location = case_when(
      site_id == "site01" ~ "CHLA", 
      site_id == "site02" ~ "CUB",
      site_id == "site03" ~ "FIU",
      site_id == "site04" ~ "LIBR",
      site_id == "site05" ~ "MUSC",
      site_id == "site06" ~ "OHSU",
      site_id == "site07" ~ "ROC",
      site_id == "site08" ~ "SRI",
      site_id == "site09" ~ "UCLA",
      site_id == "site10" ~ "UCSD",
      site_id == "site11" ~ "UFL",
      site_id == "site12" ~ "UMB",
      site_id == "site13" ~ "UMICH",
      site_id == "site14" ~ "UMN",
      site_id == "site15" ~ "UPMC",
      site_id == "site16" ~ "UTAH",
      site_id == "site17" ~ "UVM",
      site_id == "site18" ~ "UWM",
      site_id == "site19" ~ "VCU",
      site_id == "site20" ~ "WUSTL",
      site_id == "site21" ~ "YALE",
      site_id == "site22" ~ "MSSM"
    )) %>%
    mutate(site_location_city = case_when(
      site_id == "site01" ~ "Los Angeles, California",
      site_id == "site02" ~ "Boulder, Colorado",
      site_id == "site03" ~ "Miami, Florida",
      site_id == "site04" ~ "Tulsa, Oklahoma",
      site_id == "site05" ~ "Charleston, South Carolina",
      site_id == "site06" ~ "Portland, Oregon",
      site_id == "site07" ~ "Rochester, New York",
      site_id == "site08" ~ "Menlo Park, California",
      site_id == "site09" ~ "Los Angeles, California",
      site_id == "site10" ~ "San Diego, California",
      site_id == "site11" ~ "Gainesville, Florida",
      site_id == "site12" ~ "Baltimore, Maryland",
      site_id == "site13" ~ "Ann Arbor, Michigan",
      site_id == "site14" ~ "Minneapolis, Minnesota",
      site_id == "site15" ~ "Pittsburgh, Pennsylvania",
      site_id == "site16" ~ "Salt Lake City, Utah",
      site_id == "site17" ~ "Burlington, Vermont",
      site_id == "site18" ~ "Wauwatosa, Wisconsin",
      site_id == "site19" ~ "Richmond, Virginia",
      site_id == "site20" ~ "St. Louis, Missouri",
      site_id == "site21" ~ "New Haven, Conneticut",
      site_id == "site22" ~ "New York, NY (?)"
    ))
  return(site_data)
}

### BMI ----

get_bmi <- function(cohort_demog_data, anthro_data) {
  
  anthro_df <- anthro_data %>%
    rename(participant_id = src_subject_id) %>%
    mutate(interview_age = round(interview_age/12))
  df <- cohort_demog_data %>%
    select(participant_id, sex_at_birth) %>%
    inner_join(anthro_df, by = "participant_id") %>%
    select(participant_id, eventname, interview_age, sex_at_birth, anthroheightcalc, 
           anthroweight1lb:anthroweight3lb, anthro_weight1_hybrid_lb,
           anthro_weight2_hybrid_lb, anthro_weight3_hybrid_lb) %>%
    filter(eventname %in% c("2_year_follow_up_y_arm_1", "baseline_year_1_arm_1"))%>%
    mutate(anthroweight = case_when(
      !is.na(anthroweight3lb) ~ anthroweight3lb,
      is.na(anthroweight3lb) ~ (anthroweight1lb + anthroweight2lb)/2)) %>%
    mutate(anthroweight_hybrid = case_when(
      !is.na(anthro_weight3_hybrid_lb) ~ anthro_weight3_hybrid_lb,
      is.na(anthro_weight3_hybrid_lb) ~ (anthro_weight1_hybrid_lb + anthro_weight2_hybrid_lb)/2)) %>%
    mutate(anthro_weight = case_when(
      !is.na(anthroweight) ~ anthroweight,
      is.na(anthroweight) ~ anthroweight_hybrid)) %>%
    # Convert lb -> kg, inch -> meter
    mutate(anthro_weight = anthro_weight * 0.45359237, anthroheightcalc = anthroheightcalc * 0.0254) %>%
    mutate(BMI = anthro_weight/(anthroheightcalc)^2) %>%
    rename(weight_kg = anthro_weight, height_m = anthroheightcalc) %>%
    select(participant_id, eventname, interview_age, sex_at_birth, height_m, weight_kg, BMI) 
  return(df)
}

### Area Deprivation ----

merge_addr_cols <- function(res_history_data, addr_col) {
  
  addr_col <- enquo(addr_col)
  addr_col <- as_label(addr_col)
  
  baseline <- paste0(addr_col, "_baseline_year_1_arm_1")
  year_1 <- paste0(addr_col, "_1_year_follow_up_y_arm_1")
  year_2 <- paste0(addr_col, "_2_year_follow_up_y_arm_1")
  
  df <- res_history_data %>%
    mutate(!!addr_col := case_when(
      !is.na(.data[[!!baseline]]) & is.na(.data[[!!year_1]]) ~ .data[[!!baseline]],
      .data[[!!baseline]] != .data[[!!year_1]] ~ .data[[!!baseline]],
      .data[[!!baseline]] == .data[[!!year_1]] ~ .data[[!!year_1]],
      !is.na(.data[[!!year_1]]) & is.na(.data[[!!baseline]]) ~ .data[[!!year_1]],
      !is.na(.data[[!!year_1]]) & is.na(.data[[!!year_2]]) ~ .data[[!!year_1]],
      .data[[!!baseline]] != .data[[!!year_2]] ~ .data[[!!year_1]],
      .data[[!!year_1]] == .data[[!!year_2]] ~ .data[[!!year_2]],
      !is.na(.data[[!!year_2]]) ~ .data[[!!year_2]])
    )
  return(df)
}

get_most_recent_adi <- function(demog_data, res_history_data) {
  
  mostRecentADI <- res_history_data %>%
    rename(participant_id = src_subject_id) %>%
    select(where(~!all(is.na(.x)))) %>%
    rename_with(~str_replace(., "reshist_", "")) %>%
    rename_with(~str_replace(., "_adi_perc", "")) %>% 
    # Select for ADI perc rather than ADI wsum
    select(participant_id, eventname, addr1:addr3, addr1_years:addr3_years) %>%
    filter(!is.na((addr1)) | !is.na(addr2) | !is.na(addr3)) %>%
    filter(eventname != "3_year_follow_up_y_arm_1") %>%
    pivot_wider(names_from = "eventname", values_from = addr1:addr3) %>%
    select(participant_id, addr1_baseline_year_1_arm_1, addr1_1_year_follow_up_y_arm_1, addr1_2_year_follow_up_y_arm_1,
           addr2_baseline_year_1_arm_1, addr2_1_year_follow_up_y_arm_1, addr2_2_year_follow_up_y_arm_1,
           addr3_baseline_year_1_arm_1, addr3_1_year_follow_up_y_arm_1, addr3_2_year_follow_up_y_arm_1) %>%
    merge_addr_cols(addr1) %>% merge_addr_cols(addr2) %>% merge_addr_cols(addr3) %>%
    select(participant_id, addr1, addr2, addr3) %>%
    mutate(most_recent_adi = case_when(
      (is.na(addr3) & is.na(addr2)) ~ addr1,
      (is.na(addr3) & !is.na(addr2)) ~ addr2,
      !is.na(addr3) ~ addr3
    ))
  
  df <- demog_data %>%
    left_join(mostRecentADI, by = "participant_id") %>%
    select(participant_id, addr1:most_recent_adi)
  
  return(df)
}
