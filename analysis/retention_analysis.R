# Retention Analysis ##########################################################
# Author: Ethan H. Kim
# Performs retention (survival) analysis with regards to last date if available 
# Fitbit data.

## Necessary libraries -----
require(tidyverse)
require(magrittr)
require(data.table)
require(survival)
require(survminer)
require(GGally)
require(ggsci)

## Source data and processing scripts -----
source("analysis/utils/load_demog_data.R")
source("analysis/utils/load_raw_fitbit_data.R")

## Functions ----
join_quantile_tables <- function(km_fit) {
  
  # Get 75% survival
  median_75_tables <- quantile(km_fit, probs = c(0.25))
  
  # Survival probability
  surv_prob_75 <- median_75_tables$quantile
  # LCL and UCL
  surv_prob_75_LCL <- median_75_tables$lower %>%
    as.data.frame() %>%
    rename("LCL" = "25") %>%
    rownames_to_column(var = "strata")
  surv_prob_75_UCL <- median_75_tables$upper %>%
    as.data.frame() %>%
    rename("UCL" = "25") %>%
    rownames_to_column(var = "strata")
  
  # Join tables
  surv_prob_table <- surv_prob_75 %>%
    as.data.frame() %>%
    rownames_to_column(var = "strata") %>%
    rename("median_75" = "25") %>%
    left_join(surv_prob_75_LCL, by = "strata") %>%
    left_join(surv_prob_75_UCL, by = "strata")
  
  return(surv_prob_table)
}

# Plot Kaplan-Meier curve at 75% survival
plot_km_curve_75 <- function(km_fit, strata_names) {
    
    # Create 75% median survival table
    surv_75_table <- join_quantile_tables(km_fit) %>%
      select(median_75) %>%
      rename(x = median_75) %>% mutate(x2 = x) %>%
      mutate(y = rep(0, length(x)), y2 = 0.75)
    # Remove anything before '=' in legend
    names(km_fit$strata) <- gsub(".*=", "", names(km_fit$strata))
    print(names(km_fit$strata))
    km_plot <- ggsurvplot(fit = km_fit, xlim = c(0, 21), break.x.by = 2,
                          conf.int = TRUE, palette = "jama", legend = c(0.3, 0.2),
                          pval = TRUE, pval.coord = c(0.6, 0.5),
                          font.x = c(16), font.y = c(16))
    plot <- km_plot$plot +
      # horizontal segment
      geom_segment(aes(x = 0, y = 0.75, xend = max(x, na.rm = TRUE), yend = 0.75), 
                   data = surv_75_table, linetype = "dashed", size = 0.5) +
      # vertical segment
      geom_segment(aes(x = x, y = y, xend = x2, yend = y2), 
                   data = surv_75_table, linetype = "dashed", size = 0.5) +
      # change legend
      theme(axis.text.x = element_text(size = 16, hjust = 0.5, vjust = 0.8),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_text(size = 18), 
            axis.title.x = element_text(size = 18),
            axis.line.y.left = element_line(size = 0.5, color = "black"),
            axis.line.x.bottom = element_line(size = 0.5, color = "black"),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.margin = margin(t = 10, r = 30, b = 10, l = 10)) +
      # Edit axis texts
      labs(x = "\nDays", y = "Retention probability\n")
    
    return(plot)
}

# Perform survival analysis
perform_survival_analysis <- function(demog_var, data, sensitivity = FALSE) {
  # Set demog var
  var <- enquo(demog_var)
  
  if (sensitivity == TRUE) {
    # KM fit with demog_var, without strata
    km_fit <- surv_fit(
      stats::update.formula(var, Surv(time = last_wear_date_sensitivity, 
                                      event = status_sensitivity, type = "right") ~ .), 
      data = data)
    km_stats <- survdiff(
      stats::update.formula(var, Surv(time = last_wear_date_sensitivity, 
                                      event = status_sensitivity, type = "right") ~ .), 
      data = data)
    # KM fit with demog_var, with strata
    km_strata_fit <- surv_fit(
      stats::update.formula(var, Surv(time = last_wear_date_sensitivity, 
                                      event = status_sensitivity, type = "right") ~ . + 
                                      strata(site_location, na.group = TRUE)), data = data)
    km_strata_stats <- survdiff(
      stats::update.formula(var, Surv(time = last_wear_date_sensitivity, 
                                      event = status_sensitivity, type = "right") ~ .), 
      data = data)
  } else if (sensitivity == FALSE) {
    # KM fit with demog_var, without strata
    km_fit <- surv_fit(
      stats::update.formula(var, Surv(time = last_wear_date, 
                                      event = status) ~ .), data = data)
    km_stats <- survdiff(
      stats::update.formula(var, Surv(time = last_wear_date, 
                                      event = status) ~ .), data = data)
    # KM fit with demog_var, with strata
    km_strata_fit <- surv_fit(
      stats::update.formula(var, Surv(time = last_wear_date, 
                                      event = status) ~ . + 
                                      strata(site_location, na.group = TRUE)), data = data)
    km_strata_stats <- survdiff(
      stats::update.formula(var, Surv(time = last_wear_date, 
                                      event = status) ~ . + 
                              strata(site_location, na.group = TRUE)), data = data)
  }
  
  # Summarize statistics of stratified KM fit
  summary <- summary(km_strata_fit)
  table <- as.data.frame(summary$table) %>%
    rownames_to_column(var = "strata") %>%
    rename(LCL = "0.95LCL", UCL = "0.95UCL", N = "records") %>%
    mutate(CI = paste0(LCL, "-", UCL)) %>%
    select(strata, N, median, CI) %>%
    mutate(strata = gsub(",.*=", "|", strata, perl = TRUE))
  
  ## Getting median survival of 75%
  table_75 <- join_quantile_tables(km_fit)
  table_75_strata <- join_quantile_tables(km_strata_fit) %>%
    mutate(LCL = as.character(LCL), UCL = as.character(UCL),
           CI = paste0(LCL, "-", UCL)) %>%
    mutate(strata = gsub(",.*=", "|", strata, perl = TRUE)) %>%
    left_join(table %>% select(strata, N), by = "strata") %>%
    select(strata, N, median_75, CI)
  # Plot KM fit without strata
  demog_var_labels <- data %>% select({{demog_var}}) %>% 
    filter(!is.na({{demog_var}})) %>%
    distinct() %>% pull()
  km_75_plot <- plot_km_curve_75(km_fit, demog_var_labels)
    
  # Create list to return
  km_analysis <- list(
    fit_strata = km_strata_fit,
    fit_strata_summary = summary,
    fit_strata_table = table,
    fit_NA_strata = km_fit,
    fit_plot_75 = km_75_plot,
    fit_NA_table_75 = table_75,
    fit_strata_table_75 = table_75_strata,
    fit_stats = km_stats,
    fit_strata_stats = km_strata_stats)
  
  return(km_analysis)
}

print_plot <- function(plot, file_dir, file_type) {
  
  if (!(file_type %in% c(".pdf", ".jpeg", ".png"))) {
    stop("Please choose either .pdf, .jpeg or .png for file_type.")
  } else {
    file_name <- paste0(file_dir, file_type)
    print(file_name)
    if (file_type == ".pdf") {
      pdf(file_name, onefile = FALSE)
      print(plot)
      dev.off()
    } else if (file_type == ".jpeg") {
      jpeg(file_name, res = 300)
      print(plot)
      dev.off()
    } else if (file_type == ".png") {
      png(file_name, res = 300, width = 7, height = 7, units = "in")
      print(plot)
      dev.off()
    }
  }
}

### Format data -----

## Demographic data ##
# Fitbit cohort
fitbit_cohort_demogs_year_2 %<>%
  mutate(fitbit_avail = "Yes")

# ABCD Raw Fitbit-based With-Fitbit demographics (n = 6,611)
raw_fitbit_cohort_demogs <- abcd_cohort_demogs_year_2 %>%
  filter(participant_id %in% hr_daily_weartime$participant_id) %>%
  mutate(fitbit_avail = "Raw")

# Merge raw + summary Fitbit demographics
merged_fitbit_demogs <- raw_fitbit_cohort_demogs %>%
  select(participant_id, fitbit_avail) %>%
  full_join(fitbit_cohort_demogs_year_2, by = "participant_id", suffix = c("_raw", "_summ")) %>%
  mutate(fitbit_avail = case_when(fitbit_avail_raw == "Raw" ~ "Summary + raw",
                                  is.na(fitbit_avail_raw) ~ "Summary only")) %>%
  select(-fitbit_avail_raw, -fitbit_avail_summ)

# Merge the fitbit_avail column into main ABCD cohort demographics for comparison
abcd_cohort_fitbit_availability <- abcd_cohort_demogs_year_2 %>%
  left_join(merged_fitbit_demogs %>% select(participant_id, fitbit_avail), 
            by = "participant_id", suffix = c("_x", "_y")) %>%
  mutate(fitbit_avail = case_when(is.na(fitbit_avail) ~ fitbit_participant,
                                  fitbit_avail == "Summary + raw" ~ fitbit_avail,
                                  fitbit_avail == "Summary only" ~ fitbit_avail)) %>%
  mutate(fitbit_avail = case_when(fitbit_avail == "No" ~ "No data",
                                  TRUE ~ as.character(fitbit_avail))) %>%
  select(-fitbit_participant)

fitbit_participant_df <- abcd_cohort_fitbit_availability %>%
  select(participant_id, fitbit_avail)

# Create the first wear date for participants with raw Fitbit data
# from their Daily Summary data
fitbit_wear_date_df <- fitbit_datasets$fitbitDailyPhys_4_0 %>%
  select(participant_id, wear_date, eventname) %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  full_join((fitbit_datasets$fitbitDailySleep_4_0 %>%
              filter(eventname == "2_year_follow_up_y_arm_1") %>%
              select(participant_id, sleepdate)), 
            by = "participant_id") %>%
  rename(sleep_wear_date = sleepdate, phys_wear_date = wear_date) %>%
  mutate(wear_date = case_when(is.na(sleep_wear_date) ~ phys_wear_date,
                               is.na(phys_wear_date) ~ sleep_wear_date,
                               !is.na(phys_wear_date) ~ phys_wear_date)) %>%
  group_by(participant_id, wear_date) %>%
  distinct(participant_id, .keep_all = TRUE) %>%
  select(-sleep_wear_date, phys_wear_date) %>%
  inner_join(fitbit_participant_df, by = "participant_id") %>%
  # Filter for 6,541 raw Fitbit data participants
  filter(participant_id %in% hr_total_weartime$participant_id) %>%
  select(participant_id, wear_date, eventname, fitbit_avail)

## Remove intermediaries
rm(fitbit_participant_df, merged_fitbit_demogs, raw_fitbit_cohort_demogs,
   abcd_cohort_demogs_year_2, fitbit_datasets, fitbit_cohort_IDs, hr_daily_weartime,
   hr_total_weartime)

# Create dataframe for retention / survival analysis
participant_data <- list()
for (p in unique(fitbit_wear_date_df$participant_id)) {
  participant_data[[p]] <- create_start_date(fitbit_wear_date_df, p, 21)
}
wear_date_df <- bind_rows(participant_data)
write.csv(wear_date_df, "data/intermediates/wear_date_df.csv", row.names = F)

# Create dataframe for sensitivity analysis
participant_data <- list()
for (p in unique(fitbit_wear_date_df$participant_id)) {
  participant_data[[p]] <- create_start_date(fitbit_wear_date_df, p, 35)
}
wear_date_sensitivity_df <- bind_rows(participant_data)
write.csv(wear_date_sensitivity_df, "data/intermediates/wear_date_sensitivity_df.csv", 
          row.names = F)

## Create initial retention dataframe (pre-sensitivity)
retention_df <- wear_date_df %>%
  group_by(participant_id) %>%
  # Filter for the last date of Fitbit data available
  filter(row_number() == n()) %>%
  rename(last_wear_date = protocol_date) %>%
  ungroup() %>%
  select(participant_id, last_wear_date)
## Create dataframe for sensitivity analysis post-21 days
retention_sensitivity_df <- wear_date_sensitivity_df %>%
  group_by(participant_id) %>%
  filter(row_number() == n()) %>%
  rename(last_wear_date_sensitivity = protocol_date) %>%
  ungroup() %>%
  select(participant_id, last_wear_date_sensitivity)

## Join retention, retention_sensitivity to cohort/parental demographics
retention_analysis_df <- fitbit_cohort_demogs_year_2 %>%
  filter(participant_id %in% wear_date_df$participant_id) %>%
  left_join(retention_df, by = "participant_id") %>%
  left_join(retention_sensitivity_df, by = "participant_id") %>%
  # Join parental sociodemographics
  inner_join(fitbit_parent_demogs_year_2, by = "participant_id", 
             suffix = c("_child", "_parent")) %>%
  # Create "event" for survival: 1 = dead, 0 = alive
  mutate(status = ifelse(last_wear_date == 21, 0, 1)) %>%
  # If last_wear_date is under 21, but sensitivity last date is over 21, then
  # consider "alive"
  mutate(status_sensitivity = case_when(last_wear_date == 21 ~ 0,
                                        ((last_wear_date < 21) & 
                                           (last_wear_date_sensitivity > 21)) ~ 0,
                                        last_wear_date < 21 ~ 1)) %>%
  select(participant_id:region, education_parent:marital_status,
         last_wear_date, last_wear_date_sensitivity, status, status_sensitivity) %>%
  ## Modify demographics for survival analysis
  mutate(marital_status = 
           case_when(marital_status == "Refused to answer" ~ as.character(NA),
                     marital_status == "Separated" ~ "Divorced/Separated/Widowed",
                     marital_status == "Divorced" ~ "Divorced/Separated/Widowed",
                     marital_status == "Widowed" ~ "Divorced/Separated/Widowed",
                     TRUE ~ as.character(marital_status))) %>%
  mutate(family_income = 
           case_when(family_income == "< $11,999" ~ "< $25,000",
                     family_income == "$12,000 - $24,999" ~ "< $25,000",
                     family_income == "$100,000 - $199,999" ~ "> $100,000",
                     family_income == "> $200,000" ~ "> $100,000",
                     family_income == "Don't know" ~ "Don't know/Refused to answer",
                     family_income == "Refused to answer" ~ "Don't know/Refused to answer",
                     TRUE ~ as.character(family_income))) %>%
  mutate(family_income = factor(family_income,
                                levels = c("< $25,000", "$25,000 - $49,999",
                                           "$50,000 - $74,999", "$75,000 - $99,999",
                                           "> $100,000", "Don't know/Refused to answer"))) %>%
  mutate(employment_parent = 
           case_when(employment_parent == "Working now, FULL TIME/PART TIME" ~ "Working now",
                     employment_parent == "Working now, on leave" ~ "Working now",
                     employment_parent == "Temporarily laid off" ~ "Laid off/Unemployed/Retired",
                     employment_parent == "Unemployed, not looking for work" ~ "Laid off/Unemployed/Retired",
                     employment_parent == "Retired" ~ "Laid off/Unemployed/Retired",
                     employment_parent == "Disabled: permanently or temporarily" ~ "Disabled",
                     TRUE ~ as.character(employment_parent))) %>%
  mutate(education_parent = 
           case_when(education_parent == "ISCED 1" ~ "ISCED 1-3",
                     education_parent == "ISCED 2" ~ "ISCED 1-3",
                     education_parent == "ISCED 3" ~ "ISCED 1-3",
                     education_parent == "Refused to answer" ~ as.character(NA),
                     TRUE ~ as.character(education_parent))) %>%
  mutate(gender = 
           case_when(gender == "Don't know/Refused to answer" ~ as.character(NA),
                     gender == "Different/Gender Queer/Trans" ~ as.character(NA),
                     TRUE ~ as.character(gender)))
  
write.csv(retention_analysis_df,
          "data/intermediates/retention_analysis_df.csv", row.names = F)

## Survival Analysis (Univariate Kaplan-Meier) ----
### Establish socio-demographic variables
sociodemog_vars <- list(
  race_ethnicity = "race_ethnicity", weight_category = "weight_category", 
  gender = "gender", education_parent = "education_parent",
  employment_parent = "employment_parent", marital_status = "marital_status",
  family_income = "family_income")

## Survival analysis - 21 days
km_analysis <- lapply(sociodemog_vars, function(x) 
  perform_survival_analysis(!!sym(x), retention_analysis_df, sensitivity = FALSE))
## Survival analaysis - 35 days, sensitivity
km_analysis_survival <- lapply(sociodemog_vars, function(x) 
  perform_survival_analysis(!!sym(x), retention_analysis_df, sensitivity = TRUE))

## BUG:
# ggsurvfit does not translate factors identically for whatever reason
# so strata will not be ordered the right way
km_analysis$family_income$fit_plot_75
test <- km_analysis$family_income$fit_plot_75$data$strata
test <- factor(test, levels = c("< $25,000", "$25,000 - $49,999",
                                "$50,000 - $75,999", "$75,000 - $99,999",
                                "> $100,000", "Don't know/Refused to answer"))


## Save survival analysis plots
for (i in 1:length(km_analysis)) {
  demog_var <- names(km_analysis[i])
  # Set paths
  file_path <- paste0("plots/survivalAnalysis/surv_75_", demog_var)
  file_path_sensitivity = paste0("plots/survivalAnalysis/surv_75_", demog_var, "_sensitivity")
  # Print paths
  print_plot(km_analysis[[i]]$fit_plot_75, file_path, ".png")
  print_plot(km_analysis_survival[[i]]$fit_plot_75, file_path_sensitivity, ".png")
}

## Save survival analysis tables
surv_table_non_strata <- lapply(sociodemog_vars,
                                function(x) km_analysis[[x]]$fit_NA_table_75)
surv_table_list <- lapply(sociodemog_vars, 
                          function(x) km_analysis[[x]]$fit_strata_table_75)
surv_table_sensitivity_list <- lapply(sociodemog_vars, 
                          function(x) km_analysis_survival[[x]]$fit_strata_table_75)

surv_table_df <- bind_rows(surv_table_list)
surv_table_sensitivity_df <- bind_rows(surv_table_sensitivity_list)
surv_table_non_strata_df <- bind_rows(surv_table_non_strata)
write.csv(surv_table_non_strata_df,
          "results/retention_analysis/ABCD_Study_Retention_nonStrata.csv",
          row.names = F)
write.csv(surv_table_df, 
          "results/retention_analysis/ABCD_Study_Retention_Logrank_Strata.csv", 
          row.names = FALSE)
write.csv(surv_table_sensitivity_df, 
          "results/retention_analysis/ABCD_Study_Retention_Logrank_Strata_Sensitivity.csv",
          row.names = FALSE)

## Supplementary ----

### Log-rank test results ----
create_log_rank_tbl <- function(km_analysis_obj, var) {
  
  n_col <- km_analysis_obj[[var]]$fit_stats$n
  obs <- km_analysis_obj[[var]]$fit_stats$obs
  exp <- km_analysis_obj[[var]]$fit_stats$exp
  chi_sq <- (obs-exp)^2/exp
  chi_sq_total = sum(chi_sq)
  degree_freedom <- length(km_analysis_obj[[var]]$fit_stats$n) - 1
  
  tbl <- tibble(
    strata = names(n_col),
    n = n_col,
    observed = obs,
    expected = exp,
    chi_sq = chi_sq)
  
  return(tbl)
  
}

log_rank_tbls <- list()
for (i in 1:length(sociodemog_vars)) {
  sociodemog_var <- sociodemog_vars[[i]]
  log_rank_tbl <- create_log_rank_tbl(km_analysis, sociodemog_var)
  log_rank_tbls[[sociodemog_var]] <- log_rank_tbl
}
log_rank_tbls <- bind_rows(log_rank_tbls)

write_csv(log_rank_tbls, "results/retention_analysis/ABCD_Study_LogRankTest_Results.csv")


### Survival Analysis - Pre / During COVID-19 ----

#### Create enrolment df ----
enrolment_df <- fread("data/intermediates/wear_date_df.csv") %>%
  group_by(participant_id) %>%
  arrange(protocol_date) %>%
  filter(row_number() == 1) %>%
  mutate(enrolment_q_yr = lubridate::quarter(wear_date, 
                                             with_year = TRUE)) %>%
  mutate(enrolment_q_yr = 
           case_when(enrolment_q_yr == "2017.2" ~ "Q2 2017", enrolment_q_yr == "2017.3" ~ "Q3 2017",
                     enrolment_q_yr == "2017.4" ~ "Q4 2017", enrolment_q_yr == "2018.1" ~ "Q1 2018",
                     enrolment_q_yr == "2018.1" ~ "Q1 2018", enrolment_q_yr == "2018.3" ~ "Q3 2018",
                     enrolment_q_yr == "2018.4" ~ "Q4 2018", enrolment_q_yr == "2019.1" ~ "Q1 2019",
                     enrolment_q_yr == "2019.2" ~ "Q2 2019", enrolment_q_yr == "2019.3" ~ "Q3 2019",
                     enrolment_q_yr == "2019.4" ~ "Q4 2019", enrolment_q_yr == "2020.1" ~ "Q1 2020",
                     enrolment_q_yr == "2020.2" ~ "Q2 2020", enrolment_q_yr == "2020.3" ~ "Q3 2020",
                     enrolment_q_yr == "2020.4" ~ "Q4 2020")) %>%
  mutate(enrolment_q_yr = factor(enrolment_q_yr,
                                 levels = c("Q2 2017", "Q3 2017", "Q4 2017",
                                            "Q1 2018", "Q2 2018", "Q3 2018",
                                            "Q4 2018", "Q1 2019", "Q2 2019",
                                            "Q3 2019", "Q4 2019", "Q1 2020",
                                            "Q2 2020", "Q3 2020", "Q4 2020"))) %>%
  mutate(covid_19_yr = case_when(str_detect(enrolment_q_yr, "2018") ~ FALSE,
                                 str_detect(enrolment_q_yr, "2019") ~ FALSE,
                                 str_detect(enrolment_q_yr, "Q1 2020") ~ FALSE,
                                 TRUE ~ TRUE)) %>%
  select(participant_id, covid_19_yr)

covid_km_analysis_df <- retention_analysis_df %>%
  left_join(enrolment_df, by = "participant_id")

### Pre-COVID  ----
precovid_km_analysis <- lapply(sociodemog_vars, function(x) 
  perform_survival_analysis(!!sym(x), covid_km_analysis_df %>% filter(covid_19_yr == FALSE), 
                            sensitivity = FALSE))

write_surv_tables <- function(sociodemog_var_list, km_analysis_obj, folder_path,
                              csv_prefix) {
  
  non_strata_tbl <- lapply(sociodemog_var_list,
                           function(x) km_analysis_obj[[x]]$fit_NA_table_75)
  tbl_list <- lapply(sociodemog_var_list,
                     function(x) km_analysis_obj[[x]]$fit_strata_table_75)
  sensitivity_tbl_list <- lapply(sociodemog_vars, 
                                 function(x) km_analysis_obj[[x]]$fit_strata_table_75)
  
  surv_non_strata_df <- bind_rows(non_strata_tbl)
  surv_tbl_df <- bind_rows(tbl_list)
  tbl_sensitivity_df <- bind_rows(sensitivity_tbl_list)
  
  tbls_list <- list(surv_non_strata_df, surv_tbl_df, tbl_sensitivity_df)
  tbls_names <- list('nonStrata', 'Logrank_Strata', 'Logrank_Strata_Sensitivity')
  
  for (i in 1:length(tbls_list)) {
    csv_path <- paste0(folder_path, csv_prefix, "_", tbls_names[[i]], ".csv")
    print(csv_path)
    write.csv(tbls_list[[i]], csv_path, row.names = FALSE)
  }
  results <- tbls_list
  return(results)
}

precovid_km_tables <- write_surv_tables(sociodemog_var_list = sociodemog_vars, 
                          km_analysis_obj = precovid_km_analysis, 
                          folder_path = "results/retention_analysis/",
                          csv_prefix = "ABCD_Study_Retention_PreCOVID")

### During COVID  ----
covid_km_analysis <- lapply(sociodemog_vars, function(x) 
  perform_survival_analysis(!!sym(x), covid_km_analysis_df %>% filter(covid_19_yr == TRUE), 
                            sensitivity = FALSE))


covid_km_tables <- write_surv_tables(sociodemog_var_list = sociodemog_vars, 
                                     km_analysis_obj = covid_km_analysis, 
                                     folder_path = "results/retention_analysis/",
                                     csv_prefix = "ABCD_Study_Retention_COVID")

### Create overall survival table ----
surv_tbls_list <- list(
  overall = surv_table_non_strata_df,
  precovid = precovid_km_tables[[1]],
  covid = covid_km_tables[[1]])

surv_tbls_list_formatted <- list()
for (i in 1:length(surv_tbls_list)) {
  surv_data_range = names(surv_tbls_list)[i]
  surv_tbl <- surv_tbls_list[[i]] %>%
    mutate_if(is.numeric, as.character) %>%
    mutate(median_LCL_UCL = paste0(median_75, " (", LCL, " - ", UCL, ")")) %>%
    select(strata, median_LCL_UCL) %>%
    mutate(data_range = surv_data_range)
  surv_tbls_list_formatted[[surv_data_range]] <- surv_tbl
}

surv_tbl_formatted <- bind_rows(surv_tbls_list_formatted) %>%
  pivot_wider(names_from = data_range, values_from = median_LCL_UCL)

write.csv(surv_tbl_formatted, "results/retention_analysis/ABCD_Study_Diff_Retention.csv",
          row.names = F)
  

## Survival analaysis - 35 days, sensitivity
km_analysis_survival <- lapply(sociodemog_vars, function(x) 
  perform_survival_analysis(!!sym(x), retention_analysis_df, sensitivity = TRUE))
  

## Cox PH Analysis ----

test_cox <- coxph(Surv(last_wear_date, status) ~ race_ethnicity + gender + most_recent_adi + education_parent + 
        family_income, data = retention_analysis_df)
cox.zph(test_cox)
