# Script to format and load demographics-related ABCD Study Data ##############

## Source data loading scripts ----
source('load_utils/load_synapse.R')
source("analysis/utils/format_data.R")
source("analysis/utils/format_demographics.R")

## Necessary libraries ----
require(tidyverse)
require(magrittr)
require(data.table)
require(lubridate)
require(gdata)

## Load in datasets from Synapse ----
demog_datasets <- load_dataset_types("demographics")
fitbit_datasets <- load_dataset_types("fitbit")
site_data <- load_dataset_types("site")
anthro_data <- load_dataset_types("anthro")
residential_data <- load_dataset_types("res_history")

## Demographic data ----

### Cohort BMI ----
cohort_bmi <- get_bmi(demog_datasets$abcdStudyCohortDemogs_4_0, anthro_data) %>%
  rowwise() %>%
  mutate(BMI_zscore = bmi_percentile(BMI, interview_age, sex_at_birth)) %>%
  mutate(BMI_percentile = pnorm(BMI_zscore)) %>%
  mutate(weight_category = case_when(
    BMI_percentile < 0.05 ~ "Underweight",
    BMI_percentile < 0.85 ~ "Healthy",
    BMI_percentile < 0.95 ~ "Overweight",
    BMI_percentile > 0.95 ~ "Obesity",
    is.na(BMI_percentile) ~ "Missing"))
### Cohort ADI ----
cohort_adi <- get_most_recent_adi(
  demog_data = demog_datasets$abcdStudyCohortDemogs_4_0, residential_data) %>%
  distinct(participant_id, .keep_all = TRUE)
### ABCD Study Site regions and ID's ----
regions <- list(
  west = c("CUB", "OHSU", "SRI", "UTAH", "UCSD", "UCLA"),
  midwest = c("WUSTL", "UWM", "UMN", "UMICH"),
  southwest = c("LIBR"),
  southeast = c("VCU", "UVM", "UMB", "UFL", "FIU", "MUSC"),
  northeast = c("YALE", "UPMC", "ROC", "MSSM"))
cohort_site_id <- site_data %>%
  map_site_id() %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  mutate(region = case_when(
    site_location %in% regions$west ~ "West",
    site_location %in% regions$midwest ~ "Midwest",
    site_location %in% regions$southwest ~ "Southwest",
    site_location %in% regions$southeast ~ "Southeast",
    site_location %in% regions$northeast ~ "Northeast")) %>%
  select(participant_id, site_location, region)

### Fitbit participants (had at least 1 day of data) ----
phys_year_2 <- fitbit_datasets$fitbitDailyPhys_4_0 %>%
  group_by(participant_id, eventname) %>% summarize(count = n())
sleep_year_2 <- fitbit_datasets$fitbitDailySleep_4_0 %>%
  group_by(participant_id, eventname) %>% summarize(count = n())
cohort_count <- full_join(phys_year_2, sleep_year_2, by = c("participant_id", "eventname"),
                          suffix = c("_physical", "_sleep"))

year_2_fitbit_participants <- cohort_count %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>% select(participant_id) %>%
  pull()

## DEPRECATED
#cohort_count <- create_timepoint_count(fitbit_datasets$fitbitDailySleep_4_0,
#                                       fitbit_datasets$fitbitDailyPhys_4_0) %>%
#  determine_enrolment() %>%
#  filter(enrolment_sleep != "Baseline" & enrolment_physical != "Baseline")


## Cohort Demographics ----

### Baseline ABCD Cohort Demographics ----
# Overall ABCD Cohort Demographics
abcd_cohort_demogs_baseline <- 
  format_demog_data(demog_data = demog_datasets$abcdStudyCohortDemogs_4_0, 
                    demog_type = "child",
                    fitbit_participant_list = unique(cohort_count$participant_id),
                    filter_for_fitbit = "NA") %>% 
  left_join(cohort_adi, by = "participant_id") %>% select(-contains("addr")) %>%
  select(-hispanic_ethnicity, -sex_at_birth, -sex_assigned_at_birth, 
         -racial_combination, -education_grade, -interview_age) %>%
  left_join(cohort_bmi %>% filter(eventname == "baseline_year_1_arm_1"), 
            by = "participant_id") %>%
  select(-child_age) %>%
  rename(race_ethnicity = ethnicity, gender = gender_summ) %>%
  select(participant_id, interview_age, race_ethnicity, gender, most_recent_adi, 
         BMI, BMI_zscore, BMI_percentile, weight_category, fitbit_participant, eventname) %>%
  left_join(cohort_site_id, by = "participant_id") %>%
  mutate(race_ethnicity = relevel(as.factor(race_ethnicity), ref = "White")) %>%
  mutate(site_location = relevel(as.factor(site_location), ref = "UTAH")) %>%
  mutate(weight_category = relevel(as.factor(weight_category), ref = "Healthy")) %>%
  mutate(gender = relevel(as.factor(gender), ref = "Male"))

### Year 2 ABCD Cohort Demographics ----
abcd_cohort_demogs_year_2 <-
  format_demog_data(demog_data = demog_datasets$abcdStudyCohortDemogs_4_0,
                    demog_type = "child",
                    fitbit_participant_list = year_2_fitbit_participants,
                    filter_for_fitbit = "NA") %>%
  # Filter for 2 year followup timepoint
  filter(participant_id %in% cohort_site_id$participant_id) %>%
  # Add in ADI
  inner_join(cohort_adi, by = "participant_id") %>% select(-contains("addr")) %>%
  mutate(adi_percentile = case_when(
    most_recent_adi < 25 ~ 'quartile_1',
    most_recent_adi < 50 ~ 'quartile_2',
    most_recent_adi < 75 ~ 'quartile_3',
    most_recent_adi < 100 ~ 'quartile_4',
    is.na(most_recent_adi) ~ 'NA'
  )) %>%
  select(-hispanic_ethnicity, -sex_at_birth, -sex_assigned_at_birth, 
         -racial_combination, -education_grade, -interview_age) %>%
  # Add in BMI
  left_join(cohort_bmi %>% filter(eventname == "2_year_follow_up_y_arm_1"), 
            by = "participant_id") %>%
  # Update interview age if missing
  mutate(age = ifelse(is.na(interview_age), child_age + 2, interview_age)) %>%
  select(-interview_age, -child_age) %>%
  # Rename columns
  rename(race_ethnicity = ethnicity, gender = gender_summ) %>%
  select(participant_id, age, race_ethnicity, gender, most_recent_adi, adi_percentile,
         BMI, BMI_zscore, BMI_percentile, weight_category, fitbit_participant, eventname) %>%
  # Add site ID and region
  left_join(cohort_site_id, by = "participant_id") %>%
  # Assign default levels for comparison
  mutate(race_ethnicity = relevel(as.factor(race_ethnicity), ref = "White")) %>%
  mutate(site_location = relevel(as.factor(site_location), ref = "UTAH")) %>%
  mutate(weight_category = relevel(as.factor(weight_category), ref = "Healthy")) %>%
  mutate(gender = relevel(as.factor(gender), ref = "Male")) 

### Year 2 ABCD Cohort Demographics - Fitbit participants
fitbit_cohort_demogs_year_2 <- abcd_cohort_demogs_year_2 %>%
  filter(fitbit_participant == "Yes")

# Get Fitbit cohort participant_ids
fitbit_cohort_IDs <- fitbit_cohort_demogs_year_2 %>% 
  select(participant_id) %>% pull()

## Parental Demographics ----

### Year 2 ABCD Cohort Parent Demographics
abcd_parent_demogs_year_2 <- 
  format_demog_data(demog_data = demog_datasets$abcdStudyParentDemogs_4_0, 
                    demog_type = "parent",
                    fitbit_participant_list = year_2_fitbit_participants,
                    filter_for_fitbit = "NA") %>%
  rownames_to_column(var = "participant_id") %>%
  select(participant_id, age, ethnicity, gender_summ, education_summary, fitbit_participant,
         fam_income_summ, employment_summ, marital_status, -hispanic_ethnicity) %>%
  rename(education_parent = education_summary, family_income = fam_income_summ,
         employment_parent = employment_summ) %>%
  mutate(employment_parent = relevel(as.factor(employment_parent), ref = "Working now, FULL TIME/PART TIME")) %>%
  mutate(marital_status = relevel(as.factor(marital_status), ref = "Married")) %>%
  mutate(family_income = relevel(as.factor(family_income), ref = "$25,000 - $49,999")) %>%
  mutate(education_parent = relevel(as.factor(education_parent), ref = "ISCED 5"))

abcd_parent_demogs_year_2 <- demog_datasets$abcdStudyLongParentDemogs_4_0 %>%
  rename(participant_id = src_subject_id) %>%
  select(participant_id, eventname, age, marital_status, employment_status, combined_fam_income,
         edu_level_non_baseline) %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  rename(edu_level = edu_level_non_baseline) %>%
  mutate(education_parent = case_when(
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
    edu_level == "Refused to answer" ~ "Refused to answer")) %>%
  select(-edu_level) %>%
  # < $11,999 | $12,000 - $24,999 | $25,000 - $49,999 | $50,000 - $74,999 | $75,000 - $99,999 | $100,000 - $199,999 | > $200,000
  mutate(combined_fam_income = 
           case_when(combined_fam_income == "< $5000" ~ "< $11,999",
                     combined_fam_income == "$5000 - $11,999" ~ "< $11,999",
                     combined_fam_income == "$12,000 - $15,999" ~ "$12,000 - $24,999",
                     combined_fam_income == "$16,000 - $24,999" ~ "$12,000 - $24,999",
                     combined_fam_income == "$25,000 - $34,999" ~ "$25,000 - $49,999",
                     combined_fam_income == "$35,000 - $49,999" ~ "$25,000 - $49,999",
                     is.na(combined_fam_income) ~ "Refused to answer",
                     TRUE ~ as.character(combined_fam_income))) %>%
  rename(employment_parent = employment_status, family_income = combined_fam_income) %>%
  right_join(abcd_parent_demogs_year_2 %>% mutate_all(~as.character(.)), 
             by = "participant_id", suffix = c("_year2", "_baseline")) %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  mutate(age_baseline = as.numeric(age_baseline)) %>%
  mutate(education = if_else(is.na(education_parent_year2), education_parent_baseline, education_parent_year2),
         age = if_else(is.na(age_year2), age_baseline + 2, age_year2), 
         marriage = if_else(is.na(marital_status_year2), marital_status_baseline, marital_status_year2),
         employment = if_else(is.na(employment_parent_year2), employment_parent_baseline, employment_parent_year2),
         family_income = if_else(is.na(family_income_year2), family_income_baseline, family_income_year2)) %>%
  select(participant_id, age, ethnicity, gender_summ, education, fitbit_participant, 
         family_income, employment, marriage) %>%
  rename(education_parent = education, employment_parent = employment, marital_status = marriage)

fitbit_parent_demogs_year_2 <- abcd_parent_demogs_year_2 %>%
  filter(participant_id %in% year_2_fitbit_participants)

# Remove unnecessary intermediary datasets
rm(cohort_adi, cohort_bmi, cohort_site_id, demog_datasets,
   regions, residential_data, site_data, anthro_data, cohort_count,
   phys_year_2, sleep_year_2, year_2_fitbit_participants)
