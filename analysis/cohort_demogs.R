# ABCD Study Year 2 Cohort differential demographic analysis ##################
# Author: Ethan H. Kim
# Performs cohort-level demographic analysis to produce Table 1. 

## Source data loading / formatting scripts ----
source('load_utils/load_synapse.R')
source("analysis/utils/format_data.R")
source("analysis/utils/format_demographics.R")

## Necessary libraries ----
require(tidyverse)
require(magrittr)
require(data.table)
require(lubridate)
require(tableone)

## Load data ----
source("analysis/utils/load_demog_data.R")
source("analysis/utils/load_raw_fitbit_data.R")

## Cohort Demographics ----
### Overall Cohort Demographics ----
overall_cohort_demogs <- abcd_cohort_demogs_year_2 %>%
  mutate(race_ethnicity = factor(race_ethnicity, 
                                 levels = c("AIAN/P", "Asian", "Black", "Hispanic", 
                                            "Multiracial/ethnic", "White", "Other"))) %>%
  mutate(gender = factor(gender, levels = c("Male", "Female", 
                                            "Different/Gender Queer/Trans", 
                                            "Don't know/Refused to answer"))) %>%
  mutate(weight_category = factor(weight_category, levels = c("Healthy", "Obesity", 
                                                              "Missing", "Overweight", "Underweight"))) %>%
  mutate(fitbit_participant = factor(fitbit_participant, 
                                     levels = c("Yes", "No"))) %>%
  select(-BMI_zscore, -BMI_percentile, -eventname, -BMI) %>%
  left_join(fitbit_cohort_demogs_year_2 %>% select(participant_id) %>% 
              mutate(enrolment_fitbit = "Yes"), by = "participant_id") %>%
  left_join(hr_total_weartime %>% select(participant_id) %>%
              mutate(enrolment_fitbit_year_2_raw = "Yes"), by = "participant_id") %>%
  mutate(enrolment_fitbit_overall = 
           case_when(!is.na(enrolment_fitbit_year_2_raw) ~ "Year 2 - FC (Raw)",
                     is.na(enrolment_fitbit_year_2_raw) 
                     & !is.na(enrolment_fitbit) ~ "Year 2 - FC",
                     is.na(enrolment_fitbit_year_2_raw) & is.na(enrolment_fitbit) 
                     & !is.na(enrolment_fitbit) ~ "Year 2 - NFC",
                     TRUE ~ as.character("Withdrew")),
         enrolment_period = if_else(str_detect(enrolment_fitbit_overall, "Year 2"), 
                                    "Year 2 (FC)", "No Fitbit (NFC)"))

### Create TableOne ----
overall_cohort_tableone_df <- overall_cohort_demogs %>%
  mutate(enrolment_fitbit_overall = 
           if_else(str_detect(enrolment_fitbit_overall, "Raw"), "Year 2 - FC", enrolment_fitbit_overall)) %>%
  rename("Race/Ethnicity" = race_ethnicity, "Gender" = gender, 
         "Age" = age, "ADI" = most_recent_adi, 
         "BMI category" = weight_category,
         "Enrolment/Fitbit Availability" = enrolment_fitbit_overall,
         "Enrolment period" = enrolment_period) %>% 
  select(-site_location, -region) %>%
  column_to_rownames(var = "participant_id")

# Create TableOne
cohort_table_one <- CreateTableOne(
  data = overall_cohort_tableone_df, 
  strata = "Enrolment period",
  addOverall = TRUE)
# Write to CSV
write.csv(print(cohort_table_one), 
          "results/demographics/Table1_ABCD_Study_Cohort_Demographics_Revised.csv")

# Statistical tests
chisq <- chisq.test(overall_cohort_tableone_df$`Race/Ethnicity`, 
                    overall_cohort_tableone_df$`Enrolment period`)
kruskal <- kruskal.test(overall_cohort_tableone_df$Age, 
                    overall_cohort_tableone_df$`Enrolment period`)
kruskal <- kruskal.test(overall_cohort_tableone_df$ADI, 
                        overall_cohort_tableone_df$`Enrolment period`)
chisq <- chisq.test(overall_cohort_tableone_df$Gender, 
                    overall_cohort_tableone_df$`Enrolment period`)
chisq <- chisq.test(overall_cohort_tableone_df$`BMI category`, 
                    overall_cohort_tableone_df$`Enrolment period`)

## Supplementary ----

### Site-specific recruitment ----
site_recruitment_df <- abcd_cohort_demogs_year_2 %>%
  select(participant_id:most_recent_adi, weight_category,
         site_location, region) %>%
  group_by(site_location) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(prop = as.character(paste0(round((count/sum(count)*100), 2), "%")))
write.csv(site_recruitment_df,
          "results/demographics/ABCD_Study_Y2_Site_Recruitment.csv",
          row.names = F)

### Comparison of who withdrew ----
enrolment_comparison_df <- abcd_cohort_demogs_baseline %>%
  mutate(eventname = "baseline_year_1_arm_1") %>%
  select(participant_id, race_ethnicity, gender, most_recent_adi, weight_category,
         eventname) %>%
  left_join(overall_cohort_demogs %>% select(participant_id, race_ethnicity, gender, 
                                             most_recent_adi, weight_category) %>%
              mutate(eventname = "2_year_follow_up_y_arm_1"),
            by = c("participant_id"), suffix = c("_baseline", "_year2")) %>%
  mutate_all(~as.character(.)) %>%
  mutate(race_ethnicity = if_else(is.na(race_ethnicity_year2),
                                 race_ethnicity_baseline, race_ethnicity_year2),
         race_ethnicity = factor(race_ethnicity, 
                                 levels = c("AIAN/P", "Asian", "Black", "Hispanic", 
                                            "Multiracial/ethnic", "White", "Other"))) %>%
  mutate(gender = if_else(is.na(gender_year2),
                         gender_baseline, gender_year2),
         gender = factor(gender, levels = c("Male", "Female",
                                            "Different/Gender Queer/Trans",
                                            "Don't know/Refused to answer"))) %>%
  mutate(most_recent_adi = if_else(is.na(most_recent_adi_year2),
                                  most_recent_adi_baseline, most_recent_adi_year2),
         most_recent_adi = as.numeric(most_recent_adi)) %>%
  mutate(weight_category = if_else(is.na(weight_category_year2),
                                  weight_category_baseline, weight_category_year2)) %>%
  mutate(eventname = if_else(is.na(eventname_year2),
                                 eventname_baseline, eventname_year2)) %>%
  select(participant_id, race_ethnicity:eventname) %>%
  mutate(eventname = ifelse(eventname == "baseline_year_1_arm_1",
                            "Only baseline", "Stayed past Baseline")) %>%
  column_to_rownames(var = "participant_id") %>%
  mutate(race_ethnicity = factor(race_ethnicity,
                                 levels = c("AIAN/P", "Asian", "Black", "Hispanic", 
                                            "Multiracial/ethnic", "White", "Other")),
         gender = factor(gender, levels = c("Male", "Female",
                                            "Different/Gender Queer/Trans",
                                            "Don't know/Refused to answer")),
         weight_category = factor(weight_category,
                                  levels = c("Healthy", "Obesity", "Overweight",
                                             "Missing", "Underweight"))) %>%
  rename("Race/Ethnicity" = race_ethnicity, "Gender" = gender,
         "ADI" = most_recent_adi, "BMI category" = weight_category,
         "Enrolment" = eventname)

#### Create TableOne ----
enrolment_cohort_tableone <- CreateTableOne(
  data = enrolment_comparison_df,
  strata = "Enrolment", addOverall = TRUE)

write.csv(print(enrolment_cohort_tableone),
          "results/demographics/ABCD_Study_Enrolment_Comparison_TableOne_Revised.csv")

### Comparison of raw Fitbit vs. no Raw Fitbit ----
raw_fitbit_comparison_df <- fitbit_cohort_demogs_year_2 %>%
  left_join(hr_total_weartime %>% select(participant_id) %>%
              mutate(raw_fitbit = "Yes"),
            by = "participant_id") %>%
  mutate(raw_fitbit = if_else(is.na(raw_fitbit), "No", raw_fitbit)) %>%
  select(participant_id:most_recent_adi, weight_category, raw_fitbit) %>%
  column_to_rownames(var = "participant_id") %>%
  mutate(race_ethnicity = factor(race_ethnicity,
                                 levels = c("AIAN/P", "Asian", "Black", "Hispanic", 
                                            "Multiracial/ethnic", "White", "Other")),
         gender = factor(gender, levels = c("Male", "Female",
                                            "Different/Gender Queer/Trans",
                                            "Don't know/Refused to answer")),
         weight_category = factor(weight_category,
                                  levels = c("Healthy", "Obesity", "Overweight",
                                             "Missing", "Underweight"))) %>%
  rename("Age" = age, "Race/Ethnicity" = race_ethnicity, "Gender" = gender,
         "ADI" = most_recent_adi, "BMI category" = weight_category,
         "Raw Data Availability" = raw_fitbit)

#### Create TableOne ----
raw_fitbit_comparison_tableone <- CreateTableOne(
  data = raw_fitbit_comparison_df,
  strata = "Raw Data Availability", addOverall = TRUE)

write.csv(print(raw_fitbit_comparison_tableone),
          "results/demographics/ABCD_Study_Raw_Data_Availability_TableOne_Revised.csv")

### Overall Parent Demographics ----
overall_parent_demogs <- abcd_parent_demogs_year_2 %>%
  # Order by count
  mutate(ethnicity = factor(ethnicity, levels = c("AIAN/P", "Asian", "Black", "Hispanic", 
                                                  "Multiracial/ethnic", "White", "Other"))) %>%
  mutate(gender_summ = factor(gender_summ, levels = c("Female", "Male", "Different/Gender Queer/Trans", 
                                                      "Don't know/Refused to answer"))) %>%
  mutate(education_parent = case_when(education_parent == "ISCED 1" ~ "ISCED 1-3",
                                      education_parent == "ISCED 2" ~ "ISCED 1-3", 
                                      education_parent == "ISCED 3" ~ "ISCED 1-3",
                                      TRUE ~ as.character(education_parent)),
         education_parent = factor(education_parent, 
                                   levels = c("ISCED 6", "ISCED 5", "ISCED 7", 
                                              "ISCED 1-3", "ISCED 8", 
                                              "Refused to answer"))) %>%
  mutate(family_income = case_when(family_income == "< $11,999" ~ "< $24,999",
                                   family_income == "$12,000 - $24,999" ~ "< $24,999",
                                   family_income == "$100,000 - $199,999" ~ "> $100,000",
                                   family_income == "> $200,000" ~ "> $100,000",
                                   family_income == "Don't know" ~ "Don't know/Refused to answer",
                                   family_income == "Refused to answer" ~ "Don't know/Refused to answer",
                                   TRUE ~ as.character(family_income))) %>%
  mutate(family_income = factor(family_income, 
                                levels = c("< $24,999", "$25,000 - $49,999", "$50,000 - $74,999",
                                          "$75,000 - $99,999", "> $100,000", "Don't know/Refused to answer"))) %>%
  mutate(employment_parent = factor(employment_parent, levels = c("Working now, FULL TIME/PART TIME",
                                                                  "Stay at home parent", "Looking for work",
                                                                  "Disabled: permanently or temporarily",
                                                                  "Temporarily laid off",
                                                                  "Student",
                                                                  "Unemployed, not looking for work",
                                                                  "Retired", "Other/Refused to answer"))) %>%
  mutate(marital_status = factor(marital_status, levels = c("Married", "Never married",
                                                            "Divorced", "Living with partner",
                                                            "Separated", "Widowed", "Refused to answer"))) %>%
  mutate(fitbit_participant = factor(fitbit_participant, levels = c("Yes", "No"))) %>%
  left_join(fitbit_cohort_demogs_year_2 %>% select(participant_id) %>% 
              mutate(enrolment_fitbit = "Yes"), by = "participant_id") %>%
  left_join(hr_total_weartime %>% select(participant_id) %>%
              mutate(enrolment_fitbit_year_2_raw = "Yes"), by = "participant_id") %>%
  mutate(enrolment_fitbit_overall = 
           case_when(!is.na(enrolment_fitbit_year_2_raw) ~ "Year 2 - FC (Raw)",
                     is.na(enrolment_fitbit_year_2_raw) 
                     & !is.na(enrolment_fitbit) ~ "Year 2 - FC",
                     is.na(enrolment_fitbit_year_2_raw) & is.na(enrolment_fitbit) 
                     & !is.na(enrolment_fitbit) ~ "Year 2 - NFC",
                     TRUE ~ as.character("Withdrew")),
         enrolment_period = if_else(str_detect(enrolment_fitbit_overall, "Year 2"), 
                                    "Year 2 (FC)", "No Fitbit (NFC)"))

#### Create TableOne ----
overall_parent_tableone_df <- overall_parent_demogs %>%
  rename("Race/Ethnicity" = ethnicity, "Gender" = gender_summ, 
         "Age" = age, "Fitbit Data Available" = fitbit_participant,
         "Education" = education_parent, "Household Income" = family_income,
         "Employment Status" = employment_parent, "Marital Status" = marital_status,
         "Enrolment/Fitbit Availability" = enrolment_fitbit_overall,
         "Enrolment period" = enrolment_period) %>% 
  column_to_rownames(var = "participant_id") %>%
  select(-"Fitbit Data Available", -enrolment_fitbit, -enrolment_fitbit_year_2_raw)

parent_table_one <- CreateTableOne(data = overall_parent_tableone_df, 
                                   strata = "Enrolment period", 
                                   addOverall = TRUE)
# Write to CSV
write.csv(print(parent_table_one), 
          "results/demographics/ABCD_Study_Parent_Demographics_Revised.csv")


## Geospatial analysis

siteRecruitment <- abcd_cohort_demogs %>%
  group_by(site_location) %>%
  summarize(count = round(n()/11876*100, 3)) %>%
  arrange(desc(count))
write.csv(siteRecruitment, 'results/demogs/siteRecruitment.csv')

# Missing site ID's:
# INV4KRWWP18, INVCLL3TR97, INVJ3DJV7E6, INV3933Z8FN, INVZHCK39Y1, INVUD08XGWK, INVUVP8NKL5

## Temporal visualization of recruitment
temporalVis <- abcdStudyCohortDemogs_4_0 %>%
  select(participant_id, interview_date) %>%
  mutate(year_month = floor_date(as_date(interview_date, format = "%m/%d/%Y"), "month"))
temporalVis_byYearMonth <- temporalVis %>%
  group_by(year_month) %>%
  summarize(count = n())
temporalVis_byYearMonthSite <- temporalVis %>%
  inner_join(abcd_cohort_demogs %>% select(participant_id, site_location), by = "participant_id") %>%
  group_by(site_location, year_month) %>%
  summarize(count = n()) %>% ungroup() %>%
  group_by(site_location)
ggplot(data = temporalVis_byYearMonthSite, 
       aes(x = year_month, y = count, group = site_location,
           colour = site_location)) +
  geom_line() +
  geom_point() +
  labs(title = 'Participant recruitment by month, by site',
       x = 'Recruitment date', y = 'Participant count')
