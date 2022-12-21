# Linear mixed effect modelling ###############################################
# Author: Ethan Kim

## Source data loading scripts ----
source('load_utils/load_synapse.R')
source("analysis/utils/format_data.R")
source("analysis/utils/format_demographics.R")

## Necessary libraries ----
require(tidyverse)
require(magrittr)
require(data.table)
require(lubridate)
require(lmerTest)
require(lme4)
require(sjPlot)    
require(ggpubr)

#install.packages("insight")
library("insight")

## Format data -----

### Load datasets
source("analysis/utils/load_demog_data.R")
source("analysis/utils/load_raw_fitbit_data.R")

### Create dataframe of enrolment by quarter_year
enrolment_df <- fread("data/intermediates/wear_date_df.csv") %>%
  group_by(participant_id) %>%
  arrange(protocol_date) %>%
  filter(row_number() == 1) %>%
  mutate(enrolment_q_yr = lubridate::quarter(wear_date, 
                                             with_year = TRUE)) %>%
  select(participant_id, enrolment_q_yr)

### Create socio-demographic dataframe for modelling
weartime_demogs <- fitbit_cohort_demogs_year_2 %>%
  inner_join(hr_total_weartime, by = "participant_id") %>%
  inner_join(fitbit_parent_demogs_year_2, by = "participant_id", 
             suffix = c("_child", "_parent")) %>%
  inner_join(enrolment_df, by = "participant_id") %>%
  # Assign 2019 2nd quarter as default (most enrolled)
  mutate(enrolment_q_yr = relevel(as.factor(enrolment_q_yr), "2019.3")) %>%
  # Compress demographic variables
  mutate(marital_status = 
           case_when(marital_status == "Refused to answer" ~ as.character(NA),
                     marital_status == "Separated" ~ "Divorced/Separated/Widowed",
                     marital_status == "Divorced" ~ "Divorced/Separated/Widowed",
                     marital_status == "Widowed" ~ "Divorced/Separated/Widowed",
                     TRUE ~ as.character(marital_status))) %>%
  mutate(marital_status = relevel(factor(marital_status), "Married")) %>%
  mutate(family_income = 
           case_when(family_income == "< $11,999" ~ "< $25,000",
                     family_income == "$12,000 - $24,999" ~ "< $25,000",
                     family_income == "$100,000 - $199,999" ~ "> $100,000",
                     family_income == "> $200,000" ~ "> $100,000",
                     TRUE ~ as.character(family_income))) %>%
  mutate(family_income = relevel(factor(family_income), "$50,000 - $74,999")) %>%
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
  mutate(education_parent = relevel(factor(education_parent), "ISCED 8")) %>%
  rename(ADI = most_recent_adi) %>%
  mutate(adi_percentile = na_if(adi_percentile, 'NA')) %>%
  mutate(adi_percentile = factor(adi_percentile)) %>%
  mutate(adi_percentile = relevel(adi_percentile, "quartile_4"))

## Mixed effect modelling ----

### Exploratory models ----

#### Base regression model ----
lm_base <- lm(
  total_weartime ~ race_ethnicity + gender + weight_category + marital_status + 
    family_income + education_parent + adi_percentile + region, 
  data = weartime_demogs)
summary(lm_base)

#### LMER base model ----
# All sociodemographic variables + random effect for site location
lmer_base <- lmerTest::lmer(
  total_weartime ~ race_ethnicity + gender + weight_category + marital_status + 
    family_income + education_parent + adi_percentile + (1|site_location),
  data = weartime_demogs, REML = FALSE)
summary(lmer_base)

# indicating that the model is overfitted. Let us check for collinearity:
lmer_base_collinear <- car::vif(lmer_base)
lmer_base_collinear

#### LMER second model ----
# All sociodemographic variables + enrolment_q_yr; random effect for site location 
lmer_2 <- lmerTest::lmer(
  total_weartime ~ race_ethnicity + gender + weight_category + marital_status + 
    family_income + education_parent + adi_percentile + enrolment_q_yr + region  + (1|site_location),
  data = weartime_demogs, REML = FALSE)
summary(lmer_2)
lmer_2_collinear <- car::vif(lmer_2)
lmer_2_collinear
get_variance_random(lmer_2)
anova(lmer_base, lmer_2)

#### LMER third model ----
# All sociodemographic variables + random effect for site location +  enrolment_q_yr
lmer_3 <- lmerTest::lmer(
  total_weartime ~ race_ethnicity + gender + weight_category + marital_status + 
    family_income + education_parent + adi_percentile  + region  + (1|site_location) + (1|enrolment_q_yr),
  data = weartime_demogs, REML = FALSE)
summary(lmer_3)
car::vif(lmer_3)
get_variance_random(lmer_3)
anova(lmer_base, lmer_2, lmer_3)


#### LMER fourth model ----
# All sociodemographic variables + random effect for site location + 
# enrolment_q_yr + adi_percentile
lmer_4 <- lmerTest::lmer(
  total_weartime ~ race_ethnicity + gender + weight_category + marital_status + 
    family_income + education_parent  + region  + (1|site_location) + (1|enrolment_q_yr) + (1|adi_percentile),
  data = weartime_demogs, REML = FALSE)
summary(lmer_4)
car::vif(lmer_4)
get_variance_random(lmer_4)
anova(lmer_3, lmer_4)

### Finalized model ----
# Set seed for reproducibility
set.seed(12345)
final_lmer_model <- lmerTest::lmer(
  total_weartime ~ race_ethnicity + gender + weight_category + marital_status + 
    family_income + education_parent + adi_percentile + region  + 
    (1|site_location) + (1|enrolment_q_yr),
  data = weartime_demogs, REML = FALSE)

# LME Table 
lme_tbl <- tab_model(final_lmer_model)

## 95% confidence intervals
lmer_confints <- confint(final_lmer_model)
### Save model
save(final_lmer_model, file = "data/final_lmer_model.RData")
write.csv(lmer_confints, "data/lmer_confints.csv")

## Sociodemographic wear time plots ----

### Function to plot LME-specific demographics
plot_lme_demog_plots <- function(data, demog_var, demog_var_title,
                                 var_order, fill_colour, fill_var, 
                                 show_legend = FALSE) {
  
  var <- enquo(demog_var)
  df <- data %>%
    mutate(!!var := factor(!!var, levels = var_order)) %>%
    filter(!is.na(!!var))
  
  if (fill_colour == TRUE) {
    
    plot <- df %>%
      ggplot(aes(x = total_weartime, y = !!var, fill = !!enquo(fill_var))) +
      geom_boxplot()
    
  } else if (fill_colour == FALSE) {
    
    plot <- df %>%
      ggplot(aes(x = total_weartime, y = !!var)) +
      geom_boxplot()
    
  }
  # Add theme
  plot <- plot +
    labs(x = "\nTotal wear time (hours)", y =  paste0(demog_var_title, "\n")) +
    theme(axis.text.x = element_text(size = 16, hjust = 0.5, vjust = 0.8),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_text(size = 18), 
          axis.title.x = element_text(size = 18),
          axis.line.y.left = element_line(size = 0.5, color = "black"),
          axis.line.x.bottom = element_line(size = 0.5, color = "black"),
          panel.background = element_rect(fill = "white", colour = "white"),
          plot.margin = margin(t = 10, r = 30, b = 10, l = 10))
  
  if (show_legend == FALSE) {
    plot <- plot +
      theme(legend.position = "none")
  } 
  
  return(plot)
}

### Linear mixed effect (LME) model-specific demographic dataframe
weartime_lme_plot_df <- fitbit_cohort_demogs_year_2 %>%
  inner_join(hr_total_weartime, by = "participant_id") %>%
  inner_join(fitbit_parent_demogs_year_2, by = "participant_id", 
             suffix = c("_child", "_parent")) %>%
  select(-eventname, -contains("BMI"), -contains("age"), -contains("fitbit_participant")) %>%
  mutate(race_ethnicity = factor(race_ethnicity, 
                                 levels = c("White", "Hispanic", "Black", 
                                            "Multiracial/ethnic",
                                            "AIAN/P", "Asian", "Other"))) %>%
  mutate(gender = factor(gender, levels = c("Male", "Female", 
                                            "Different/Gender Queer/Trans", 
                                            "Don't know/Refused to answer"))) %>%
  mutate(weight_category = factor(weight_category, levels = c("Healthy", "Obesity", 
                                                              "Missing", "Overweight", 
                                                              "Underweight"))) %>%
  mutate(ethnicity = factor(ethnicity, levels = c("White", "Hispanic", "Black", 
                                                  "Asian", "Multiracial/ethnic", 
                                                  "AIAN/P", "Other"))) %>%
  mutate(gender_summ = ifelse(gender_summ %in% c("Don't know", "Refused to answer"),
                              "Don't know/Refused to answer", gender_summ),
         gender_summ = factor(gender_summ, levels = c("Female", "Male", 
                                                      "Different/Gender Queer/Trans", 
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
                                   family_income == "$12-000, $24,999" ~ "< $24,999",
                                   family_income == "> $200,000" ~ "> $100,000",
                                   family_income == "$100,000 - $199,999" ~ "> $100,000",
                                   family_income == "Don't know" ~ "Don't know/Refused to answer",
                                   family_income == "Refused to answer" ~ "Don't know/Refused to answer",
                                   TRUE ~ as.character(family_income)),
         family_income = factor(family_income, 
                                levels = c("> $100,000", "$75,000 - $99,999", 
                                           "$50,000 - $74,999", "$25,000 - $49,999", 
                                           "< $24,999", "Don't know/Refused to answer"))) %>%
  mutate(employment_parent = ifelse(employment_parent %in% c("Other", "Don't know"),
                                    "Other/Refused to answer", employment_parent),
         employment_parent = factor(employment_parent, 
                                    levels = c("Working now, FULL TIME/PART TIME",
                                               "Stay at home parent", "Looking for work",
                                               "Disabled: permanently or temporarily",
                                               "Temporarily laid off", "Student",
                                               "Unemployed, not looking for work",
                                               "Retired", "Other/Refused to answer"))) %>%
  mutate(marital_status = factor(marital_status, levels = c("Married", "Never married",
                                                            "Divorced", "Living with partner",
                                                            "Separated", "Widowed", "Refused to answer"))) %>%
  inner_join(enrolment_df, by = "participant_id") %>%
  mutate(enrolment_q_yr = factor(enrolment_q_yr,
                                 levels = c("2018.4", "2019.1", "2019.2", "2019.3", 
                                            "2019.4", "2020.1", "2020.2", "2020.3", 
                                            "2020.4"))) %>%
  mutate(enrolment_q_yr = 
           case_when(enrolment_q_yr == "2018.4" ~ "Q4 2018", enrolment_q_yr == "2019.1" ~ "Q1 2019",
                     enrolment_q_yr == "2019.2" ~ "Q2 2019", enrolment_q_yr == "2019.3" ~ "Q3 2019",
                     enrolment_q_yr == "2019.4" ~ "Q4 2019", enrolment_q_yr == "2020.1" ~ "Q1 2020",
                     enrolment_q_yr == "2020.2" ~ "Q2 2020", enrolment_q_yr == "2020.3" ~ "Q3 2020",
                     enrolment_q_yr == "2020.4" ~ "Q4 2020"))

### Race/ethnicity ----
race_eth_LME_plot <- plot_lme_demog_plots(
  data = weartime_lme_plot_df, demog_var = race_ethnicity, 
  demog_var_title = "Race/Ethnicity",
  var_order = c("Black", "Other", "Hispanic", 
                "AIAN/P", "Multiracial/ethnic", "White", "Asian"),
  fill_colour = FALSE) +
  ggtitle("Differential total Fitbit wear-time between race/ethnicity groups")

### Site-specific wear time ----
site_weartime_LME_plot <- plot_lme_demog_plots(
  data = weartime_lme_plot_df, demog_var = site_location, 
  demog_var_title = "Site ID", 
  var_order = c("UPMC", "FIU", "UFL", "UCSD", "ROC", "VCU", "UMB", "UCLA", "WUSTL",
                "LIBR", "MUSC", "UVM", "UMICH", "OHSU", "CUB", "UWM", "YALE", "SRI",  
                "UMN", "UTAH", "CHLA"),
  fill_colour = TRUE, fill_var = region, show_legend = TRUE) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  ggsci::scale_fill_jama()


### Enrolment plot ----
weartime_lme_plot_df %>%
  mutate(covid_19_yr = case_when(str_detect(enrolment_q_yr, "2018") ~ "No",
                                 str_detect(enrolment_q_yr, "2019") ~ "No",
                                 str_detect(enrolment_q_yr, "Q1 2020") ~ "No",
                                 TRUE ~ "Yes")) %>%
  group_by(covid_19_yr) %>%
  summarize(median_weartime = median(total_weartime))
enrolment_LME_plot <- plot_lme_demog_plots(
  data = weartime_lme_plot_df, demog_var = enrolment_q_yr,
  demog_var_title = "Enrolment (Year-Quarter)",
  var_order = c("Q4 2020", "Q3 2020", "Q2 2020", "Q1 2020", 
                "Q4 2019", "Q3 2019", "Q2 2019", "Q1 2019", 
                "Q4 2018"),
  fill_colour = FALSE) +
  annotate("rect", xmin = 9000, xmax = 27000, ymin = 0.5, ymax = 3.5, alpha = 0.1,
           fill = "blue") +
  geom_segment(aes(x = 21113, y = 0, xend = 21113, yend = 9.5, colour = "red"), 
               linetype = 2) +
  geom_segment(aes(x = 23598, y = 0, xend = 23891, yend = 9.5, colour = "darkblue"), 
               linetype = 2)

### Combined plot ----
lmer_demog_plots <- ggarrange(
  race_eth_LME_plot, site_weartime_LME_plot, enrolment_LME_plot,
  ncol = 3, nrow = 1)

## Supplementary ----

### Enrolment by quarter ----
enrolment_df %>%
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
  group_by(enrolment_q_yr) %>%
  summarize(count = n()) %>%
  mutate(sum = sum(count), prop = count/sum) %>%
  ggplot(aes(x = enrolment_q_yr, y = prop, group = enrolment_q_yr)) +
  geom_col() +
  labs(x = "\nEnrolment (Quarter - Year)", y = "Proportion") +
  theme(axis.text.x.bottom = element_text(size = 13, angle = 90,
                                          hjust = 0.9, vjust = 0.5),
        axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        panel.background = element_rect(fill = "white", colour = "white",
                                        linetype = "solid"),
        axis.line.y.left = element_line(size = 0.5, color = "black"),
        axis.line.x.bottom = element_line(size = 0.5, color = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10))

### Missingness of weight before vs. during COVID ----
weartime_lme_plot_df %>%
  mutate(covid_affected = ifelse(str_detect(enrolment_q_yr, "2020"), "Yes", "No")) %>%
  group_by(weight_category, covid_affected) %>%
  summarize(count = n()) %>%
  ggplot(aes(weight_category, count, fill = covid_affected)) +
  geom_col()

### Sociodemographics of before vs. during COVID ----
sociodemogs_covid <- weartime_lme_plot_df %>%
  mutate(covid_19_yr = case_when(str_detect(enrolment_q_yr, "2018") ~ "No",
                                 str_detect(enrolment_q_yr, "2019") ~ "No",
                                 str_detect(enrolment_q_yr, "Q1 2020") ~ "No",
                                 TRUE ~ "Yes")) %>%
  select(-participant_id, -site_location, -enrolment_q_yr) %>%
  select(race_ethnicity:total_weartime, education_parent:covid_19_yr) %>%
  rename("Child race/ethnicity" = race_ethnicity, 
         "Child gender" = gender, "ADI" = most_recent_adi, "BMI category" = weight_category,
         "Total weartime" = total_weartime, "Education" = education_parent,
         "Household income" = family_income, "Employment" = employment_parent,
         "Marital status" = marital_status, "COVID-19 Enrolment" = covid_19_yr,
         "Site Region" = region)
sociodemogs_covid_tableone <- CreateTableOne(
  data = sociodemogs_covid, strata = "COVID-19 Enrolment")
write.csv(print(sociodemogs_covid_tableone),
          "results/demographics/Supp_ABCD_Study_COVID-19_Demographics.csv")

# Gender
gender_LME_plot <- plot_lme_demog_plots(
  data = weartime_lme_plot_df, demog_var = gender, 
  demog_var_title = "Gender", 
  var_order = c("Don't know/Refused", "Female", "Male", "D/G/T"),
  fill_colour = FALSE) +
  annotate("label", x = 3.7, y = 1000, label = "LMER p < 0.001")
# Marital status
marital_stat_LME_plot <- plot_lme_demog_plots(
  data = weartime_lme_plot_df, demog_var = marital_status, 
  demog_var_title = "Parent's marital status", 
  var_order = c("Married", "Single parent", "Living with partner", "Never married"),
  fill_colour = FALSE) +
  annotate("label", x = 3.7, y = 1000, label = "LMER p < 0.001")
# Parental education
family_income_LME_plot <- plot_lme_demog_plots(
  data = weartime_lme_plot_df, demog_var = family_income, 
  demog_var_title = "Household income", 
  var_order = c(">$100K", "$75K-$99.9K", "$50K-$74.9K", "$25K-$49.9K", 
                "Don't know/Refused", "<$25K"),
  fill_colour = FALSE) +
  annotate("label", x = 5.7, y = 1000, label = "LMER p < 0.001")
# Parental education
parental_edu_LME_plot <- plot_lme_demog_plots(
  data = weartime_lme_plot_df, demog_var = education_parent, 
  demog_var_title = "Parental education", 
  var_order = c("ISCED 6","ISCED 8", "ISCED 7", "ISCED 5", "ISCED 1-3"),
  fill_colour = FALSE) +
  annotate("label", x = 4.7, y = 1000, label = "LMER p < 0.001")
