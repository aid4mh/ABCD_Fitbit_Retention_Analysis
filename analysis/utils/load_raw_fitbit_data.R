### Load raw heart rate Fitbit data ############################################

## Necessary libraries
require(tidyverse)
require(data.table)
require(gdata)

## Source data cleaning scripts
source("analysis/utils/format_data.R")

# Raw Fitbit #
hr_daily_weartime <- fread("data/processed_fitbit/weartime/heartrate_daily_weartime.csv") %>%
  filter(timepoint != "baseline_year_1_arm_1") %>%
  select(-timepoint) %>%
  mutate(total_hours = total_minutes/60)

hr_total_weartime <- hr_daily_weartime %>%
  group_by(participant_id) %>%
  summarize(total_weartime = sum(total_minutes)/60)
