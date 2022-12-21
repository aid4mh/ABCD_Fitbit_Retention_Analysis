# Fitbit wear time analysis ##################################################

## Source data loading / processing scripts -----
source("analysis/utils/format_data.R")
source("analysis/utils/format_demographics.R")
source("analysis/utils/create_heatmaps.R")

## Necessary libraries ----
require(tidyverse)
require(magrittr)
require(data.table)
require(factoextra)
require(amap)
require(circlize)
require(viridis)
require(ComplexHeatmap)
require(lubridate)
require(tableone)
require(ggsignif)
require(ggsci)

## Load datasets ----
source("analysis/utils/load_demog_data.R")
source("analysis/utils/load_raw_fitbit_data.R")

## Functions ----
map_pID_to_cluster <- function(heatmap_list, pID_list) {
  rowOrder <- row_order(heatmap)
  pID_by_cluster <- sapply(rowOrder, function(x) pID_list[x])
  names(pID_by_cluster) <- paste0('C', 1:length(pID_by_cluster))
  pID_by_cluster <- melt(pID_by_cluster)
  colnames(pID_by_cluster) <- c('participant_id', 'cluster')
  return(pID_by_cluster)
}

# Raw Fitbit data summarized by day
summarize_raw_fitbit_wear <- function(dataset) {
  df <- dataset %>%
    group_by(participant_id, protocol_date) %>%
    summarize(total_wear_hour = sum(total_hours)) %>%
    group_by(participant_id) %>%
    pivot_wider(names_from = protocol_date,
                values_from = total_wear_hour) %>%
    mutate(across(everything(), ~replace_na(.,0))) %>%
    as.data.frame() %>%
    column_to_rownames(var ="participant_id") %>%
    as.matrix()
  return(df)
}

## Cluster analysis ----

## Pivot to wider, per-day matrix
hr_data_summary <- hr_daily_weartime %>%
  summarize_raw_fitbit_wear() %>%
  as.data.frame() %>%
  rename("*2"= "2") %>%
  as.matrix()

## Elbow plots per distance metric
distance_metrics <- c("maximum", "manhattan", "euclidean")
elbow_plot_list <- list()
for (metric in distance_metrics) {
  elbow_plot_list[[metric]] <- fviz_nbclust(
    hr_data_summary, FUNcluster = cluster::clara,
    method = "wss", diss = dist(hr_data_summary, metric))
}
save(elbow_plot_list, file = "results/clustering/elbow_plot_list.RData")

## Cluster with various distance metrics
## After EDA, chose Manhattan
set.seed(12345) 
kmeans_dist_types <- list()
for (dist in c("spearman", "manhattan", "kendall")) {
  kmeans_result <- amap::Kmeans(hr_data_summary, centers = 4,
                          iter.max = 100, method = dist)
  kmeans_dist_types[[dist]] <- kmeans_result
}
save(kmeans_dist_types, file = "results/clustering/kmeans_dist_types_list.RData")

## Create heatmap
cluster_order <- factor(paste0("C", kmeans_dist_types$manhattan$cluster),
                        levels = c("C1", "C3", "C4", "C2"))
colour_legend <- colorRamp2(range(hr_data_summary), 
                            c("#fee0d2", "#de2d26"))
fig <- ComplexHeatmap::Heatmap(
  matrix = hr_data_summary, split = cluster_order,  
  cluster_row_slices = F, cluster_columns = F,
  show_column_names = T, show_column_dend = F,
  show_row_names = F, show_row_dend = F,
  # Set column label parameters
  name = "Heatmap", column_title = "Days",
  column_title_side = "bottom", column_names_gp = gpar(fontsize = 16),
  column_title_gp = gpar(fontsize = 18),
  # Set cluster label parameters
  row_title = c("C1", "C2", "C3", "C4"), row_title_gp = gpar(fontsize = 16),
  row_title_side = "left", 
  # Set heatmap legend parameters
  col = colour_legend, show_heatmap_legend = T,
  heatmap_legend_param = list(
    title = "Wear time (Hours)", at = c(0, 360, 720, 1080, 1440),
    labels = c("0", "6", "12", "18", "24"),
    direction = "horizontal"))
draw(fig, heatmap_legend_side = "top")
save(fig, file = "results/clustering/weartime_cluster_heatmap.RData")

### Temporal wear time analysis ----
hr_data_faceted <- fread("data/processed_fitbit/weartime/heartrate_1min_daily_faceted_summary.csv") %>%
  filter(timepoint == "2_year_follow_up_y_arm_1")

hr_data_faceted %>%
  group_by(participant_id, time_of_day) %>%
  summarize(total_min_per_time_of_day = sum(total_minutes)) %>%
  mutate(time_of_day = factor(time_of_day, 
                              levels = c("Late night", "Early morning",
                                         "Morning", "Afternoon", "Evening",
                                         "Night"))) %>%
  ggplot(aes(x = time_of_day, y = total_min_per_time_of_day)) +
  geom_boxplot()


### Diff demog. cluster analysis ----

## Map cluster membership to participant demographics
hr_data_summary_cluster <- as.data.frame(hr_data_summary) %>%
  add_column(cluster = cluster_order) %>%
  rownames_to_column(var = "participant_id") %>%
  select(participant_id, cluster) %>%
  left_join(fitbit_cohort_demogs_year_2, by = "participant_id") %>%
  left_join(hr_total_weartime, by = "participant_id") %>%
  column_to_rownames(var = "participant_id") %>%
  # Change cluster membership to match heatmap
  mutate(cluster = case_when(cluster == "C3" ~ "C2",
                             cluster == "C2" ~ "C4",
                             cluster == "C4" ~ "C3",
                             TRUE ~ as.character(cluster))) %>%
  # Mutate as factor to establish order in table
  mutate(gender = factor(gender, levels = c("Male", "Female", 
                                            "Different/Gender Queer/Trans",
                                            "Don't know/Refused to answer"))) %>%
  mutate(race_ethnicity = factor(race_ethnicity, 
                                 levels = c("White", "Hispanic", "Black",
                                            "Multiracial/ethnic",
                                            "AIAN/P", "Asian", "Other"))) %>%
  mutate(weight_category = factor(weight_category,
                                  levels = c("Healthy", "Obesity", "Overweight", 
                                             "Underweight", "Missing"))) %>%
  # Change vars to readable var names
  rename("Total weartime" = total_weartime, "Gender" = gender, 
         "Age" = age, "Race/Ethnicity" = race_ethnicity,
         "BMI category" = weight_category) %>%
  select(-site_location, -region)

hr_data_summary_cluster_parents <- as.data.frame(hr_data_summary) %>%
  add_column(cluster = cluster_order) %>%
  rownames_to_column(var = "participant_id") %>%
  select(participant_id, cluster) %>%
  left_join(fitbit_parent_demogs_year_2, by = "participant_id") %>%
  left_join(hr_total_weartime, by = "participant_id") %>%
  column_to_rownames(var = "participant_id") %>%
  # Change cluster membership to match heatmap
  mutate(cluster = case_when(cluster == "C3" ~ "C2",
                             cluster == "C2" ~ "C4",
                             cluster == "C4" ~ "C3",
                             TRUE ~ as.character(cluster))) %>%
  # Group ISCED 1-3 together and group family income
  mutate(education_parent = case_when(education_parent == "ISCED 1" ~ "ISCED 1-3",
                                      education_parent == "ISCED 2" ~ "ISCED 1-3",
                                      education_parent == "ISCED 3" ~ "ISCED 1-3",
                                      TRUE ~ as.character(education_parent)),
         family_income = case_when(family_income == "< $11,999" ~ "< $24,999",
                                   family_income == "$12,000 - $24,999" ~ "< $24,999",
                                   family_income == "$25,000 - $49,999" ~ "< $25,000 - $49,999",
                                   family_income == "$100,000 - $199,999" ~ "> $100,000",
                                   family_income == "> $200,000" ~ "> $100,000",
                                   family_income == "Don't know" ~ "Don't know/Refused to answer",
                                   family_income == "Refused to answer" ~ "Don't know/Refused to answer",
                                   TRUE ~ as.character(family_income))) %>%
  # Mutate as factor to establish order in table
  mutate(education_parent = factor(education_parent, 
                                   levels = c("ISCED 6", "ISCED 5", "ISCED 7",
                                              "ISCED 1-3", "ISCED 8", 
                                              "Refused to answer")),
         family_income = factor(family_income, 
                                levels = c("> $100,000", "$75,000 - $99,999", 
                                           "< $49,999", "$50,000 - $74,999", 
                                           "Don't know/Refused to answer")),
         employment_parent = factor(employment_parent, 
                                     levels = c("Working now, FULL TIME/PART TIME",
                                                "Stay at home parent", "Looking for work",
                                                "Student",
                                                "Disabled: permanently or temporarily",
                                                "Unemployed, not looking for work",
                                                "Temporarily laid off", "Retired")),
         marital_status = factor(marital_status, 
                                 levels = c("Married", "Divorced", "Never married",
                                            "Living with partner", "Separated",
                                            "Widowed", "Refused to answer")),
         gender_summ = factor(gender_summ, levels = c("Female", "Male", "Different/Gender Queer/Trans")),
         ethnicity = factor(ethnicity, levels = c("White", "Hispanic", "Black", "Asian",
                                                  "Multiracial/ethnic", "AIAN/P",
                                                  "Other"))) %>%
  # Change vars to readable var names
  rename("Total weartime" = total_weartime, "Parental education" = education_parent, 
         "Household income" = family_income, "Employment" = employment_parent,
         "Marital status" = marital_status, "Gender" = gender_summ,
         "Race/Ethnicity" = ethnicity)
  

## Create TableOne objects
hr_cluster_cohort_tableone <- CreateTableOne(
  strata = "cluster", data = hr_data_summary_cluster)
hr_cluster_parent_tableone <- CreateTableOne(
    strata = "cluster", data = hr_data_summary_cluster_parents)
write.csv(print(hr_cluster_cohort_tableone),
          "results/demographics/ABCD_Study_Cluster_Cohort_Demographics.csv")
write.csv(print(hr_cluster_parent_tableone),
          "results/demographics/ABCD_Study_Cluster_Parent_Demographics.csv")


### Create boxplots based on demographics ----
# Race/ethnicity
race_eth_cluster_boxplot <- hr_data_summary_cluster %>%
  group_by(`Race/Ethnicity`, cluster) %>%
  mutate(cluster = factor(cluster, levels = c("C4", "C3", "C2", "C1"))) %>%
  summarize(count = n()) %>%
  group_by(cluster) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(`Race/Ethnicity` %in% c("White", "Black", "Hispanic", "Multiracial/ethnic")) %>%
  mutate(`Race/Ethnicity` = factor(`Race/Ethnicity`, 
                                 levels = c("Multiracial/ethnic", "Hispanic", "Black", 
                                            "White"))) %>%
  ggplot(aes(y = cluster, x = proportion, fill = `Race/Ethnicity`)) +
  geom_col() + 
  theme_minimal() +
  labs(y = "Cluster\n", x = "\nProportion", fill = "Race/Ethnicity") +
  theme(axis.text.x = element_text(size = 16, hjust = 0.5, vjust = 0.8),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line.y.left = element_line(size = 0.5, color = "black"),
        axis.line.x.bottom = element_line(size = 0.5, color = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        legend.position = "none") + 
  #guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  scale_fill_jama()

# Household income
income_cluster_boxplot <- hr_data_summary_cluster_parents %>%
  group_by(`Household income`, cluster) %>%
  mutate(cluster = factor(cluster, levels = c("C4", "C3", "C2", "C1"))) %>%
  filter(!is.na(`Household income`)) %>%
  mutate(`Household income` = factor(`Household income`, 
                                     levels = c("> $100,000", "$75,000 - $99,999",
                                                "$50,000 - $74,999", "< $49,999",
                                                "Don't know/Refused to answer"))) %>%
  summarize(count = n()) %>%
  group_by(cluster) %>%
  mutate(proportion = count/sum(count)) %>%
  ggplot(aes(y = cluster, x = proportion, fill = `Household income`)) +
  geom_col() +
  theme_minimal() +
  labs(y = "Cluster\n", x = "\nProportion", fill = "Weight") +
  theme(axis.text.x = element_text(size = 16, hjust = 0.5, vjust = 0.8),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.line.y.left = element_line(size = 0.5, color = "black"),
        axis.line.x.bottom = element_line(size = 0.5, color = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        legend.position = "none") +
  scale_fill_jama()


# Arrange into one plot
weartime_fig <- multipanelfigure::multi_panel_figure(
  columns = 2, rows = 2, figure_name = "weartime_figure")
weartime_fig %<>%
  multipanelfigure::fill_panel(fig, row = 1:2, column = 1) %>%
  multipanelfigure::fill_panel(race_eth_cluster_boxplot, row = 1, column = 2) %>%
  multipanelfigure::fill_panel(weight_cat_cluster_boxplot, row = 2, column = 2)
grid_obj <- grid.grabExpr(draw(fig))
ggarrange(
  fig, race_eth_cluster_boxplot, weight_cat_cluster_boxplot,
  labels = c("A", "B", "C"),
  ncol = 3, nrow = 1)
