## Functions for generating heatmaps #######################################

## Specific for feature-specific data
# Function for generating feature-specific heatmap
generate_feature_heatmap <- function(data, data_type, feature,
                                     number_of_clusters, cluster_order = NA) {
  set.seed(12345)
  # Select feature
  feature_data <- select_feature(data, data_type, feature)
  
  # Set clusters
  km <- kmeans(as.matrix(feature_data),centers = number_of_clusters)
  km.cluster.allocation <- factor(km$cluster)
  # Set custom cluster ordering
  if (is.na(cluster_order) == F){
    km.cluster.allocation <-  factor(km.cluster.allocation, levels=cluster_order)
  }
  
  # Set heatmap colours
  if (data_type == "qa_qc") {
    color_legend <- colorRamp2(c(min(feature_data), max(feature_data)), 
                               c('#e0e0e0', '#44c571'))
  }
  #color_legend <- colorRamp2(c(0, 1), c('#e0e0e0', '#2ca25f'))
  
  # Create heatmap
  fig <- Heatmap(as.matrix(feature_data),
                 split = km.cluster.allocation,
                 cluster_columns = F,
                 show_column_names = F, show_row_names = F,
                 column_title = feature,
                 show_row_dend = FALSE,
                 show_column_dend = FALSE,
                 col = color_legend,
                 show_heatmap_legend = FALSE)
  return(fig)
  
}

# Function to plot all heatmaps in one plot
plot_heatmaps <- function(heatmap_list, data_type, plot_title) {
  # sleep data figure ====
  if (data_type == "sleep") {
    fig <- multi_panel_figure(
      width = 200, height = 150,
      columns = 4, rows = 3)
    fig %<>% fill_panel(heatmap_list$total_sleep_minutes)
    fig %<>% fill_panel(heatmap_list$sleepperiod_minutes)
    fig %<>% fill_panel(heatmap_list$wake_minutes)
    fig %<>% fill_panel(heatmap_list$light_minutes)
    fig %<>% fill_panel(heatmap_list$deep_minutes)
    fig %<>% fill_panel(heatmap_list$rem_minutes)
    fig %<>% fill_panel(heatmap_list$wake_count)
    fig %<>% fill_panel(heatmap_list$avg_hr_wake)
    fig %<>% fill_panel(heatmap_list$avg_hr_light)
    fig %<>% fill_panel(heatmap_list$avg_hr_deep)
    fig %<>% fill_panel(heatmap_list$avg_hr_rem)
    
    # physical activity figure ====
  } else if (data_type == "physical") {
    fig <- multi_panel_figure(
      width = 250, height = 300,
      columns = 5, rows = 6)
    fig %<>% fill_panel(heatmap_list$day_total_steps_no_el)
    fig %<>% fill_panel(heatmap_list$day_min)
    fig %<>% fill_panel(heatmap_list$`30_second_data_existed`)
    fig %<>% fill_panel(heatmap_list$excl_day_min)
    fig %<>% fill_panel(heatmap_list$excl_day_min_hr50)
    fig %<>% fill_panel(heatmap_list$total_step)
    fig %<>% fill_panel(heatmap_list$total_ave_met)
    fig %<>% fill_panel(heatmap_list$total_sedentary_min)
    fig %<>% fill_panel(heatmap_list$total_light_active_min)
    fig %<>% fill_panel(heatmap_list$total_fairly_active_min)
    fig %<>% fill_panel(heatmap_list$total_very_active_min)
    fig %<>% fill_panel(heatmap_list$fitbit_totalsteps)
    fig %<>% fill_panel(heatmap_list$fitbit_sedentarymin)
    fig %<>% fill_panel(heatmap_list$fitbit_lightlyactivemin)
    fig %<>% fill_panel(heatmap_list$fitbit_fairlyactivemin)
    fig %<>% fill_panel(heatmap_list$fitbit_restingheartrate)
    fig %<>% fill_panel(heatmap_list$mstep_lt_80_dailystep)
    fig %<>% fill_panel(heatmap_list$dayt_total_steps)
    fig %<>% fill_panel(heatmap_list$dayt_ave_met_value)
    fig %<>% fill_panel(heatmap_list$dayt_sedentary_min)
    fig %<>% fill_panel(heatmap_list$dayt_light_active_min)
    fig %<>% fill_panel(heatmap_list$dayt_farily_active_min)
    fig %<>% fill_panel(heatmap_list$dayt_very_active_min)
  } else if (data_type == "qa_qc") {
    fig <- multi_panel_figure(
      width = 200, height = 400,
      columns = 4, rows = 4)
    fig %<>% fill_panel(heatmap_list$excl_day_min)
    fig %<>% fill_panel(heatmap_list$excl_day_min_hr50)
    fig %<>% fill_panel(heatmap_list$excl_day_min_nohr)
    fig %<>% fill_panel(heatmap_list$excl_day_min_hr_rept)
    fig %<>% fill_panel(heatmap_list$excl_night_min)
    fig %<>% fill_panel(heatmap_list$excl_night_min_hr50)
    fig %<>% fill_panel(heatmap_list$excl_night_min_nohr)
    fig %<>% fill_panel(heatmap_list$excl_night_min_hr_rept)
    fig %<>% fill_panel(heatmap_list$excl_sleep_min)
    fig %<>% fill_panel(heatmap_list$excl_sleep_min_hr50)
    fig %<>% fill_panel(heatmap_list$excl_sleep_min_nohr)
    fig %<>% fill_panel(heatmap_list$excl_sleep_min_rept)
    fig %<>% fill_panel(heatmap_list$mstep_lt_80_dailystep)
    fig %<>% fill_panel(heatmap_list$day_min_gt_600)
    fig %<>% fill_panel(heatmap_list$sleep_min_gt_300)
    fig %<>% fill_panel(heatmap_list$total_min)
  }
  
  filePath <- paste0("plots/",plot_title,".pdf")
  cat("Saving figure to: ", filePath)
  save_multi_panel_figure(fig, filename = filePath)
}

## Specific for per-day data
# Function to generate heatmap of per-day activity data
generate_perDay_heatmap <- function(data, pID_list = NA, 
                                    number_of_clusters,
                                    combined = F, cluster_order = NA,
                                    plot_colour = "green", plot_title,
                                    plot_quality = NA) {
  
  set.seed(12345)
  # is the data being fed in the combined activity data (i.e., compliance)?
  if (combined == F) {
    # create per-day activity data
    activity_df <- create_perDay_activity(data, pID_list)
  } else {
    # data can be read in as is
    activity_df <- data
  }
  
  ## Heatmap generation 
  # Create color legend
  if (plot_colour == "green") {
    color_legend <- colorRamp2(c(0, 1), c('#e0e0e0', '#2ca25f'))
  } else {
    color_legend <- colorRamp2(c(0, 1), c('#e0e0e0', plot_colour))
  }
  
  # Clustering according to number_of_clusters
  km <- kmeans(activity_df,centers = number_of_clusters)
  km.cluster.allocation <- factor(km$cluster)
  # Set cluster order - assumes 2 clusters
  km.cluster.allocation <- factor(km.cluster.allocation, levels = cluster_order)
  
  # Plot heatmap
  activity_heatmap <- Heatmap(
    matrix = activity_df, cluster_columns = F,
    row_split = km.cluster.allocation, 
    row_title = c("C1", "C2"),
    show_column_names = F, 
    show_row_names = F, show_row_dend = F,
    show_column_dend = F, col = color_legend,
    show_heatmap_legend = F, column_title = plot_title,
    column_title_gp = gpar(fontsize = 16, fontface = "bold"),
    raster_quality = plot_quality)
  return(activity_heatmap)
}

create_clustered_heatmap <- function(data, number_of_clusters, cluster_order_levels,
                                     cluster_numerical_order,
                                     legend_name, plot_title, legend_breakpoints,
                                     max_value, min_value, min_value_colour, 
                                     max_value_colour, cluster_result = NA) {
  set.seed(12345)
  if (!is.na(cluster_result)) {
    km <- cluster_result
  } else {
    km <- kmeans(data, centers = number_of_clusters)
  }
  
  if (is.na(cluster_order_levels)) {

  } else {
    km_split <- factor(paste0("C", km$cluster), levels = cluster_order_levels)
  }
  
  colour_legend <- colorRamp2(c(min_value, max_value),
                              c(min_value_colour, max_value_colour))
  
  fig <- ComplexHeatmap::Heatmap(
    matrix = data, split = km_split,  
    cluster_row_slices = F, cluster_columns = F,
    show_column_names = T, show_column_dend = F,
    show_row_names = F, show_row_dend = F,
    name = legend_name, column_title = plot_title,
    col = colour_legend, show_heatmap_legend = T,
    heatmap_legend_param = list(
      title = legend_name, at = legend_breakpoints,
      labels = legend_breakpoints
    ),
    row_title = cluster_numerical_order
  )
  return(fig)
}

# Map cluster of feature to participant ID's
map_pID_to_cluster <- function(heatmap, pID_list) {
  
  rowOrder <- row_order(heatmap)
  pID_by_cluster <- sapply(rowOrder, function(x) pID_list[x])
  names(pID_by_cluster) <- paste0('C', 1:length(pID_by_cluster))
  pID_by_cluster <- reshape2::melt(pID_by_cluster)
  colnames(pID_by_cluster) <- c('participant_id', 'cluster')
  return(pID_by_cluster)
  
}

# Creates clustered participant ID's, specific for each data feature
generate_feature_specific_cluster_pIDs <- function(feature, data, data_type, pIDs,
                                                   no_of_clusters) {
  data %<>%
    filter(participant_id %in% pIDs)
  # generate feature-specific heatmap
  heatmap <- generate_feature_heatmap(data, data_type, feature, no_of_clusters)
  # map the cluster-specific pID's to clusters
  cluster_pIDs <- map_pID_to_cluster(heatmap, unique(data$participant_id))
  
  return(cluster_pIDs)
}