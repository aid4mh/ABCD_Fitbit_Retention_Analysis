## Load data from Synapse #######################################

## Packages ########################################

library(synapser)
library(data.table)

## Functions #######################################

# load data from Synapse
load_synapse_data <- function(dataset_id) {
  # download from Synapse using syn_id;
  # relies on secrets
  files <- synGet(entity=dataset_id)
  # get cache file path
  filePath <- files$path
  # read csv
  data <- fread(filePath)
  return(data)
}

load_dataset_types <- function(dataset_type) {
  validTypes <- c("demographics", "prePostAssessments", "fitbit", "site", "res_history", "anthro")
  if (any(validTypes == dataset_type)) {
    if (dataset_type == "demographics") {
      cat("\nReading in demographic data\n")
      demogDatasets <- c(abcdStudyCohortDemogs_4_0 = "syn27220180", abcdStudyParentDemogs_4_0 = "syn27220181",
                         abcdStudyLongParentDemogs_4_0 = "syn32305169")
      datasets <- lapply(demogDatasets, function(x) load_synapse_data(x))
    } else if (dataset_type == "prePostAssessments") {
      cat("Reading in pre- and post-assessment data\n")
      prePostDatasets <- c(preCohortSurvey_4_0 = 'syn28085216', postCohortSurvey_4_0 = 'syn28085209', 
                           preParentSurvey_4_0 = 'syn28085230', postParentSurvey_4_0 = 'syn28085223')
      datasets <- lapply(prePostDatasets, function(x) load_synapse_data(x))
    } else if (dataset_type == "fitbit") {
      cat("Reading in daily-level summary Fitbit data\n")
      fitbitDatasets <- c(fitbitDailyPhys_4_0 = "syn27220179", fitbitDailySleep_4_0 = "syn27220177")
      datasets <- lapply(fitbitDatasets, function(x) load_synapse_data(x))
    } else if (dataset_type == "site") {
      cat("Reading in ABCD Study site data\n")
      datasets <- load_synapse_data('syn29179324')
    } else if (dataset_type == "res_history") {
      cat("Reading in residential history data\n")
      datasets <- load_synapse_data("syn29614230")
    } else if (dataset_type == "anthro") {
      datasets <- load_synapse_data("syn30057425")
    }
    return(datasets)
  } else {
    stop(paste("Invalid dataset_type. Please enter a valid dataset_type from the following:",
         "demographics, prePostAssessments, fitbit, site, res_history or anthro"))
  }
  
}

## Run script ######################################

# All Synapse-related variables are stored in 
# .synapseConfig, stored under the user base directory ~/
synLogin()
