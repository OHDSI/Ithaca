library(jsonlite)
library(SqlRender)
library(DatabaseConnector)
library(ellmer)
library(dplyr)
library(RubricConcepts)
library(Ithaca)

options(sqlRenderTempEmulationSchema = "scratch.scratch_jswerdel")

options(scipen = 999)

codeFolder <- "PhenotypeLibrary"
dataFolder <- "ProjectData/PheValuator"
drive <- "p"

source(paste0(drive, ":/", codeFolder, "/cdmSettings.R"), echo = FALSE)



# conditionList <- list(
#   list(
#     testCondition = "ST-elevation myocardial infarction",
#     dxCriteria = "1 inpatient/ED dx",
#     additionalInformation = "",
#     rubricName = "ST-elevation myocardial infarction",
#     cohortId = 24228
#   )
# )

conditions <- read.csv("P:/largeScalePhenotyping/condition.csv")
dxCriteria <- "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points."

conditionList <- list()
for(conditionUp in seq_len(nrow(conditions))) {
  cond <- list(testCondition = conditions$condition[[conditionUp]],
               dxCriteria = dxCriteria,
               additionalInformation = "",
               rubricName = conditions$condition[[conditionUp]],
               cohortId = conditions$prevCohort_id[[conditionUp]])
  conditionList[[conditionUp]] <- cond
}

conditionList <- list(Seizure, acuteLiverInjury, modSevAtopicDermatitis)
createRubricConceptSets(
  conditionList = conditionList,
  outputDirectory = "P:/llm/rubricsPears",
  database = CCAE_OHDA_DBSpec,  # swap if needed
  seedLibraryPath = "P:/llm/rubrics",
  similarityThreshold = 0.94,
  verbose = TRUE
)

#to add concept sets to library
updateConceptSetLibraryFromJson(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.rds",
  csName = "neurology_specialty_outpatient_visit_codes",
  conceptSetJsonPath = "P:/llm/rubricsNew/Ischemic stroke/neurology_specialty_outpatient_visit_codes/neurology specialty outpatient visit codes.json")


####post update
library(Ithaca)

config <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")
databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = .GlobalEnv)

# Pick one DB from general.cdm by name
databaseId <- "OptumDod"
if (!(databaseId %in% names(databases))) {
  stop("Database ID not found in config: ", databaseId)
}

module2Result <- createRubricConceptSets(
  conditionList = config$conditionSpecs,
  outputDirectory = as.character(config$outputDirectory[[1]]),
  database = databases[[databaseId]],
  seedLibraryPath = as.character(config$seedLibraryPath[[1]]),
  llmClientReasoning = Ithaca:::ithGetLlmClientFromConfig(config, "reasoning"),
  llmClientNonReasoning = Ithaca:::ithGetLlmClientFromConfig(config, "nonReasoning"),
  useLlmAdjudication = isTRUE(config$useLlmAdjudication),
  llmTopCandidates = as.integer(config$llmTopCandidates[[1]]),
  llmAmbiguityMinScore = config$llmAmbiguityMinScore,
  llmAmbiguityMaxScore = config$llmAmbiguityMaxScore,
  llmTop2GapThreshold = as.numeric(config$llmTop2GapThreshold[[1]]),
  llmConfidenceThreshold = as.numeric(config$llmConfidenceThreshold[[1]]),
  verbose = isTRUE(config$verbose)
)


#to delete a concept set by name
# dry run first
deleteConceptSetFromLibrary(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetName = "septic arthritis diagnosis codes",
  dryRun = TRUE
)

# apply delete
deleteConceptSetFromLibrary(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetName = "septic arthritis diagnosis codes",
  dryRun = FALSE
)
