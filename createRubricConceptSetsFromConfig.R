# Create concept sets for every condition in conditionSpecsCsvPath using settings from config.yaml.
# The database supplies OMOP vocabulary access: conceptSetBuilding.conceptSetDatabaseId, or the first general.cdm entry.
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("createRubricConceptSetsFromConfig.R")

library(Ithaca)

config <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")
databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = .GlobalEnv)

databaseId <- as.character(Ithaca:::ithCoalesce(config$conceptSetDatabaseId, names(databases)[[1]])[[1]])
if (!(databaseId %in% names(databases))) {
  stop("Concept set database ID not found in config: ", databaseId)
}
message("Creating concept sets using database: ", databaseId)

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
  conceptSetWorkers = Ithaca:::ithConfigParallelWorkers(config, "conceptSetCreation", legacyKey = "conceptSetWorkers"),
  conceptSetFolder = config$conceptSetFolder,
  emptyConceptSet = as.character(Ithaca:::ithCoalesce(config$emptyConceptSet, "placeholder")[[1]]),
  emptyConceptSetFallback = !identical(config$emptyConceptSetFallback, FALSE),
  rebuildEmptyConceptSets = isTRUE(config$rebuildEmptyConceptSets),
  llmConfig = config,
  verbose = isTRUE(config$verbose)
)
