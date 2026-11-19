# extras/codeToRun.r
#
# Module-by-module, run-ready guide for Ithaca.
#
# This file includes:
#   0) Config-first call (recommended)
#   1) Module 1: Rubric creation
#   2) Module 2: Concept set creation/update
#   3) Module 3: Cohort generation
#   4) Module 4: Subject scoring
#
# Usage:
#   source("extras/codeToRun.r")

library(Ithaca)

# ============================================================================
# 0) CONFIG-FIRST CALL (recommended)
# ============================================================================
# One-time template creation:
# writeIthacaWorkflowConfigTemplate("config.yaml", overwrite = FALSE)
#
# Full workflow from config:
# configResults <- runIthacaWorkflowFromConfig("config.yaml")
#
# Optional random cohort build from same config:
# randomCohortResults <- buildRandomOmopCohortFromConfig("config.yaml")


# ============================================================================
# SHARED INPUTS USED BY MODULE CALLS BELOW
# ============================================================================

# Required for modules 1, 2, and 4:
conditionList <- list(
  list(
    testCondition = "Gastroesophageal reflux disease",
    dxCriteria = "1 inpatient/ED diagnosis OR 2 outpatient diagnoses 1-60 days apart",
    additionalInformation = "",
    rubricName = "Gastroesophageal reflux disease",
    cohortId = 14412
  )
)

# Required for modules 1, 2, and 4:
outputDirectory <- "P:/llm/rubrics"

# Required for modules 2 and 4:
seedLibraryPath <- "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"

# Required for modules 2, 3, and 4:
# Replace with your real DB spec objects loaded in your environment.
databases <- list(
  CCAE_OHDA_DBSpec
  # , MDCR_OHDA_DBSpec
)

# Required for modules that need WebAPI retrieval:
webapiBaseUrl <- "https://your-ohdsi-server/WebAPI"

# Optional, but recommended explicit clients:
llmClientReasoning <- ithGetLlmClientFromConfig(list(), "reasoning")
llmClientNonReasoning <- ithGetLlmClientFromConfig(list(), "nonReasoning")

# Required for module 4 reproducibility pin:
# Replace with the exact concept library version you want to score against.
conceptLibraryVersion <- 1L

# Optional per-database cohort table mapping for module 4
# Names must match each database's databaseId.
# cohortTableName <- c(
#   CCAE = "pv_table_ccae",
#   MDCR = "pv_table_mdcr"
# )
cohortTableName <- NULL


# ============================================================================
# 1) MODULE 1: RUBRIC CREATION
# Function: getRubrics(...)
# ============================================================================
# Needed:
# - conditionList
# - outputDirectory
# - llmClient (optional override)

module1Result <- getRubrics(
  conditionList = conditionList,
  outputDirectory = outputDirectory,
  rubricTemplates = NULL,
  includeLabs = FALSE,
  promptDir = NULL,
  llmClient = llmClientReasoning,
  useLibraryHints = FALSE,
  libraryHintSimilarityThreshold = 0.94,
  libraryHintTopCandidates = 5,
  useExistingRubric = FALSE,
  verbose = TRUE
)


# ============================================================================
# 2) MODULE 2: CONCEPT SET CREATION / UPDATE
# Function: createRubricConceptSets(...)
# ============================================================================
# Needed:
# - conditionList
# - outputDirectory
# - database (single DB object)
# - seedLibraryPath (DuckDB path)

module2Result <- createRubricConceptSets(
  conditionList = conditionList,
  outputDirectory = outputDirectory,
  database = databases[[1]],
  seedLibraryPath = seedLibraryPath,
  llmClientReasoning = llmClientReasoning,
  llmClientNonReasoning = llmClientNonReasoning,
  similarityThreshold = 0.94,
  fuzzyThreshold = 0.94,
  useLlmAdjudication = TRUE,
  llmTopCandidates = 25,
  llmAmbiguityMinScore = NULL,
  llmAmbiguityMaxScore = NULL,
  llmTop2GapThreshold = 0.015,
  llmConfidenceThreshold = 0.60,
  verbose = TRUE
)


# ============================================================================
# 3) MODULE 3: COHORT GENERATION
# Function: generateCohorts(...)
# ============================================================================
# Needed:
# - dbList = databases
# - either:
#   A) cohortDefinitionSet with columns cohortId, cohortName, sql
#   B) cohortIds + webapiBaseUrl

# Option A: Manual cohortDefinitionSet
cohortDefinitionSet <- ithMakeCohortDefinitionSet(
  cohortIds = c(14412),
  cohortNames = c("gerd_index"),
  cohortSql = c("SELECT person_id AS subject_id FROM your_source_table")
)

module3Result <- generateCohorts(
  cohortDefinitionSet = cohortDefinitionSet,
  dbList = databases,
  cohortTable = cohortTableName,
  stopOnError = TRUE,
  incremental = TRUE,
  createTables = TRUE,
  verbose = TRUE
)

# Option B: WebAPI-based cohort definition retrieval (if no SQL supplied)
# module3Result <- generateCohorts(
#   cohortDefinitionSet = NULL,
#   dbList = databases,
#   cohortIds = c(14412),
#   webapiBaseUrl = webapiBaseUrl,
#   cohortTable = cohortTableName,
#   stopOnError = TRUE,
#   incremental = TRUE,
#   createTables = TRUE,
#   verbose = TRUE
# )


# ============================================================================
# 4) MODULE 4: SUBJECT SCORING
# Function: scoreSubjects(...)
# ============================================================================
# Needed:
# - conditionList
# - database = databases (one or more)
# - outputDirectory
# - conceptLibraryVersion (required pin)
# - seedLibraryPath (recommended explicit)
# - cohortDefinitionSet OR webapiBaseUrl if generateCohort = TRUE

module4Result <- scoreSubjects(
  conditionList = conditionList,
  database = databases,
  outputDirectory = outputDirectory,
  seedLibraryPath = seedLibraryPath,
  conceptLibraryVersion = conceptLibraryVersion,
  cohortTableName = cohortTableName,
  useExisting = FALSE,
  generateCohort = FALSE,
  cohortDefinitionSet = cohortDefinitionSet,
  webapiBaseUrl = webapiBaseUrl,
  llmClient = llmClientReasoning,
  promptDir = NULL,
  verbose = TRUE
)


# ============================================================================
# QUICK CHECKS
# ============================================================================
print("Module 1 complete: rubrics created/updated")
print("Module 2 complete: concept sets matched/created in DuckDB")
print("Module 3 complete: cohorts generated")
print("Module 4 complete: scoring artifacts generated")

if (!is.null(module4Result)) {
  print(names(module4Result))
}
