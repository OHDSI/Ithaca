# codeToRun.r
#
# Quickstart runner for the Ithaca package workflow.
# Naming note: use camelCase for object names, function arguments, and list fields in active code.
# Update the placeholders below, then source this file.
#
# Example:
# source("codeToRun.r")

# Preferred: config-driven run
# writeIthacaWorkflowConfigTemplate("config.yaml", overwrite = FALSE)
# results <- runIthacaWorkflowFromConfig("config.yaml")

# library(Ithaca)

# 1) Define condition specifications -----------------------------------------------------
conditionSpecs <- list(
  list(
    testCondition = "Gastroesophageal reflux disease",
    dxCriteria = "1 inpatient/ED diagnosis OR 2 outpatient diagnoses 1-60 days apart",
    additionalInformation = "",
    rubricName = "Gastroesophageal reflux disease",
    cohortId = 14412
  )
)

# 2) Define database specifications ------------------------------------------------------
# Replace with your database spec objects loaded in your environment.
databases <- list(
  CCAE_OHDA_DBSpec
  # , MDCR_OHDA_DBSpec
)

# 3) Optional cohort definition set (required if generateCohorts = TRUE) ----------------
cohortDefinitionSet <- ithMakeCohortDefinitionSet(
  cohortIds = c(14412),
  cohortNames = c("gerdIndex"),
  cohortSql = c("SELECT ...")
)

# Optional: use WebAPI retrieval instead of manual cohortDefinitionSet
# (set cohortDefinitionSet <- NULL when using webapiBaseUrl)
# webapiBaseUrl <- "https://your-ohdsi-server/WebAPI"
webapiBaseUrl <- NULL

# 4) Optional pre/post rubric edits -------------------------------------------------------
preRunEdit <- list(
  conditionName = "Gastroesophageal reflux disease",
  ruleId = "ER1",
  updates = list(criteria = "updated pre-run criteria text"),
  regenerateSql = TRUE
)

postRunEdit <- list(
  conditionName = "Gastroesophageal reflux disease",
  ruleId = "ER2",
  updates = list(criteria = "updated post-run criteria text"),
  regenerateSql = TRUE
)

# 5) Run the full pipeline ---------------------------------------------------------------
results <- runIthacaExample(
  conditionSpecs = conditionSpecs,
  databases = databases,
  outputDirectory = "P:/llm/rubrics",
  promptDir = NULL,
  rubricTemplates = NULL,
  includeLabs = FALSE,
  useExistingRubric = FALSE,
  seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary_V64.rds",
  generateCohorts = FALSE,
  cohortDefinitionSet = cohortDefinitionSet,
  webapiBaseUrl = webapiBaseUrl,
  preRunEdit = preRunEdit,
  postRunEdit = postRunEdit,
  useExistingScores = FALSE,
  verbose = TRUE
)

# 6) Access outputs ----------------------------------------------------------------------
workflow <- results$workflow
scoreResults <- results$scoreResults

print(names(scoreResults))


# ---------------------------------------------------------------------------------------
# OPTIONAL: rubric-evaluation style validation flow
# ---------------------------------------------------------------------------------------
# This section demonstrates:
# 1) building a random OMOP cohort using a source cohort from WebAPI,
# 2) summarizing overlap,
# 3) running rubric-vs-phenotype algorithm report metrics.

# Example placeholders (replace with your environment values):
# connectionDetails <- DatabaseConnector::createConnectionDetails(...)
# cdm_database_schema <- DOD_OHDA_DBSpec$cdmDatabaseSchema
# cohort_database_schema <- DOD_OHDA_DBSpec$workDatabaseSchema
# cohortTableName <- paste0("random_cohort_", gsub("\\s+", "", DOD_OHDA_DBSpec$databaseId))
# webapiBaseUrl <- "https://epi.jnj.com:8443/WebAPI"

# build_result <- buildRandomOmopCohort(
#   connectionDetails = connectionDetails,
#   cdm_database_schema = cdm_database_schema,
#   cohort_database_schema = cohort_database_schema,
#   cohortTableName = cohortTableName,
#   cohort_definition_id = 26187001,
#   source_cohort_definition_id = 26187,
#   webapiBaseUrl = webapiBaseUrl,
#   sample_size = 2000000,
#   min_visit_start_date = "01/01/2016",
#   max_visit_end_date = "12/31/2026",
#   min_age = 0,
#   max_age = 120,
#   cohortName = "2M random persons with one random visit",
#   sourceCohortName = "Stevens-Johnson syndrome"
# )
# print(build_result)

# overlap_summary <- summarizeCohortOverlaps(
#   connectionDetails = connectionDetails,
#   cohort_database_schema = cohort_database_schema,
#   cohortTableName = cohortTableName,
#   cohortIds = c(26187),
#   randomCohortIds = c(26187001)
# )
# print(overlap_summary)

# rubric_report <- testPhenotypeAlgorithmRubricReport(
#   phenotype = "Stevens-Johnson syndrome",
#   analysisName = "rubric_validation",
#   runDateTime = as.character(Sys.time()),
#   connectionDetails = connectionDetails,
#   evaluationCohortFile = "P:/llm/phenotypes/Stevens-Johnson syndrome/Optum DOD/sumCohortScores_Cohort26187001_V1.rds",
#   phenotypeCohortId = c(26187),
#   cdmDatabaseSchema = cdm_database_schema,
#   databaseId = DOD_OHDA_DBSpec$databaseId,
#   cohortDatabaseSchema = cohort_database_schema,
#   cohortTable = cohortTableName,
#   yesScoreOverride = NULL,
#   noScoreOverride = NULL,
#   outputFolder = "P:/llm/phenotypes/Stevens-Johnson syndrome/Optum DOD",
#   frequencyPlotFpFnScores = TRUE,
#   generateTestingPhenotypeFirst = FALSE,
#   webapiBaseUrl = webapiBaseUrl,
#   splayPrior = 7,
#   splayPost = 7,
#   requireObservationPeriod = FALSE
# )
# print(rubric_report)
