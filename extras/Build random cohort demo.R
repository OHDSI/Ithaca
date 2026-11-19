# If needed:
# devtools::load_all("E:/Ithaca")

dbList <- list(DOD_OHDA_DBSpec, Panther_OHDA_DBSpec)

for(dbUp in seq_along(dbList)) {
  for(conditionUp in seq_len(nrow(conditions))) {
  # for(conditionUp in 5:5) {
    result <- buildRandomOmopCohort(
      connectionDetails = dbList[[dbUp]]$connectionDetails,
      cdm_database_schema = dbList[[dbUp]]$cdmDatabaseSchema,
      cohort_database_schema = dbList[[dbUp]]$workDatabaseSchema,
      cohortTableName = gsub(" ", "", paste0("largeScale", dbList[[dbUp]]$databaseId)), # or your cohort table
      randomSampleFromCohortDefinitionId = NULL,
      cohort_definition_id = as.integer(paste0(conditions$prevCohort_id[[conditionUp]], "001")), # NEW random cohort ID to write
      sourceCohortDefinitionId = conditions$prevCohort_id[[conditionUp]],            # existing source cohort in WebAPI
      webapiBaseUrl = "https://epi.jnj.com:8443/WebAPI",    # e.g. https://.../WebAPI
      sample_size = 2000000,
      min_visit_start_date = "01/01/2000",
      max_visit_end_date = "12/31/2026",
      min_age = conditions$minAge[[conditionUp]],
      max_age = conditions$maxAge[[conditionUp]],
      cohortName = "random_visit_sample",
      sourceCohortName = "source_webapi_cohort",
      seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.rds"
    )
  }
}

str(result)

############

devtools::load_all("E:/Ithaca")

source("buildRandomOmopCohortMultipleDatabases.R")

# to remove a cohort and force regen
# DELETE FROM scratch.scratch_jswerdel.ithaca_cdm_optum_ehr_v4154_checksum
# WHERE cohort_definition_id = 10617001;

