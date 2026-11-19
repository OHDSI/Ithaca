# Build a random OMOP cohort from visit_occurrence using OHDSI tooling
# (DatabaseConnector + SqlRender + CohortGenerator), targeting Databricks.

resolveSqlFilePath <- function(sqlFilePath) {
  if (grepl("^([A-Za-z]:)?[/\\\\]", sqlFilePath)) {
    return(sqlFilePath)
  }

  projectCandidate <- file.path(getwd(), sqlFilePath)
  if (file.exists(projectCandidate)) {
    return(projectCandidate)
  }

  scriptCandidate <- file.path(dirname(sys.frame(1)$ofile %||% ""), sqlFilePath)
  if (nzchar(scriptCandidate) && file.exists(scriptCandidate)) {
    return(scriptCandidate)
  }

  return(projectCandidate)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

getDateOverwriteValidation <- function(connectionDetails,
                                          validationSqlFilePath,
                                          cohort_database_schema,
                                          cohortTableName,
                                          cohort_definition_id,
                                          sourceCohortDefinitionId) {
  validationSql <- paste(readLines(validationSqlFilePath, warn = FALSE), collapse = "\n")

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = validationSql,
    cohort_database_schema = cohort_database_schema,
    cohort_table_name = cohortTableName,
    cohort_definition_id = as.integer(cohort_definition_id),
    source_cohort_definition_id = as.integer(sourceCohortDefinitionId),
    snakeCaseToCamelCase = TRUE
  )
}

getCohortRowCount <- function(connectionDetails,
                              cohort_database_schema,
                              cohortTableName,
                              cohort_definition_id) {
  sql <- paste0(
    "SELECT COUNT(*) AS row_count FROM ",
    cohort_database_schema,
    ".",
    cohortTableName,
    " WHERE cohort_definition_id = @cohort_definition_id"
  )

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  rowCountDf <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cohort_definition_id = as.integer(cohort_definition_id),
    snakeCaseToCamelCase = TRUE
  )

  if (is.null(rowCountDf) || nrow(rowCountDf) == 0 || !("rowCount" %in% names(rowCountDf))) {
    return(NA_integer_)
  }

  as.integer(rowCountDf$rowCount[[1]])
}

#' Summarize overlap counts between source and random cohorts
#'
#' @param connectionDetails DatabaseConnector connection details.
#' @param cohort_database_schema Cohort schema.
#' @param cohortTableName Cohort table name.
#' @param cohortIds Source cohort IDs.
#' @param randomCohortIds Random cohort IDs.
#' @param summarySqlFilePath SQL file path for overlap summarization.
#'
#' @return Data frame with overlap counts by source/random cohort pair.
#' @examples
#' \dontrun{
#' summarizeCohortOverlaps(
#'   connectionDetails = connectionDetails,
#'   cohort_database_schema = "results_schema",
#'   cohortTableName = "cohort",
#'   cohortIds = c(2002),
#'   randomCohortIds = c(1001)
#' )
#' }
#' @export
summarizeCohortOverlaps <- function(connectionDetails,
                                      cohort_database_schema,
                                      cohortTableName,
                                      cohortIds,
                                      randomCohortIds,
                                      summarySqlFilePath = file.path("inst", "sql", "sql_server", "summarize_cohort_overlaps.sql")) {
  stopifnot(is.character(cohort_database_schema), nzchar(cohort_database_schema))
  stopifnot(is.character(cohortTableName), nzchar(cohortTableName))
  stopifnot(length(cohortIds) > 0)
  stopifnot(length(randomCohortIds) > 0)
  stopifnot(is.character(summarySqlFilePath), length(summarySqlFilePath) == 1, nzchar(summarySqlFilePath))

  cohortIds <- unique(as.integer(cohortIds))
  randomCohortIds <- unique(as.integer(randomCohortIds))
  cohortIds_sql <- paste(cohortIds, collapse = ",")
  randomCohortIds_sql <- paste(randomCohortIds, collapse = ",")

  resolvedSummarySqlFilePath <- resolveSqlFilePath(summarySqlFilePath)
  if (!file.exists(resolvedSummarySqlFilePath)) {
    stop(paste0("Summary SQL file not found: ", resolvedSummarySqlFilePath))
  }

  sql <- paste(readLines(resolvedSummarySqlFilePath, warn = FALSE), collapse = "\n")

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  aggregatedSummary <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cohort_database_schema = cohort_database_schema,
    cohort_table_name = cohortTableName,
    cohortIds_sql = cohortIds_sql,
    random_cohort_ids_sql = randomCohortIds_sql,
    snakeCaseToCamelCase = TRUE
  )

  pairGrid <- expand.grid(
    sourceCohortDefinitionId = cohortIds,
    randomCohortDefinitionId = randomCohortIds,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  if (nrow(aggregatedSummary) == 0) {
    pairGrid$overlapSubjectRows <- 0L
    pairGrid$rowsAlignedToSourceDates <- 0L
    pairGrid$rowsNotAlignedToSourceDates <- 0L
    return(pairGrid)
  }

  mergedSummary <- merge(
    pairGrid,
    aggregatedSummary,
    by = c("sourceCohortDefinitionId", "randomCohortDefinitionId"),
    all.x = TRUE,
    sort = TRUE
  )

  for (columnName in c("overlapSubjectRows", "rowsAlignedToSourceDates", "rowsNotAlignedToSourceDates")) {
    mergedSummary[[columnName]][is.na(mergedSummary[[columnName]])] <- 0L
    mergedSummary[[columnName]] <- as.integer(mergedSummary[[columnName]])
  }

  mergedSummary
}

#' Summarize overlap counts for a tibble/data.frame of cohort pairs
#'
#' @param connectionDetails DatabaseConnector connection details.
#' @param cohort_database_schema Cohort schema.
#' @param cohortTableName Cohort table name.
#' @param cohortPairs Data frame with `prevCohort_id` and `randCohortId`.
#' @param summarySqlFilePath SQL file path for overlap summarization.
#'
#' @return Data frame with overlap counts merged to input cohort pairs.
#' @examples
#' \dontrun{
#' pairs <- data.frame(prevCohort_id = 2002, randCohortId = 1001)
#' summarizeCohortOverlaps_from_tibble(
#'   connectionDetails = connectionDetails,
#'   cohort_database_schema = "results_schema",
#'   cohortTableName = "cohort",
#'   cohortPairs = pairs
#' )
#' }
#' @export
summarizeCohortOverlapsFromTibble <- function(connectionDetails,
                                                  cohort_database_schema,
                                                  cohortTableName,
                                                  cohortPairs,
                                                  summarySqlFilePath = file.path("inst", "sql", "sql_server", "summarize_cohort_overlaps.sql")) {
  stopifnot(is.data.frame(cohortPairs))

  pairData <- as.data.frame(cohortPairs, stringsAsFactors = FALSE)
  requiredColumns <- c("prevCohort_id", "randCohortId")
  missingColumns <- setdiff(requiredColumns, names(pairData))
  if (length(missingColumns) > 0) {
    stop(paste0("cohortPairs is missing required columns: ", paste(missingColumns, collapse = ", ")))
  }

  pairData$prevCohort_id <- as.integer(pairData$prevCohort_id)
  pairData$randCohortId <- as.integer(pairData$randCohortId)

  pairData <- pairData[
    !is.na(pairData$prevCohort_id) & !is.na(pairData$randCohortId),
    ,
    drop = FALSE
  ]

  if (nrow(pairData) == 0) {
    return(pairData)
  }

  cohortIds <- unique(pairData$prevCohort_id)
  randomCohortIds <- unique(pairData$randCohortId)

  summaryData <- summarizeCohortOverlaps(
    connectionDetails = connectionDetails,
    cohort_database_schema = cohort_database_schema,
    cohortTableName = cohortTableName,
    cohortIds = cohortIds,
    randomCohortIds = randomCohortIds,
    summarySqlFilePath = summarySqlFilePath
  )

  names(summaryData)[names(summaryData) == "sourceCohortDefinitionId"] <- "prevCohort_id"
  names(summaryData)[names(summaryData) == "randomCohortDefinitionId"] <- "randCohortId"

  associatedResults <- merge(
    pairData,
    summaryData,
    by = c("prevCohort_id", "randCohortId"),
    all.x = TRUE,
    sort = FALSE
  )

  for (columnName in c("overlapSubjectRows", "rowsAlignedToSourceDates", "rowsNotAlignedToSourceDates")) {
    associatedResults[[columnName]][is.na(associatedResults[[columnName]])] <- 0L
    associatedResults[[columnName]] <- as.integer(associatedResults[[columnName]])
  }

  associatedResults
}

#' Build cohort from OMOP visit_occurrence
#'
#' @param database Optional database settings object/list. When provided, missing
#' connection/schema/table inputs are resolved from fields such as
#' `connectionDetails`, `cdmDatabaseSchema`, `cohortDatabaseSchema` or
#' `workDatabaseSchema`, and `cohortTable`.
#' @param connectionDetails A DatabaseConnector connectionDetails object.
#' @param cdm_database_schema OMOP CDM schema containing visit_occurrence.
#' @param cohort_database_schema Schema containing the cohort tables.
#' @param cohortTableName Cohort table name (without schema).
#' @param cohort_definition_id Numeric cohort ID to write.
#' @param sourceCohortDefinitionId Cohort ID in WebAPI to pull and generate.
#' @param webapiBaseUrl Base URL of WebAPI (e.g. https://host/WebAPI).
#' @param randomSampleFromCohortDefinitionId Optional cohort ID used to constrain the random sample pool. When provided, this cohort is fetched from WebAPI and generated at runtime before random sampling; a sampled visit must match both a subject and cohort start date in this pool. When `NULL`, sampling is unrestricted.
#' @param sample_size Number of subjects to sample (default 2,000,000).
#' @param min_visit_start_date Minimum visit start date in MM/DD/YYYY format.
#' @param max_visit_end_date Maximum visit end date in MM/DD/YYYY format.
#' @param min_age Minimum age at visit date.
#' @param max_age Maximum age at visit date.
#' @param cohortName Name used in the CohortGenerator definition set.
#' @param sourceCohortName Name used for the generated source cohort.
#' @param seedLibraryPath Explicit seed concept set library path to carry with this cohort build run for downstream rubric concept-set initialization.
#' @param sqlFilePath Path to parameterized SQL file.
#' @param validationSqlFilePath Path to SQL file for overwrite-date validation.
#'
#' @details
#' SQL is rendered with SqlRender parameters and translated to Spark SQL.
#' CohortGenerator is used to:
#' 1) Generate the source cohort pulled from WebAPI.
#' 2) Generate the random cohort and then overwrite start/end dates for overlapping
#'    subjects using dates from the source cohort.
#'
#' @return List with source generation result, random generation result, and overwrite validation.
#' @examples
#' \dontrun{
#' # Option 1: explicit args
#' out <- buildRandomOmopCohort(
#'   connectionDetails = connectionDetails,
#'   cdm_database_schema = "catalog.cdm_schema",
#'   cohort_database_schema = "catalog.results_schema",
#'   cohortTableName = "cohort",
#'   cohort_definition_id = 1001,
#'   sourceCohortDefinitionId = 2002,
#'   randomSampleFromCohortDefinitionId = 3003,
#'   webapiBaseUrl = "https://your-ohdsi-server/WebAPI",
#'   seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
#' )
#'
#' # Option 2: unified database object + required cohort inputs
#' out2 <- buildRandomOmopCohort(
#'   database = CCAE_OHDA_DBSpec,
#'   sourceCohortDefinitionId = 2002,
#'   webapiBaseUrl = "https://your-ohdsi-server/WebAPI"
#' )
#' }
#' @export
buildRandomOmopCohort <- function(database = NULL,
                                     connectionDetails = NULL,
                                     cdm_database_schema = NULL,
                                     cohort_database_schema = NULL,
                                     cohortTableName = NULL,
                                     cohort_definition_id = 1,
                                     sourceCohortDefinitionId,
                                     webapiBaseUrl,
                                     randomSampleFromCohortDefinitionId = NULL,
                                     sample_size = 2000000L,
                                     min_visit_start_date = "01/01/2000",
                                     max_visit_end_date = "12/31/2026",
                                     min_age = 0,
                                     max_age = 120,
                                     cohortName = "random_visit_sample",
                                     sourceCohortName = "source_webapi_cohort",
                                     seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
                                     sqlFilePath = file.path("inst", "sql", "sql_server", "build_random_omop_cohort.sql"),
                                     validationSqlFilePath = file.path("inst", "sql", "sql_server", "validate_overwrite_dates.sql")) {
  if (!is.null(database)) {
    if (is.null(connectionDetails)) {
      connectionDetails <- database$connectionDetails
    }

    if (is.null(cdm_database_schema)) {
      cdm_database_schema <- database$cdmDatabaseSchema
    }

    if (is.null(cohort_database_schema)) {
      if (!is.null(database$cohortDatabaseSchema)) {
        cohort_database_schema <- database$cohortDatabaseSchema
      } else {
        cohort_database_schema <- database$workDatabaseSchema
      }
    }

    if (is.null(cohortTableName) && !is.null(database$cohortTable)) {
      cohortTableName <- database$cohortTable
    }
  }

  cdm_database_schema <- if (is.null(cdm_database_schema)) NA_character_ else as.character(cdm_database_schema[[1]])
  cohort_database_schema <- if (is.null(cohort_database_schema)) NA_character_ else as.character(cohort_database_schema[[1]])
  cohortTableName <- if (is.null(cohortTableName)) NA_character_ else as.character(cohortTableName[[1]])

  message(paste0(
    "Working on ", sourceCohortName, " (source cohort ", sourceCohortDefinitionId,
    ") -> ", cohortName, " (random cohort ", cohort_definition_id, ") in ", cdm_database_schema, " ...."
  ))

  stopifnot(!is.null(connectionDetails))
  stopifnot(is.character(cdm_database_schema), nzchar(cdm_database_schema))
  stopifnot(is.character(cohort_database_schema), nzchar(cohort_database_schema))
  stopifnot(is.character(cohortTableName), nzchar(cohortTableName))
  stopifnot(is.numeric(cohort_definition_id), length(cohort_definition_id) == 1)
  stopifnot(is.numeric(sourceCohortDefinitionId), length(sourceCohortDefinitionId) == 1)
  stopifnot(is.character(webapiBaseUrl), length(webapiBaseUrl) == 1, nzchar(webapiBaseUrl))
  stopifnot(is.null(randomSampleFromCohortDefinitionId) || (is.numeric(randomSampleFromCohortDefinitionId) && length(randomSampleFromCohortDefinitionId) == 1))
  stopifnot(is.numeric(sample_size), length(sample_size) == 1, sample_size > 0)
  stopifnot(is.character(min_visit_start_date), length(min_visit_start_date) == 1, !is.na(as.Date(min_visit_start_date, format = "%m/%d/%Y")))
  stopifnot(is.character(max_visit_end_date), length(max_visit_end_date) == 1, !is.na(as.Date(max_visit_end_date, format = "%m/%d/%Y")))
  stopifnot(is.numeric(min_age), length(min_age) == 1)
  stopifnot(is.numeric(max_age), length(max_age) == 1)
  stopifnot(min_age <= max_age)
  stopifnot(is.character(cohortName), nzchar(cohortName))
  stopifnot(is.character(sourceCohortName), nzchar(sourceCohortName))
  stopifnot(is.character(seedLibraryPath), length(seedLibraryPath) == 1, nzchar(seedLibraryPath))
  stopifnot(is.character(sqlFilePath), length(sqlFilePath) == 1, nzchar(sqlFilePath))
  stopifnot(is.character(validationSqlFilePath), length(validationSqlFilePath) == 1, nzchar(validationSqlFilePath))

  if (!file.exists(seedLibraryPath)) {
    warning(
      "seedLibraryPath does not exist: ",
      seedLibraryPath,
      ". Continuing cohort build; concept-set seeding can be handled downstream."
    )
  } else {
    message(paste0("Using explicit concept set seed library path: ", seedLibraryPath))
  }

  resolvedSqlFilePath <- resolveSqlFilePath(sqlFilePath)
  if (!file.exists(resolvedSqlFilePath)) {
    stop(paste0("SQL file not found: ", resolvedSqlFilePath))
  }
  message(paste0("Main cohort SQL file resolved: ", resolvedSqlFilePath))

  resolvedValidationSqlFilePath <- resolveSqlFilePath(validationSqlFilePath)
  if (!file.exists(resolvedValidationSqlFilePath)) {
    stop(paste0("Validation SQL file not found: ", resolvedValidationSqlFilePath))
  }
  message(paste0("Validation SQL file resolved: ", resolvedValidationSqlFilePath))

  parameterizedSql <- paste(readLines(resolvedSqlFilePath, warn = FALSE), collapse = "\n")
  message("Loaded parameterized SQL for random cohort.")

  message(paste0("Pulling source cohort definition from WebAPI (cohortId=", as.integer(sourceCohortDefinitionId), ")."))
  sourceCohortDefinition <- ROhdsiWebApi::getCohortDefinition(
    cohortId = as.integer(sourceCohortDefinitionId),
    baseUrl = webapiBaseUrl
  )

  message("Requesting source cohort SQL from WebAPI.")
  sourceSql <- ROhdsiWebApi::getCohortSql(
    cohortDefinition = sourceCohortDefinition,
    baseUrl = webapiBaseUrl,
    generateStats = FALSE
  )

  if (is.list(sourceSql) && !is.null(sourceSql$sql)) {
    sourceSql <- sourceSql$sql
  }
  sourceSql <- as.character(sourceSql)
  message("Source cohort SQL retrieved.")

  randomSampleFilterId <- if (is.null(randomSampleFromCohortDefinitionId)) -1L else as.integer(randomSampleFromCohortDefinitionId)
  randomSampleCohortDefinitionSet <- NULL

  if (randomSampleFilterId == -1L) {
    message("Random sample pool: unrestricted (all eligible persons).")
  } else if (randomSampleFilterId == as.integer(sourceCohortDefinitionId)) {
    message(paste0("Random sample pool cohort is the same as source cohort (", randomSampleFilterId, "); source cohort generation will be reused."))
  } else {
    message(paste0("Pulling random sample pool cohort definition from WebAPI (cohortId=", randomSampleFilterId, ")."))
    randomSampleCohortDefinition <- ROhdsiWebApi::getCohortDefinition(
      cohortId = randomSampleFilterId,
      baseUrl = webapiBaseUrl
    )

    message("Requesting random sample pool cohort SQL from WebAPI.")
    randomSampleSql <- ROhdsiWebApi::getCohortSql(
      cohortDefinition = randomSampleCohortDefinition,
      baseUrl = webapiBaseUrl,
      generateStats = FALSE
    )

    if (is.list(randomSampleSql) && !is.null(randomSampleSql$sql)) {
      randomSampleSql <- randomSampleSql$sql
    }
    randomSampleSql <- as.character(randomSampleSql)
    message("Random sample pool cohort SQL retrieved.")

    randomSampleCohortDefinitionSet <- data.frame(
      cohortId = randomSampleFilterId,
      cohortName = paste0("random_sample_pool_cohort_", randomSampleFilterId),
      sql = randomSampleSql,
      stringsAsFactors = FALSE
    )
  }

  message("Rendering random cohort SQL.")
  if (randomSampleFilterId != -1L) {
    message(paste0("Random sample pool restricted to cohort_definition_id=", randomSampleFilterId, "."))
  }

  renderedSql <- SqlRender::render(
    sql = parameterizedSql,
    cdm_database_schema = cdm_database_schema,
    cohort_database_schema = cohort_database_schema,
    cohort_table_name = cohortTableName,
    cohort_definition_id = as.integer(cohort_definition_id),
    source_cohort_definition_id = as.integer(sourceCohortDefinitionId),
    random_sample_from_cohort_definition_id = randomSampleFilterId,
    sample_size = as.integer(sample_size),
    min_visit_start_date = format(as.Date(min_visit_start_date, format = "%m/%d/%Y"), "%Y-%m-%d"),
    max_visit_end_date = format(as.Date(max_visit_end_date, format = "%m/%d/%Y"), "%Y-%m-%d"),
    min_age = as.integer(min_age),
    max_age = as.integer(max_age)
  )

  message("Random cohort SQL rendered; CohortGenerator translates it to the target dialect.")

  sourceCohortDefinitionSet <- data.frame(
    cohortId = as.integer(sourceCohortDefinitionId),
    cohortName = sourceCohortName,
    sql = sourceSql,
    stringsAsFactors = FALSE
  )

  randomCohortDefinitionSet <- data.frame(
    cohortId = as.integer(cohort_definition_id),
    cohortName = cohortName,
    sql = renderedSql,
    stringsAsFactors = FALSE
  )

  cohortTableNames <- CohortGenerator::getCohortTableNames(
    cohortTable = cohortTableName
  )
  message("Resolved cohort table names for CohortGenerator.")

  message("Ensuring cohort tables exist (incremental create mode).")
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohort_database_schema,
    cohortTableNames = cohortTableNames,
    incremental = TRUE
  )

  message(paste0("Generating source cohort: ", sourceCohortName, "."))
  sourceGenerationResult <- CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdm_database_schema,
    cohortDatabaseSchema = cohort_database_schema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = sourceCohortDefinitionSet,
    stopOnError = TRUE,
    incremental = TRUE
  )

  randomSamplePoolGenerationResult <- NULL
  if (!is.null(randomSampleCohortDefinitionSet)) {
    message(paste0("Generating random sample pool cohort: ", randomSampleCohortDefinitionSet$cohortName[[1]], "."))
    randomSamplePoolGenerationResult <- CohortGenerator::generateCohortSet(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdm_database_schema,
      cohortDatabaseSchema = cohort_database_schema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = randomSampleCohortDefinitionSet,
      stopOnError = TRUE,
      incremental = TRUE
    )
  }

  if (randomSampleFilterId != -1L) {
    samplePoolRowCount <- getCohortRowCount(
      connectionDetails = connectionDetails,
      cohort_database_schema = cohort_database_schema,
      cohortTableName = cohortTableName,
      cohort_definition_id = randomSampleFilterId
    )

    if (is.na(samplePoolRowCount)) {
      stop(
        "Could not verify row count for random sample pool cohort_definition_id=",
        randomSampleFilterId,
        "."
      )
    }

    if (samplePoolRowCount == 0L) {
      stop(
        "Random sample pool cohort_definition_id=",
        randomSampleFilterId,
        " has zero rows in ",
        cohort_database_schema,
        ".",
        cohortTableName,
        ". Cannot generate random cohort from an empty sampling pool."
      )
    }

    message(
      "Random sample pool row count for cohort_definition_id=",
      randomSampleFilterId,
      ": ",
      samplePoolRowCount
    )
  }

  message(paste0("Generating random cohort: ", cohortName, "."))
  randomGenerationResult <- CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdm_database_schema,
    cohortDatabaseSchema = cohort_database_schema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = randomCohortDefinitionSet,
    stopOnError = TRUE,
    incremental = TRUE
  )

  cohortCounts <- ithReportCohortCounts(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohort_database_schema,
    cohortTable = cohortTableName,
    cohortIds = c(
      sourceCohortDefinitionSet$cohortId,
      if (!is.null(randomSampleCohortDefinitionSet)) randomSampleCohortDefinitionSet$cohortId,
      randomCohortDefinitionSet$cohortId
    )
  )

  message("Running overwrite-date validation query.")
  overwriteValidation <- getDateOverwriteValidation(
    connectionDetails = connectionDetails,
    validationSqlFilePath = resolvedValidationSqlFilePath,
    cohort_database_schema = cohort_database_schema,
    cohortTableName = cohortTableName,
    cohort_definition_id = as.integer(cohort_definition_id),
    sourceCohortDefinitionId = as.integer(sourceCohortDefinitionId)
  )

  if (nrow(overwriteValidation) > 0) {
    message(
      paste0(
        "Overwrite validation: overlapSubjectRows=",
        overwriteValidation$overlapSubjectRows[1],
        ", rowsAlignedToSourceDates=",
        overwriteValidation$rowsAlignedToSourceDates[1],
        ", rowsNotAlignedToSourceDates=",
        overwriteValidation$rowsNotAlignedToSourceDates[1]
      )
    )
  }

  message("Cohort workflow completed (random cohort ", cohort_definition_id, " in ", cdm_database_schema, ").")

  invisible(list(
    sourceCohortGeneration = sourceGenerationResult,
    randomSamplePoolCohortGeneration = randomSamplePoolGenerationResult,
    randomCohortGeneration = randomGenerationResult,
    cohortCounts = cohortCounts,
    overwriteValidation = overwriteValidation,
    seedLibraryPath = seedLibraryPath
  ))
}

# -----------------------------
# Example usage (Databricks):
# -----------------------------
# connectionDetails <- DatabaseConnector::createConnectionDetails(
#   dbms = "spark",
#   server = "<databricks-host>",
#   user = "token",
#   password = "<personal-access-token>",
#   pathToDriver = "<path-to-jdbc-driver>",
#   port = "443",
#   extraSettings = "httpPath=<sql-warehouse-http-path>;ssl=1;transportMode=http"
# )
#
# result <- buildRandomOmopCohort(
#   connectionDetails = connectionDetails,
#   cdm_database_schema = "catalog.cdm_schema",
#   cohort_database_schema = "catalog.results_schema",
#   cohortTableName = "random_cohort",
#   cohort_definition_id = 1001,
#   sourceCohortDefinitionId = 2002,
#   webapiBaseUrl = "https://your-ohdsi-server/WebAPI",
#   sample_size = 2000000,
#   min_visit_start_date = "01/01/2000",
#   max_visit_end_date = "12/31/2026",
#   min_age = 0,
#   max_age = 120,
#   cohortName = "2M random persons with one random visit",
#   sourceCohortName = "cohort_2002_from_webapi",
#   seedLibraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
#   sqlFilePath = file.path("inst", "sql", "sql_server", "build_random_omop_cohort.sql"),
#   validationSqlFilePath = file.path("inst", "sql", "sql_server", "validate_overwrite_dates.sql")
# )
# print(result)
#
# overlap_summary <- summarizeCohortOverlaps(
#   connectionDetails = connectionDetails,
#   cohort_database_schema = "catalog.results_schema",
#   cohortTableName = "random_cohort",
#   cohortIds = c(2002, 3003),
#   randomCohortIds = c(1001, 1002),
#   summarySqlFilePath = file.path("inst", "sql", "sql_server", "summarize_cohort_overlaps.sql")
# )
# print(overlap_summary)
#
# cohortPairs <- data.frame(
#   prevCohort_id = c(2002, 3003),
#   randCohortId = c(1001, 1002),
#   condition = c("Stevens-Johnson syndrome", "Example condition"),
#   minAge = c(0, 18),
#   maxAge = c(120, 90),
#   stringsAsFactors = FALSE
# )
#
# associated_summary <- summarizeCohortOverlaps_from_tibble(
#   connectionDetails = connectionDetails,
#   cohort_database_schema = "catalog.results_schema",
#   cohortTableName = "random_cohort",
#   cohortPairs = cohortPairs,
#   summarySqlFilePath = file.path("inst", "sql", "sql_server", "summarizeCohortOverlaps.sql")
# )
# print(associated_summary)
