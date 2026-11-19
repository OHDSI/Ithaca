ithGetSubjectData <- function(subjectsToTest,
                                 database,
                                 endDays,
                                 sqlFile = "inst/sql/sql_server/patientProfilesGroup.sql") {
  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  sqlQuery <- SqlRender::readSql(sqlFile)
  allRows <- NULL

  for (subjectUp in seq_len(nrow(subjectsToTest))) {
    sql <- SqlRender::render(
      sqlQuery,
      cdm_schema = database$cdmDatabaseSchema,
      subjectId = subjectsToTest$subjectId[[subjectUp]],
      endDays = endDays,
      startDate = paste0('"', subjectsToTest$cohortStartDate[[subjectUp]], '"')
    )

    rows <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
    if (ncol(rows) >= 3) {
      rows <- rows[, c(1:3)]
    }
    allRows <- rbind(allRows, rows)
  }

  allRows$dayInStudy <- as.integer(allRows$dayInStudy)
  colnames(allRows)[colnames(allRows) %in% c("conceptName", "dayInStudy")] <- c("Health Record Item", "Day in Study")

  allRows
}

ithGetCohortSubjectData <- function(cohortsToTest,
                                        database,
                                        endDays,
                                        sqlFile = "inst/sql/sql_server/subjectDataCohorts.sql",
                                        cohortTable = NULL) {
  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  sqlQuery <- SqlRender::readSql(sqlFile)
  tempTable <- if (is.null(cohortTable)) ithDefaultCohortTable(database) else cohortTable

  sql <- SqlRender::render(
    sqlQuery,
    cdm_schema = database$cdmDatabaseSchema,
    cohortSchema = database$workDatabaseSchema,
    cohortTable = tempTable,
    cohorts = cohortsToTest,
    endDays = endDays
  )

  DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
}

ithGetSubjectDataSingle <- function(subjectsToTest,
                                        database,
                                        endDays,
                                        excludedConditionCode = NULL,
                                        sqlFile = "inst/sql/sql_server/patientProfilesGroup.sql") {
  if (nrow(subjectsToTest) == 0) {
    return(data.frame())
  }

  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  sqlQuery <- SqlRender::readSql(sqlFile)
  excludedValues <- paste(unlist(excludedConditionCode, use.names = FALSE), collapse = ", ")

  tuples <- vapply(
    seq_len(nrow(subjectsToTest)),
    function(i) paste0("(", subjectsToTest$subjectId[[i]], ", '", subjectsToTest$cohortStartDate[[i]], "')"),
    character(1)
  )
  finalList <- paste(tuples, collapse = ",")

  sql <- SqlRender::render(
    sqlQuery,
    cdm_schema = database$cdmDatabaseSchema,
    allValues = finalList,
    endDays = endDays,
    excludedConditionCode = excludedValues
  )

  rows <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  rows$dayInStudy <- as.integer(rows$dayInStudy)
  colnames(rows)[colnames(rows) %in% c("conceptName", "dayInStudy")] <- c("Health Record Item", "Day in Study")
  rows
}

ithGetRandomSample <- function(cohort, database, count, cohortTable = NULL) {
  tempTable <- if (is.null(cohortTable)) ithDefaultCohortTable(database) else cohortTable

  sql <- paste0(
    "SELECT TOP ", count, " subject_id, cohort_start_date ",
    "FROM ", database$workDatabaseSchema, ".", tempTable,
    " WHERE cohort_definition_id IN (", cohort, ") ORDER BY NEWID();"
  )
  sql <- SqlRender::translate(sql, targetDialect = database$connectionDetails$dbms)

  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
}

ithGetSpecifiedSample <- function(cohort,
                                     database,
                                     subjectList,
                                     count = 10000000,
                                     cohortTable = NULL,
                                     seed = 123) {
  tempTable <- if (is.null(cohortTable)) ithDefaultCohortTable(database) else cohortTable

  sql <- paste0(
    "SELECT subject_id, cohort_start_date ",
    "FROM ", database$workDatabaseSchema, ".", tempTable,
    " WHERE cohort_definition_id IN (", cohort, ")",
    " AND subject_id IN (", subjectList, ")"
  )
  sql <- SqlRender::translate(sql, targetDialect = database$connectionDetails$dbms)

  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  rows <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)

  if (nrow(rows) <= count) {
    return(rows)
  }

  set.seed(seed)
  idx <- sample.int(nrow(rows), size = count, replace = FALSE)
  rows[idx, ]
}

ithGetSubjects <- function(subjects, cohorts, database, cohortTable = NULL) {
  tempTable <- if (is.null(cohortTable)) ithDefaultCohortTable(database) else cohortTable

  sql <- paste0(
    "SELECT DISTINCT subject_id, MAX(cohort_start_date) AS cohort_start_date ",
    "FROM ", database$workDatabaseSchema, ".", tempTable,
    " WHERE cohort_definition_id IN (", cohorts, ")",
    " AND subject_id IN (", subjects, ")",
    " GROUP BY subject_id"
  )
  sql <- SqlRender::translate(sql, targetDialect = database$connectionDetails$dbms)

  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
}

ithGetIngredients <- function(database,
                                conceptList,
                                sqlFile = "inst/sql/sql_server/getIngredients.sql") {
  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  sqlQuery <- SqlRender::readSql(sqlFile)
  sql <- SqlRender::render(
    sqlQuery,
    cdm_schema = database$cdmDatabaseSchema,
    conceptIds = paste(conceptList, collapse = ",")
  )

  DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
}

#' Get longitudinal subject data for selected subjects
#'
#' @param subjectsToTest Data frame with `subjectId` and `cohortStartDate`.
#' @param database Database specification.
#' @param endDays Follow-up days.
#' @param exclude Deprecated compatibility argument.
#' @param excludedConditionCode Optional excluded condition codes.
#'
#' @return Data frame of subject-level profile rows.
#' @examples
#' \dontrun{
#' subjects <- data.frame(subjectId = c(1, 2), cohortStartDate = as.Date(c("2020-01-01", "2020-02-01")))
#' rows <- getSubjectData(subjects, database = CCAE_OHDA_DBSpec, endDays = 365)
#' }
#' @export
getSubjectData <- function(subjectsToTest, database, endDays, exclude = FALSE, excludedConditionCode = NULL) {
  ithGetSubjectData(subjectsToTest = subjectsToTest, database = database, endDays = endDays)
}

#' Get profile data for all subjects in selected cohorts
#'
#' @param cohortsToTest Cohort IDs.
#' @param database Database specification.
#' @param endDays Follow-up days.
#'
#' @return Data frame of cohort subject profile rows.
#' @examples
#' \dontrun{
#' rows <- getCohortSubjectData(cohortsToTest = c(14412), database = CCAE_OHDA_DBSpec, endDays = 365)
#' }
#' @export
getCohortSubjectData <- function(cohortsToTest, database, endDays) {
  ithGetCohortSubjectData(cohortsToTest = cohortsToTest, database = database, endDays = endDays)
}

#' Get subject data with optional exclusions
#'
#' @param connection Deprecated compatibility argument.
#' @param subjectsToTest Data frame with `subjectId` and `cohortStartDate`.
#' @param database Database specification.
#' @param endDays Follow-up days.
#' @param excludedConditionCode Optional excluded condition codes.
#'
#' @return Data frame of subject-level profile rows.
#' @examples
#' \dontrun{
#' subjects <- data.frame(subjectId = 1, cohortStartDate = as.Date("2020-01-01"))
#' rows <- getSubjectDataSingle(NULL, subjects, database = CCAE_OHDA_DBSpec, endDays = 365)
#' }
#' @export
getSubjectDataSingle <- function(connection, subjectsToTest, database, endDays, excludedConditionCode = "") {
  ithGetSubjectDataSingle(
    subjectsToTest = subjectsToTest,
    database = database,
    endDays = endDays,
    excludedConditionCode = excludedConditionCode
  )
}

#' Draw a random sample of cohort subjects
#'
#' @param cohort Cohort ID(s).
#' @param database Database specification.
#' @param count Sample size.
#'
#' @return Data frame with sampled subject IDs and cohort start dates.
#' @examples
#' \dontrun{
#' sample <- getRandomSample(cohort = 14412, database = CCAE_OHDA_DBSpec, count = 100)
#' }
#' @export
getRandomSample <- function(cohort, database, count) {
  ithGetRandomSample(cohort = cohort, database = database, count = count)
}

#' Get and optionally down-sample specified subjects from a cohort
#'
#' @param cohort Cohort ID(s).
#' @param database Database specification.
#' @param subjectList Subject IDs.
#' @param count Maximum returned row count.
#'
#' @return Data frame with subject IDs and cohort start dates.
#' @examples
#' \dontrun{
#' sample <- getSpecifiedSample(
#'   cohort = 14412,
#'   database = CCAE_OHDA_DBSpec,
#'   subjectList = "1,2,3",
#'   count = 2
#' )
#' }
#' @export
getSpecifiedSample <- function(cohort, database, subjectList, count = 10000000) {
  ithGetSpecifiedSample(cohort = cohort, database = database, subjectList = subjectList, count = count)
}

#' Get subject rows for specific cohort IDs and subjects
#'
#' @param subjects Subject IDs.
#' @param cohorts Cohort IDs.
#' @param database Database specification.
#'
#' @return Data frame with subject IDs and cohort start dates.
#' @examples
#' \dontrun{
#' rows <- getSubjects(subjects = "1,2", cohorts = "14412", database = CCAE_OHDA_DBSpec)
#' }
#' @export
getSubjects <- function(subjects, cohorts, database) {
  ithGetSubjects(subjects = subjects, cohorts = cohorts, database = database)
}

#' Get ingredient concepts for concept IDs
#'
#' @param database Database specification.
#' @param conceptList Concept IDs.
#'
#' @return Data frame of ingredient concept mappings.
#' @examples
#' \dontrun{
#' ingredients <- getIngredients(database = CCAE_OHDA_DBSpec, conceptList = c(1125315, 1310149))
#' }
#' @export
getIngredients <- function(database, conceptList) {
  ithGetIngredients(database = database, conceptList = conceptList)
}
