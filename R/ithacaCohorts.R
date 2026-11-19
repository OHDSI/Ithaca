ithDefaultCohortTable <- function(database) {
  if (!is.null(database$cohortTable) &&
      length(database$cohortTable) > 0L &&
      nzchar(as.character(database$cohortTable[[1]]))) {
    return(as.character(database$cohortTable[[1]]))
  }

  gsub("[.]", "_", paste0("pv_table_", database$cdmDatabaseSchema))
}

#' Build cohort definitions from OHDSI WebAPI
#'
#' @param cohortIds Integer cohort IDs to fetch from WebAPI.
#' @param webapiBaseUrl Base URL for OHDSI WebAPI.
#' @param cohortNames Optional cohort names.
#' @param generateStats Logical; request generated stats from WebAPI export.
#'
#' @return Data frame with `cohortId`, `cohortName`, and `sql`.
#' @examples
#' \dontrun{
#' defs <- ithGetWebapiCohortDefinitionSet(
#'   cohortIds = c(14412),
#'   webapiBaseUrl = "https://your-ohdsi-server/WebAPI"
#' )
#' }
#' @export
ithGetWebapiCohortDefinitionSet <- function(cohortIds,
                                                 webapiBaseUrl,
                                                 cohortNames = NULL,
                                                 generateStats = FALSE,
                                                 verbose = TRUE) {
  stopifnot(length(cohortIds) > 0)
  stopifnot(is.character(webapiBaseUrl), length(webapiBaseUrl) == 1, nzchar(webapiBaseUrl))

  ithMaybeMessage(verbose, "Fetching ", length(unique(cohortIds)), " cohort definition(s) from WebAPI...")

  exported <- ROhdsiWebApi::exportCohortDefinitionSet(
    baseUrl = webapiBaseUrl,
    cohortIds = as.integer(unique(cohortIds)),
    generateStats = isTRUE(generateStats)
  )

  if (!all(c("cohortId", "cohortName", "sql") %in% names(exported))) {
    stop("WebAPI cohort export did not return required columns: cohortId, cohortName, sql")
  }

  definitionSet <- exported[, c("cohortId", "cohortName", "sql")]

  if (!is.null(cohortNames)) {
    if (length(cohortNames) != nrow(definitionSet)) {
      stop("cohortNames must have the same length as fetched cohort definitions")
    }
    definitionSet$cohortName <- as.character(cohortNames)
  }

  ithMaybeMessage(verbose, "Retrieved cohort definitions from WebAPI.")

  definitionSet
}

#' Create a CohortGenerator cohort definition set
#'
#' @param cohortIds Integer cohort IDs.
#' @param cohortSql OHDSI SQL statements for cohorts.
#' @param cohortNames Optional cohort names.
#'
#' @return Data frame with `cohortId`, `cohortName`, and `sql`.
#' @examples
#' defs <- ithMakeCohortDefinitionSet(
#'   cohortIds = c(1, 2),
#'   cohortSql = c("SELECT 1 AS subject_id", "SELECT 2 AS subject_id"),
#'   cohortNames = c("cohort_one", "cohort_two")
#' )
#' @export
ithMakeCohortDefinitionSet <- function(cohortIds, cohortSql, cohortNames = NULL) {
  if (length(cohortIds) != length(cohortSql)) {
    stop("cohortIds and cohortSql must have the same length")
  }

  if (is.null(cohortNames)) {
    cohortNames <- paste0("cohort_", cohortIds)
  }

  data.frame(
    cohortId = as.integer(cohortIds),
    cohortName = as.character(cohortNames),
    sql = as.character(cohortSql),
    stringsAsFactors = FALSE
  )
}

# Counts distinct subjects (and rows) per cohort id after generation. Reporting only: a failure warns instead of stopping the workflow.
ithReportCohortCounts <- function(connectionDetails,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  cohortIds,
                                  label = NULL,
                                  verbose = TRUE) {
  cohortIds <- sort(unique(as.integer(cohortIds)))
  cohortIds <- cohortIds[!is.na(cohortIds)]
  if (length(cohortIds) == 0L) {
    return(invisible(NULL))
  }

  counts <- tryCatch({
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
    found <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT cohort_definition_id, COUNT(*) AS row_count, COUNT(DISTINCT subject_id) AS subject_count FROM @schema.@table ",
        "WHERE cohort_definition_id IN (@ids) GROUP BY cohort_definition_id"
      ),
      schema = cohortDatabaseSchema,
      table = cohortTable,
      ids = paste(cohortIds, collapse = ","),
      snakeCaseToCamelCase = TRUE
    )
    out <- data.frame(cohortId = cohortIds, rowCount = 0L, subjectCount = 0L)
    hit <- match(as.integer(found$cohortDefinitionId), out$cohortId)
    out$rowCount[hit] <- as.integer(found$rowCount)
    out$subjectCount[hit] <- as.integer(found$subjectCount)
    out
  }, error = function(e) {
    warning("Could not count generated cohorts: ", conditionMessage(e), call. = FALSE)
    NULL
  })

  if (!is.null(counts) && isTRUE(verbose)) {
    ithMessageCohortCounts(counts, label)
  }
  invisible(counts)
}

ithMessageCohortCounts <- function(counts, label = NULL) {
  prefix <- if (is.null(label)) "" else paste0(" [", label, "]")
  for (i in seq_len(nrow(counts))) {
    message(
      "Generated cohort", prefix, ": cohortId=", counts$cohortId[[i]],
      ", subjects=", counts$subjectCount[[i]], " (cohort rows=", counts$rowCount[[i]], ")"
    )
  }
}

# Creating the same cohort table from several workers at once fails (table already exists), so
# parallel runs create every table once in this session first.
ithCreateCohortTables <- function(database, cohortTable, incremental = TRUE, cohortDatabaseSchema = database$workDatabaseSchema) {
  CohortGenerator::createCohortTables(
    connectionDetails = database$connectionDetails,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTable),
    cohortDatabaseSchema = cohortDatabaseSchema,
    incremental = incremental
  )
}

# Generates one job's cohort definitions in its database. Used in-process and on workers; returns
# list(ok, value, error, seconds) instead of raising.
ithRunCohortGenerationJob <- function(task) {
  startedAt <- Sys.time()
  database <- task$database
  outcome <- tryCatch({
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = task$cohortTable)
    if (isTRUE(task$createTables)) {
      ithCreateCohortTables(database, task$cohortTable, task$incremental)
    }
    generation <- CohortGenerator::generateCohortSet(
      connectionDetails = database$connectionDetails,
      cdmDatabaseSchema = database$cdmDatabaseSchema,
      cohortDatabaseSchema = database$workDatabaseSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = task$cohortDefinitionSet,
      incremental = task$incremental,
      stopOnError = task$stopOnError
    )
    counts <- ithReportCohortCounts(
      connectionDetails = database$connectionDetails,
      cohortDatabaseSchema = database$workDatabaseSchema,
      cohortTable = task$cohortTable,
      cohortIds = task$cohortDefinitionSet$cohortId,
      verbose = FALSE
    )
    list(ok = TRUE, value = list(generation = generation, counts = counts), error = NULL)
  }, error = function(e) {
    list(ok = FALSE, value = NULL, error = conditionMessage(e))
  })
  outcome$seconds <- as.numeric(difftime(Sys.time(), startedAt, units = "secs"))
  outcome$databaseId <- database$databaseId
  outcome$cohortIds <- task$cohortDefinitionSet$cohortId
  outcome
}

# Definition sets with these columns have cohorts that depend on others in the set, so they are
# generated together.
ithCohortSetHasDependencies <- function(cohortDefinitionSet) {
  any(c("isSubset", "subsetParent", "subsetDefinitionId") %in% names(cohortDefinitionSet))
}

# One task per job, or with more than one worker one task per cohort in the job's definition set.
ithCohortGenerationTasks <- function(jobs, workers, stopOnError, incremental) {
  tasks <- list()
  for (job in jobs) {
    definitions <- job$cohortDefinitionSet
    parts <- if (workers > 1L && nrow(definitions) > 1L && !ithCohortSetHasDependencies(definitions)) {
      lapply(seq_len(nrow(definitions)), function(row) definitions[row, , drop = FALSE])
    } else {
      list(definitions)
    }
    for (part in parts) {
      tasks[[length(tasks) + 1L]] <- list(
        database = job$database,
        cohortDefinitionSet = part,
        cohortTable = if (is.null(job$cohortTable)) ithDefaultCohortTable(job$database) else job$cohortTable,
        stopOnError = stopOnError,
        incremental = incremental
      )
    }
  }
  tasks
}

# Generates cohorts for a list of jobs, each list(database, cohortDefinitionSet, cohortTable). With
# more than one worker each job is split into one task per cohort, so many cohorts in one database
# also run in parallel; they write different cohort IDs to the same table. A failed task raises an
# error after the others finish, or, with stopOnFailure = FALSE, is returned in the outcomes
# (list(ok, value, error, seconds, databaseId, cohortIds) per task) for the caller to handle.
ithGenerateCohortJobs <- function(jobs,
                                  workers = 1L,
                                  stopOnError = TRUE,
                                  incremental = TRUE,
                                  createTables = TRUE,
                                  stopOnFailure = TRUE,
                                  verbose = TRUE) {
  requestedWorkers <- max(1L, as.integer(workers))
  tasks <- ithCohortGenerationTasks(jobs, requestedWorkers, stopOnError, incremental)
  workers <- min(requestedWorkers, max(1L, length(tasks)))
  for (index in seq_along(tasks)) {
    tasks[[index]]$createTables <- createTables && workers <= 1L
  }

  if (createTables && workers > 1L) {
    created <- character(0)
    for (task in tasks) {
      key <- paste(task$database$databaseId, task$database$workDatabaseSchema, task$cohortTable)
      if (key %in% created) next
      ithMaybeMessage(verbose, "Ensuring cohort tables exist in schema: ", task$database$workDatabaseSchema)
      ithCreateCohortTables(task$database, task$cohortTable, incremental)
      created <- c(created, key)
    }
  }

  describe <- function(task) {
    paste0("database ", task$database$databaseId, " (cohort ", paste(task$cohortDefinitionSet$cohortId, collapse = ", "), ")")
  }
  outcomes <- ithRunJobs(
    tasks, ithRunCohortGenerationJob, workers = workers,
    onStart = function(index) {
      if (length(tasks) == length(jobs)) {
        ithMaybeMessage(verbose, "Generating cohorts for database: ", tasks[[index]]$database$databaseId)
      } else {
        ithMaybeMessage(verbose, "Generating cohort for ", describe(tasks[[index]]))
      }
    },
    onFinish = function(index, outcome) {
      databaseId <- tasks[[index]]$database$databaseId
      if (isTRUE(outcome$ok)) {
        if (isTRUE(verbose) && !is.null(outcome$value$counts)) {
          ithMessageCohortCounts(outcome$value$counts, databaseId)
        }
        ithMaybeMessage(verbose, "Completed cohort generation for ", describe(tasks[[index]]))
      } else {
        ithMaybeMessage(verbose, "Cohort generation failed for ", describe(tasks[[index]]), " - ", outcome$error)
      }
    },
    stopOnFirstError = stopOnFailure
  )

  failed <- which(vapply(outcomes, function(outcome) !is.null(outcome) && !isTRUE(outcome$ok), logical(1)))
  if (isTRUE(stopOnFailure) && length(failed) > 0L) {
    stop(
      paste(vapply(failed, function(index) {
        paste0("Cohort generation failed for ", describe(tasks[[index]]), ": ", outcomes[[index]]$error)
      }, character(1)), collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(outcomes)
}

ithGenerateCohorts <- function(cohortDefinitionSet,
                                 dbList,
                                 cohortIds = NULL,
                                 webapiBaseUrl = NULL,
                                 cohortTable = NULL,
                                 stopOnError = TRUE,
                                 incremental = TRUE,
                                 createTables = TRUE,
                                 workers = 1L,
                                 verbose = TRUE) {
  ithMaybeMessage(verbose, "Starting cohort generation workflow...")
  if (is.null(cohortDefinitionSet)) {
    if (is.null(cohortIds) || is.null(webapiBaseUrl)) {
      stop("Provide cohortDefinitionSet or provide both cohortIds and webapiBaseUrl")
    }

    cohortDefinitionSet <- ithGetWebapiCohortDefinitionSet(
      cohortIds = cohortIds,
      webapiBaseUrl = webapiBaseUrl,
      verbose = verbose
    )
  }

  if (!all(c("cohortId", "cohortName", "sql") %in% names(cohortDefinitionSet))) {
    stop("cohortDefinitionSet must contain cohortId, cohortName, and sql columns")
  }

  jobs <- lapply(dbList, function(database) {
    list(database = database, cohortDefinitionSet = cohortDefinitionSet, cohortTable = cohortTable)
  })
  ithGenerateCohortJobs(
    jobs,
    workers = workers,
    stopOnError = stopOnError,
    incremental = incremental,
    createTables = createTables,
    verbose = verbose
  )

  ithMaybeMessage(verbose, "Cohort generation workflow completed.")
}

ithGetCohortSubjects <- function(cohorts, database, cohortTable = NULL, verbose = TRUE) {
  ithMaybeMessage(verbose, "Fetching cohort subjects for ", length(cohorts), " cohort(s) from database: ", database$databaseId)
  tableToUse <- if (is.null(cohortTable)) ithDefaultCohortTable(database) else cohortTable

  sql <- paste0(
    "SELECT * FROM ", database$workDatabaseSchema, ".", tableToUse,
    " WHERE cohort_definition_id IN (", paste(cohorts, collapse = ","), ")"
  )

  sql <- SqlRender::translate(sql, targetDialect = database$connectionDetails$dbms)
  conn <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
}

ithCountCohortRows <- function(cohortIds, database, cohortTable = NULL) {
  tableToUse <- if (is.null(cohortTable)) ithDefaultCohortTable(database) else cohortTable
  sql <- SqlRender::translate(
    paste0(
      "SELECT cohort_definition_id, COUNT(*) AS row_count FROM ", database$workDatabaseSchema, ".", tableToUse,
      " WHERE cohort_definition_id IN (", paste(cohortIds, collapse = ","), ") GROUP BY cohort_definition_id"
    ),
    targetDialect = database$connectionDetails$dbms
  )
  conn <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  counts <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  stats::setNames(as.numeric(counts$rowCount), as.character(counts$cohortDefinitionId))
}

# Stops before any rule is scored when a condition's random cohort has no rows (or its cohort
# table cannot be read) in a database; otherwise every rule would silently score zero subjects.
ithCheckRandomCohortsReadyForScoring <- function(conditionSpecs, databases, cohortTables, countRows = ithCountCohortRows) {
  problems <- character(0)
  for (dbUp in seq_along(databases)) {
    database <- databases[[dbUp]]
    tableToUse <- if (is.null(cohortTables[[dbUp]])) ithDefaultCohortTable(database) else cohortTables[[dbUp]]
    cohortIds <- unique(as.integer(vapply(conditionSpecs, function(x) suppressWarnings(as.numeric(x$randomCohortDefinitionId[[1]])), numeric(1))))
    counts <- tryCatch(
      countRows(cohortIds, database, cohortTable = tableToUse),
      error = function(e) structure(conditionMessage(e), class = "cohortCountError")
    )
    if (inherits(counts, "cohortCountError")) {
      problems <- c(problems, paste0(
        database$databaseId, ": could not read cohort table ", database$workDatabaseSchema, ".", tableToUse, " (", counts, ")"
      ))
      next
    }
    for (condition in conditionSpecs) {
      cohortId <- as.character(as.integer(condition$randomCohortDefinitionId[[1]]))
      if (is.na(counts[cohortId]) || counts[cohortId] == 0) {
        problems <- c(problems, paste0(
          as.character(condition$rubricName[[1]]), " on ", database$databaseId, ": random cohort ", cohortId,
          " has no rows in ", database$workDatabaseSchema, ".", tableToUse
        ))
      }
    }
  }
  if (length(problems) > 0L) {
    stop(
      "Random cohorts are not ready for scoring:\n  ", paste(problems, collapse = "\n  "),
      "\nBuild them with buildRandomOmopCohortFromConfig() (or score with generateCohorts = TRUE) and score again. ",
      "Use the conditionSpecs 'ignore' column to score the other conditions now.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Generate cohorts in one or more databases
#'
#' Uses CohortGenerator conventions:
#' `getCohortTableNames()`, `createCohortTables()`, and `generateCohortSet()`.
#'
#' @param cohortDefinitionSet Cohort definitions with `cohortId`, `cohortName`, and `sql`.
#' If `NULL`, definitions are fetched from WebAPI using `cohortIds` and `webapiBaseUrl`.
#' @param dbList List of database specifications.
#' @param cohortIds Optional cohort IDs to fetch from WebAPI.
#' @param webapiBaseUrl Optional OHDSI WebAPI base URL used to fetch cohort definitions.
#' @param cohortTable Optional cohort table name.
#' @param stopOnError Logical; stop on first cohort generation error.
#' @param incremental Logical; generate incrementally.
#' @param createTables Logical; create cohort tables before generation.
#' @param workers Number of parallel worker processes (1, the default, runs one at a time in this
#' session). With more than one, each cohort in each database is generated as its own task, so many
#' cohorts in one database also run in parallel (a definition set with subset cohorts is kept
#' together). Each worker opens its own database connection.
#'
#' @return Invisibly returns generation results from CohortGenerator.
#' @examples
#' \dontrun{
#' defs <- ithMakeCohortDefinitionSet(
#'   cohortIds = c(14412),
#'   cohortSql = c("SELECT 1 AS subject_id")
#' )
#' generateCohorts(defs, dbList = list(CCAE_OHDA_DBSpec), incremental = TRUE)
#' }
#' @export
generateCohorts <- function(cohortDefinitionSet,
                            dbList,
                            cohortIds = NULL,
                            webapiBaseUrl = NULL,
                            cohortTable = NULL,
                            stopOnError = TRUE,
                            incremental = TRUE,
                            createTables = TRUE,
                            workers = 1L,
                            verbose = TRUE) {
  ithGenerateCohorts(
    cohortDefinitionSet = cohortDefinitionSet,
    dbList = dbList,
    cohortIds = cohortIds,
    webapiBaseUrl = webapiBaseUrl,
    cohortTable = cohortTable,
    stopOnError = stopOnError,
    incremental = incremental,
    createTables = createTables,
    workers = workers,
    verbose = verbose
  )
}

#' Get generated cohort subject rows
#'
#' @param cohorts Cohort IDs.
#' @param database Database specification.
#' @param cohortTable Optional cohort table name.
#'
#' @return Data frame of cohort rows.
#' @examples
#' \dontrun{
#' rows <- getCohortSubjects(cohorts = c(14412), database = CCAE_OHDA_DBSpec)
#' }
#' @export
getCohortSubjects <- function(cohorts, database, cohortTable = NULL, verbose = TRUE) {
  ithGetCohortSubjects(cohorts = cohorts, database = database, cohortTable = cohortTable, verbose = verbose)
}
