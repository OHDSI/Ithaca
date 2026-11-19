# Feasibility probe: does building random cohorts in parallel beat building them one at a time?
#
# It picks N conditions from the config's conditionSpecs (fixed seed), and builds each one's random
# cohort on every database, once serially in this session and once on workers. Every run writes to
# its own new tables in cohortDatabaseSchema (ithacaprobe_<database>_serial / _parallel), never to
# the real ithaca_* cohort tables, and all probe tables are dropped at the end. config.yaml is not
# changed; the CDM mapping below replaces general.cdm for the probe only.
#
# Example:
#   pkgload::load_all("E:/Ithaca")
#   source("extras/parallelCohortProbe.R")
#   probe <- probeParallelCohortBuild()
#   probe$summary
#   probe$results
#
# Run order matters a little: the serial run goes first, so the parallel run can benefit from
# warehouse caching of the CDM tables. Use modes = c("parallel", "serial") to check the opposite order.

probeCohortCdm <- list(
  Ccae = "cdm_merative_ccae_v4149",
  OptumDod = "cdm_optum_extended_dod_v4155"
)

probeConditionName <- function(spec) {
  as.character(Ithaca:::ithCoalesce(spec$rubricName, spec$testCondition)[[1]])
}

# Cohort IDs a condition's build writes: random, source, and the optional sample-pool cohort.
probeConditionCohortIds <- function(spec, randomDefaults) {
  sourceId <- Ithaca:::ithCoalesce(spec$sourceCohortDefinitionId, randomDefaults$sourceCohortDefinitionId)
  poolId <- Ithaca:::ithCoalesce(spec$randomSampleFromCohortDefinitionId, randomDefaults$randomSampleFromCohortDefinitionId)
  randomId <- Ithaca:::ithCoalesce(spec$randomCohortDefinitionId, Ithaca:::ithCoalesce(randomDefaults$randomCohortDefinitionId, 1L))
  ids <- suppressWarnings(as.integer(c(randomId[[1]], sourceId[[1]], if (!is.null(poolId)) poolId[[1]])))
  list(randomId = ids[[1]], sourceId = ids[[2]], poolId = if (length(ids) > 2L) ids[[3]] else NA_integer_, all = ids[!is.na(ids)])
}

# Picks conditions whose cohort IDs do not overlap, so concurrent jobs sharing one cohort table
# never write or regenerate the same cohort ID.
probePickConditions <- function(specs, randomDefaults, count, seed) {
  set.seed(seed)
  picked <- integer()
  usedIds <- integer()
  for (index in sample(seq_along(specs))) {
    ids <- probeConditionCohortIds(specs[[index]], randomDefaults)
    if (length(ids$all) < 2L || anyNA(ids$all[1:2]) || length(intersect(ids$all, usedIds)) > 0L) next
    picked <- c(picked, index)
    usedIds <- c(usedIds, ids$all)
    if (length(picked) == count) break
  }
  if (length(picked) < count) {
    warning("Only ", length(picked), " condition(s) with non-overlapping cohort IDs found; wanted ", count)
  }
  picked
}

# Runs one condition x database build. Used in-process for the serial run and on workers for the
# parallel run. Errors are returned, not raised.
probeRunCohortJob <- function(task) {
  startedAt <- Sys.time()
  status <- "ok"
  errorMessage <- ""
  rowCount <- NA_integer_
  tryCatch({
    suppressMessages(do.call(Ithaca::buildRandomOmopCohort, task$args))
    rowCount <- Ithaca:::getCohortRowCount(
      connectionDetails = task$args$database$connectionDetails,
      cohort_database_schema = task$args$database$cohortDatabaseSchema,
      cohortTableName = task$args$database$cohortTable,
      cohort_definition_id = task$args$cohort_definition_id
    )
  }, error = function(e) {
    status <<- "error"
    errorMessage <<- conditionMessage(e)
  })
  data.frame(
    jobIndex = task$jobIndex,
    condition = task$condition,
    databaseId = task$databaseId,
    workerPid = Sys.getpid(),
    status = status,
    randomCohortRows = rowCount,
    seconds = as.numeric(difftime(Sys.time(), startedAt, units = "secs")),
    message = substr(gsub("[\r\n]+", " ", errorMessage), 1, 300),
    stringsAsFactors = FALSE
  )
}

probeBuildTasks <- function(config, databases, specs, randomDefaults, mode) {
  tasks <- list()
  for (spec in specs) {
    ids <- probeConditionCohortIds(spec, randomDefaults)
    for (databaseId in names(databases)) {
      database <- databases[[databaseId]]
      database$cohortTable <- tolower(paste0("ithacaprobe_", databaseId, "_", mode))
      tasks[[length(tasks) + 1L]] <- list(
        jobIndex = length(tasks) + 1L,
        condition = probeConditionName(spec),
        databaseId = databaseId,
        args = list(
          database = database,
          cohort_definition_id = ids$randomId,
          sourceCohortDefinitionId = ids$sourceId,
          webapiBaseUrl = as.character(Ithaca:::ithCoalesce(config$webapiBaseUrl, randomDefaults$webapiBaseUrl)[[1]]),
          randomSampleFromCohortDefinitionId = if (is.na(ids$poolId)) NULL else ids$poolId,
          sample_size = as.integer(Ithaca:::ithCoalesce(randomDefaults$sampleSize, 2000000L)[[1]]),
          min_visit_start_date = as.character(Ithaca:::ithCoalesce(randomDefaults$minVisitStartDate, "01/01/2000")[[1]]),
          max_visit_end_date = as.character(Ithaca:::ithCoalesce(randomDefaults$maxVisitEndDate, "12/31/2026")[[1]]),
          min_age = as.numeric(Ithaca:::ithCoalesce(randomDefaults$minAge, 0)[[1]]),
          max_age = as.numeric(Ithaca:::ithCoalesce(randomDefaults$maxAge, 120)[[1]]),
          cohortName = as.character(Ithaca:::ithCoalesce(randomDefaults$cohortName, "random_visit_sample")[[1]]),
          sourceCohortName = as.character(Ithaca:::ithCoalesce(randomDefaults$sourceCohortName, "source_webapi_cohort")[[1]]),
          seedLibraryPath = as.character(Ithaca:::ithCoalesce(randomDefaults$seedLibraryPath, config$seedLibraryPath)[[1]]),
          sqlFilePath = normalizePath(
            as.character(Ithaca:::ithCoalesce(randomDefaults$sqlFilePath, file.path("inst", "sql", "sql_server", "build_random_omop_cohort.sql"))[[1]]),
            winslash = "/", mustWork = FALSE
          ),
          validationSqlFilePath = normalizePath(
            as.character(Ithaca:::ithCoalesce(randomDefaults$validationSqlFilePath, file.path("inst", "sql", "sql_server", "validate_overwrite_dates.sql"))[[1]]),
            winslash = "/", mustWork = FALSE
          )
        )
      )
    }
  }
  tasks
}

probeDropTables <- function(databases, modes) {
  for (databaseId in names(databases)) {
    database <- databases[[databaseId]]
    connection <- tryCatch(suppressMessages(DatabaseConnector::connect(database$connectionDetails)), error = function(e) NULL)
    if (is.null(connection)) next
    for (mode in modes) {
      tableNames <- CohortGenerator::getCohortTableNames(cohortTable = tolower(paste0("ithacaprobe_", databaseId, "_", mode)))
      for (tableName in unlist(tableNames[vapply(tableNames, is.character, logical(1))])) {
        try(DatabaseConnector::executeSql(
          connection,
          paste0("DROP TABLE IF EXISTS ", database$cohortDatabaseSchema, ".", tableName),
          progressBar = FALSE, reportOverallTime = FALSE
        ), silent = TRUE)
      }
    }
    suppressWarnings(try(DatabaseConnector::disconnect(connection), silent = TRUE))
  }
}

# Workers that each create the same cohort table at once collide (SQL state 42P07), so create them first.
probeCreateTables <- function(tasks) {
  created <- character()
  for (task in tasks) {
    database <- task$args$database
    key <- paste(database$cohortDatabaseSchema, database$cohortTable)
    if (key %in% created) next
    CohortGenerator::createCohortTables(
      connectionDetails = database$connectionDetails,
      cohortDatabaseSchema = database$cohortDatabaseSchema,
      cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = database$cohortTable),
      incremental = TRUE
    )
    created <- c(created, key)
  }
}

probeRunMode <- function(tasks, mode, workers) {
  report <- function(done, value) {
    message(sprintf(
      "[%s] %s %d/%d finished %s on %s: %s in %.0fs, %s rows%s", format(Sys.time(), "%H:%M:%S"), mode, done, length(tasks),
      value$condition, value$databaseId, value$status, value$seconds, format(value$randomCohortRows, big.mark = ","),
      if (identical(value$status, "error")) paste0(" - ", substr(value$message, 1, 150)) else ""
    ))
  }
  probeCreateTables(tasks)
  wallStart <- Sys.time()
  if (workers <= 1L) {
    values <- vector("list", length(tasks))
    for (index in seq_along(tasks)) {
      values[[index]] <- probeRunCohortJob(tasks[[index]])
      report(index, values[[index]])
    }
  } else {
    cluster <- Ithaca:::ithStartWorkerCluster(min(workers, length(tasks)))
    on.exit(Ithaca:::ithStopWorkerCluster(cluster), add = TRUE)
    values <- Ithaca:::ithRunOnCluster(
      cluster, tasks, probeRunCohortJob,
      onResult = function(index, value, done, total) {
        if (!is.data.frame(value)) {
          value <- data.frame(
            jobIndex = index, condition = tasks[[index]]$condition, databaseId = tasks[[index]]$databaseId,
            workerPid = NA_integer_, status = "error", randomCohortRows = NA_integer_, seconds = NA_real_,
            message = paste(as.character(value), collapse = " "), stringsAsFactors = FALSE
          )
        }
        report(done, value)
      }
    )
    values <- lapply(seq_along(values), function(index) {
      value <- values[[index]]
      if (is.data.frame(value)) value else data.frame(
        jobIndex = index, condition = tasks[[index]]$condition, databaseId = tasks[[index]]$databaseId,
        workerPid = NA_integer_, status = "error", randomCohortRows = NA_integer_, seconds = NA_real_,
        message = paste(as.character(value), collapse = " "), stringsAsFactors = FALSE
      )
    })
  }
  results <- do.call(rbind, values)
  results$mode <- mode
  list(results = results, elapsedSeconds = as.numeric(difftime(Sys.time(), wallStart, units = "secs")))
}

#' Probe parallel random-cohort builds
#'
#' @param conditionCount Number of conditions to build (each is built on every database).
#' @param cdm Named list of database id -> CDM schema name used instead of general.cdm.
#' @param workers Worker processes for the parallel run (capped at 5).
#' @param seed Seed for picking conditions.
#' @param modes Runs to do, in order: "serial" and/or "parallel".
#' @param configPath Path to config.yaml.
#' @return A list with `summary` (one row per mode), `results` (one row per job per mode) and
#'   `conditions` (the picked condition names).
probeParallelCohortBuild <- function(conditionCount = 10L,
                                     cdm = probeCohortCdm,
                                     workers = 5L,
                                     seed = 20261103L,
                                     modes = c("serial", "parallel"),
                                     configPath = "config.yaml") {
  workers <- min(as.integer(workers), 5L)
  config <- suppressMessages(Ithaca:::ithReadIthacaWorkflowConfig(configPath))
  config$cdm <- cdm
  databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = globalenv())
  randomDefaults <- Ithaca:::ithCoalesce(config$randomCohortDefaults, Ithaca:::ithCoalesce(config$randomCohort, list()))

  picked <- probePickConditions(config$conditionSpecs, randomDefaults, as.integer(conditionCount), seed)
  specs <- config$conditionSpecs[picked]
  conditionNames <- vapply(specs, probeConditionName, character(1))
  message("Probing ", length(specs), " condition(s) x ", length(databases), " database(s): ", paste(conditionNames, collapse = ", "))

  # Tables left by an interrupted earlier probe would be reused instead of rebuilt.
  probeDropTables(databases, c("serial", "parallel"))
  on.exit(probeDropTables(databases, c("serial", "parallel")), add = TRUE)

  allResults <- list()
  summaryRows <- list()
  for (mode in modes) {
    message("--- ", mode, if (mode == "parallel") paste0(" (", workers, " workers)") else "", " ---")
    tasks <- probeBuildTasks(config, databases, specs, randomDefaults, mode)
    run <- probeRunMode(tasks, mode, if (mode == "parallel") workers else 1L)
    allResults[[length(allResults) + 1L]] <- run$results
    summaryRows[[length(summaryRows) + 1L]] <- data.frame(
      mode = mode,
      workers = if (mode == "parallel") workers else 1L,
      jobs = nrow(run$results),
      ok = sum(run$results$status == "ok"),
      failed = sum(run$results$status != "ok"),
      elapsedSeconds = round(run$elapsedSeconds, 1),
      summedJobSeconds = round(sum(run$results$seconds, na.rm = TRUE), 1),
      stringsAsFactors = FALSE
    )
  }

  summary <- do.call(rbind, summaryRows)
  if (all(c("serial", "parallel") %in% summary$mode)) {
    summary$speedupVsSerial <- round(summary$elapsedSeconds[summary$mode == "serial"] / summary$elapsedSeconds, 2)
  }
  message("\n"); print(summary)
  list(summary = summary, results = do.call(rbind, allResults), conditions = conditionNames)
}
