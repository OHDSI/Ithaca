# Feasibility probe: do a condition's rubric-rule scoring queries run faster in parallel?
#
# It runs the same rule queries once per worker count (default: 1 in-process, then 4 workers) and
# compares wall time. Nothing is saved to the scoring folders and no LLM calls are made; a rule whose
# SQL fails is only reported. Queries run against the database in config.yaml.
#
# Example:
#   pkgload::load_all("E:/Ithaca")
#   source("extras/parallelScoringProbe.R")
#   probe <- probeParallelScoring("Autism", maxRules = 8)
#   probe$summary
#   probe$results
#
# Use countOnly = TRUE to wrap each query in SELECT COUNT(*) so no rows are transferred; comparing
# that run with a normal one shows how much of the time is spent fetching results.
# Run order matters a little: later runs can benefit from warehouse caching, so rerun with
# workerCounts = c(4, 1) if the speed-up looks suspiciously good.

# Resolves the rule SQL exactly as ithScoreConditionDatabase does (plugins, render, translate).
probePrepareRuleSql <- function(config, databases, conditionName, databaseId, ruleIds = NULL, maxRules = 8L) {
  specs <- config$conditionSpecs
  names <- vapply(specs, function(x) as.character(Ithaca:::ithCoalesce(x$rubricName, x$testCondition)[[1]]), character(1))
  condition <- specs[[match(conditionName, names)]]
  if (is.null(condition)) stop("Condition not found in config: ", conditionName)
  database <- databases[[databaseId]]
  if (is.null(database)) stop("Database not found in config: ", databaseId)

  outputDirectory <- as.character(config$outputDirectory[[1]])
  randomCohortDefinitionId <- as.integer(condition$randomCohortDefinitionId[[1]])
  shortRubric <- Ithaca:::ithLoadLatestShortRubric(conditionName, outputDirectory)
  shortRubric <- shortRubric[order(shortRubric$rubric, Ithaca:::ithRuleIdOrder(shortRubric$ruleId)), ]
  if (!is.null(ruleIds)) {
    shortRubric <- shortRubric[shortRubric$ruleId %in% ruleIds, , drop = FALSE]
  } else if (nrow(shortRubric) > maxRules) {
    shortRubric <- shortRubric[round(seq(1, nrow(shortRubric), length.out = maxRules)), , drop = FALSE]
  }

  csLibrary <- Ithaca:::ithLoadConceptSetLibraryForScoring(
    outputDirectory = outputDirectory,
    seedLibraryPath = config$seedLibraryPath,
    libraryVersion = config$conceptLibraryVersion
  )
  matched <- Ithaca:::ithLoadLatest(file.path(outputDirectory, conditionName), "matchedConceptSets.rds")
  tempTable <- if (is.null(config$cohortTableName)) Ithaca:::ithDefaultCohortTable(database) else config$cohortTableName

  sqlList <- vapply(seq_len(nrow(shortRubric)), function(i) {
    sql <- shortRubric$sqlStatement[[i]]
    plugins <- unique(stringr::str_extract_all(sql, "\\B@[A-Za-z0-9_]+")[[1]])
    plugins <- plugins[!(plugins %in% c("@databaseSchema", "@cohortSchema", "@cohortTable", "@cohortId"))]
    rendered <- SqlRender::render(
      sql,
      databaseSchema = database$cdmDatabaseSchema,
      cohortSchema = database$workDatabaseSchema,
      cohortTable = tempTable,
      cohortId = randomCohortDefinitionId
    )
    for (plugin in plugins) {
      detail <- Ithaca:::ithResolveConceptSetPluginDetails(
        substr(plugin, 2, nchar(plugin)),
        matchedConceptSets = matched,
        conceptSetJsonLibrary = csLibrary[[1]],
        database = database
      )
      rendered <- gsub(plugin, detail$value, rendered, fixed = TRUE)
    }
    SqlRender::translate(rendered, database$connectionDetails$dbms)
  }, character(1))

  list(
    rules = data.frame(ruleId = shortRubric$ruleId, rubricId = shortRubric$rubric, stringsAsFactors = FALSE),
    sql = sqlList,
    database = database
  )
}

# Runs one rule query. Used in-process for the serial run and on workers for parallel runs; each
# process opens its own connection once and reuses it.
probeRunRuleQuery <- function(index, settings) {
  startedAt <- Sys.time()
  status <- "ok"
  message <- ""
  rows <- NA_integer_
  tryCatch({
    connection <- get0("probeScoringConnection", envir = globalenv(), inherits = FALSE)
    if (is.null(connection)) {
      connection <- suppressMessages(DatabaseConnector::connect(settings$connectionDetails))
      assign("probeScoringConnection", connection, envir = globalenv())
    }
    sql <- settings$sql[[index]]
    if (isTRUE(settings$countOnly)) {
      inner <- sub(";\\s*$", "", trimws(sql))
      sql <- paste0("SELECT COUNT(*) AS n FROM (", inner, ") probeCountQuery")
    }
    result <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE)
    rows <- if (isTRUE(settings$countOnly)) as.integer(result[[1]][[1]]) else nrow(result)
  }, error = function(e) {
    status <<- "error"
    message <<- conditionMessage(e)
  })
  endedAt <- Sys.time()
  data.frame(
    ruleIndex = index,
    workerPid = Sys.getpid(),
    status = status,
    rows = rows,
    startSeconds = as.numeric(difftime(startedAt, settings$runStart, units = "secs")),
    seconds = as.numeric(difftime(endedAt, startedAt, units = "secs")),
    message = substr(gsub("[\r\n]+", " ", message), 1, 300),
    stringsAsFactors = FALSE
  )
}

probeRunRuleQueries <- function(settings, workers, ruleLabels) {
  total <- length(settings$sql)
  settings$runStart <- Sys.time()
  wallStart <- Sys.time()
  report <- function(done, value) {
    message(sprintf(
      "[%s] %d/%d finished %s: %s in %.0fs, %s rows%s", format(Sys.time(), "%H:%M:%S"), done, total,
      ruleLabels[[value$ruleIndex]], value$status, value$seconds, format(value$rows, big.mark = ","),
      if (identical(value$status, "error")) paste0(" - ", substr(value$message, 1, 150)) else ""
    ))
  }

  results <- vector("list", total)
  if (workers <= 1L) {
    for (index in seq_len(total)) {
      results[[index]] <- probeRunRuleQuery(index, settings)
      report(index, results[[index]])
    }
    # Serial runs happen in this session; close the connection so it is not left open.
    connection <- get0("probeScoringConnection", envir = globalenv(), inherits = FALSE)
    if (!is.null(connection)) {
      suppressWarnings(try(DatabaseConnector::disconnect(connection), silent = TRUE))
      rm("probeScoringConnection", envir = globalenv())
    }
  } else {
    workers <- min(workers, total)
    cluster <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    allOptions <- options()
    workerOptions <- allOptions[grepl("^(sqlRender|DatabaseConnector|databaseConnector)", names(allOptions))]
    parallel::clusterCall(cluster, function(path, opts) {
      options(opts)
      pkgload::load_all(path, quiet = TRUE)
      invisible(NULL)
    }, "E:/Ithaca", workerOptions)
    parallel::clusterExport(cluster, "probeRunRuleQuery", envir = globalenv())
    settings$runStart <- Sys.time()
    wallStart <- Sys.time()

    dispatch <- function(node, index) {
      parallel:::sendCall(cluster[[node]], probeRunRuleQuery, list(index, settings), tag = index)
    }
    nextIndex <- 1L
    for (node in seq_len(workers)) {
      dispatch(node, nextIndex)
      nextIndex <- nextIndex + 1L
    }
    for (done in seq_len(total)) {
      reply <- parallel:::recvOneResult(cluster)
      value <- reply$value
      if (!is.data.frame(value)) {
        value <- data.frame(
          ruleIndex = reply$tag, workerPid = NA_integer_, status = "error", rows = NA_integer_,
          startSeconds = NA_real_, seconds = NA_real_, message = paste(as.character(value), collapse = " "),
          stringsAsFactors = FALSE
        )
      }
      results[[reply$tag]] <- value
      report(done, value)
      if (nextIndex <= total) {
        dispatch(reply$node, nextIndex)
        nextIndex <- nextIndex + 1L
      }
    }
  }

  results <- do.call(rbind, results)
  results$ruleLabel <- ruleLabels[results$ruleIndex]
  list(results = results, elapsedSeconds = as.numeric(difftime(Sys.time(), wallStart, units = "secs")))
}

#' Probe parallel scoring queries
#'
#' @param conditionName Condition (rubricName) to take rules from; it must already have a short
#'   rubric and matched concept sets.
#' @param databaseId Database to query; `NULL` uses the first database in the config.
#' @param workerCounts Worker counts to try, in order. 1 runs in this session.
#' @param ruleIds Optional rule IDs to run; default samples `maxRules` rules spread across the rubric.
#' @param maxRules Number of rules to run when `ruleIds` is `NULL`.
#' @param countOnly When `TRUE`, each query is wrapped in `SELECT COUNT(*)` so no rows are
#'   transferred (separates query time from fetch time).
#' @param configPath Path to config.yaml.
#' @return A list with `summary` (one row per worker count), `results` (one row per rule per run)
#'   and `rules`.
probeParallelScoring <- function(conditionName,
                                 databaseId = NULL,
                                 workerCounts = c(1L, 4L),
                                 ruleIds = NULL,
                                 maxRules = 8L,
                                 countOnly = FALSE,
                                 configPath = "config.yaml") {
  config <- suppressMessages(Ithaca:::ithReadIthacaWorkflowConfig(configPath))
  databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = globalenv())
  if (is.null(databaseId)) databaseId <- names(databases)[[1]]

  prepared <- probePrepareRuleSql(config, databases, conditionName, databaseId, ruleIds, maxRules)
  ruleLabels <- paste0(prepared$rules$rubricId, "/", prepared$rules$ruleId)
  message("Probing ", length(ruleLabels), " rule(s) for '", conditionName, "' on ", databaseId,
          if (countOnly) " (count only)" else "", ": ", paste(ruleLabels, collapse = ", "))

  settings <- list(
    sql = prepared$sql,
    connectionDetails = prepared$database$connectionDetails,
    countOnly = countOnly
  )

  allResults <- list()
  summaryRows <- list()
  for (workers in as.integer(workerCounts)) {
    message("--- ", workers, " worker(s) ---")
    run <- probeRunRuleQueries(settings, workers, ruleLabels)
    run$results$workers <- workers
    allResults[[length(allResults) + 1L]] <- run$results
    summaryRows[[length(summaryRows) + 1L]] <- data.frame(
      workers = workers,
      rules = nrow(run$results),
      ok = sum(run$results$status == "ok"),
      failed = sum(run$results$status != "ok"),
      elapsedSeconds = round(run$elapsedSeconds, 1),
      summedQuerySeconds = round(sum(run$results$seconds, na.rm = TRUE), 1),
      totalRows = sum(run$results$rows, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  summary <- do.call(rbind, summaryRows)
  baseline <- summary$elapsedSeconds[summary$workers == 1L]
  if (length(baseline) == 1L) {
    summary$speedupVs1 <- round(baseline / summary$elapsedSeconds, 2)
  }
  message("\n"); print(summary)
  list(summary = summary, results = do.call(rbind, allResults), rules = prepared$rules)
}
