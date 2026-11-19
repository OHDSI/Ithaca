# Parallel helpers shared by concept-set creation and subject scoring.

# Starts a PSOCK cluster with Ithaca loaded on every worker. Workers start with default options, but
# database access depends on session options such as sqlRenderTempEmulationSchema (temp table
# emulation on Databricks/Spark), so those are copied.
ithStartWorkerCluster <- function(workers) {
  isDevelopment <- requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("Ithaca")
  packagePath <- if (isDevelopment) getNamespaceInfo("Ithaca", "path") else NULL
  cluster <- parallel::makeCluster(workers)
  allOptions <- options()
  workerOptions <- allOptions[grepl("^(sqlRender|DatabaseConnector|databaseConnector)", names(allOptions))]
  tryCatch(
    parallel::clusterCall(cluster, ithLoadIthacaOnWorker, packagePath, isDevelopment, workerOptions),
    error = function(e) {
      parallel::stopCluster(cluster)
      stop(e)
    }
  )
  cluster
}

# Disconnects any scoring connections the workers opened, then stops the cluster.
ithStopWorkerCluster <- function(cluster) {
  try(parallel::clusterCall(cluster, function() {
    state <- get0("ithScoringWorkerState", envir = globalenv(), inherits = FALSE)
    for (conn in state$connections) try(DatabaseConnector::disconnect(conn), silent = TRUE)
    invisible(NULL)
  }), silent = TRUE)
  parallel::stopCluster(cluster)
}

# Sends `taskFunction(task, ...)` for each task to the cluster with load-balanced dispatch (as in
# parallel::clusterApplyLB), in the order given by `dispatchOrder`, and calls `onResult(index, value,
# done, total)` in the main process as each task finishes. `onDispatch(index)` is called as each task
# is sent. Returns the values in task order.
ithRunOnCluster <- function(cluster, tasks, taskFunction, dispatchOrder = seq_along(tasks), onResult = NULL,
                            onDispatch = NULL) {
  total <- length(tasks)
  values <- vector("list", total)
  if (total == 0L) {
    return(values)
  }
  dispatchOrder <- as.integer(dispatchOrder)
  nextPosition <- 1L
  dispatch <- function(node) {
    index <- dispatchOrder[[nextPosition]]
    if (!is.null(onDispatch)) {
      onDispatch(index)
    }
    parallel:::sendCall(cluster[[node]], taskFunction, list(tasks[[index]]), tag = index)
    nextPosition <<- nextPosition + 1L
  }
  for (node in seq_len(min(length(cluster), total))) {
    dispatch(node)
  }
  for (done in seq_len(total)) {
    reply <- parallel:::recvOneResult(cluster)
    values[reply$tag] <- list(reply$value)
    if (!is.null(onResult)) {
      onResult(reply$tag, reply$value, done, total)
    }
    if (nextPosition <= total) {
      dispatch(reply$node)
    }
  }
  values
}

# Runs `taskFunction(task)` for every task and returns the outcomes in task order. A task function
# returns list(ok, value, error, seconds) and must not raise. One worker (or one task) runs in this
# session, and stops after the first failure when `stopOnFirstError` (later outcomes are NULL); more
# workers run on a cluster and every task finishes. `onStart(index)` and `onFinish(index, outcome)`
# run in this session.
ithRunJobs <- function(tasks, taskFunction, workers = 1L, onStart = NULL, onFinish = NULL, stopOnFirstError = TRUE) {
  total <- length(tasks)
  outcomes <- vector("list", total)
  if (total == 0L) {
    return(outcomes)
  }
  asOutcome <- function(value) {
    if (is.list(value) && !is.null(value$ok)) {
      return(value)
    }
    list(ok = FALSE, value = NULL, error = paste(as.character(value), collapse = " "), seconds = NA_real_)
  }
  workers <- min(max(1L, as.integer(workers)), total)
  if (workers <= 1L) {
    for (index in seq_len(total)) {
      if (!is.null(onStart)) {
        onStart(index)
      }
      outcomes[[index]] <- asOutcome(taskFunction(tasks[[index]]))
      if (!is.null(onFinish)) {
        onFinish(index, outcomes[[index]])
      }
      if (isTRUE(stopOnFirstError) && !isTRUE(outcomes[[index]]$ok)) {
        break
      }
    }
    return(outcomes)
  }
  cluster <- ithStartWorkerCluster(workers)
  on.exit(ithStopWorkerCluster(cluster), add = TRUE)
  values <- ithRunOnCluster(
    cluster, tasks, taskFunction,
    onDispatch = onStart,
    onResult = function(index, value, done, total) {
      if (!is.null(onFinish)) {
        onFinish(index, asOutcome(value))
      }
    }
  )
  lapply(values, asOutcome)
}

# Resolves a worker count from general.parallelWorkers, falling back to a legacy flat key.
ithConfigParallelWorkers <- function(config, name, legacyKey = NULL) {
  value <- config$parallelWorkers[[name]]
  if (is.null(value) && !is.null(legacyKey)) {
    value <- config[[legacyKey]]
  }
  value <- suppressWarnings(as.integer(ithCoalesce(value, 1L)[[1]]))
  if (length(value) == 0L || is.na(value) || value < 1L) {
    stop("general.parallelWorkers.", name, " must be a whole number >= 1", call. = FALSE)
  }
  value
}

# --- Scoring ------------------------------------------------------------------------------

ithScoringRuleTimesFileName <- "ruleQueryTimes.csv"

ithScoringRuleComplexityScore <- function(sqlText, ruleId = "") {
  sql <- if (is.null(sqlText) || length(sqlText) == 0L) "" else tolower(as.character(sqlText[[1]]))
  if (!nzchar(trimws(sql))) {
    return(0)
  }
  count <- function(pattern) length(stringr::str_extract_all(sql, pattern)[[1]])
  score <- 1 +
    2 * count("\\bjoin\\b") +
    2 * count("\\bunion\\b") +
    1.5 * count("\\bexists\\b|\\bin\\s*\\(") +
    1.5 * count("\\bcount\\s*\\(") +
    1.5 * count("\\bdistinct\\b") +
    1 * count("\\bor\\b") +
    1 * count("\\bgroup\\s+by\\b") +
    1 * count("\\bdatediff\\b|\\bmonths_between\\b|\\bdate_add\\b") +
    1 * count("@[a-z0-9_]+")
  if (grepl("^AE", toupper(as.character(ruleId)))) {
    score <- score + 2
  }
  as.numeric(score)
}

# Order in which rule queries are sent to workers: slowest first, using the query times saved by the
# previous run for this condition and database, so the longest queries do not finish last. Rules
# without a saved time go first (AE rules, which return most of the cohort, ahead of the others).
ithScoringDispatchOrder <- function(shortRubric, cohortDir) {
  ruleCount <- nrow(shortRubric)
  timesPath <- file.path(cohortDir, ithScoringRuleTimesFileName)
  previousSeconds <- rep(NA_real_, ruleCount)
  hasSql <- "sqlStatement" %in% names(shortRubric)
  if (file.exists(timesPath)) {
    times <- tryCatch(utils::read.csv(timesPath, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(times) && all(c("rubricId", "ruleId", "seconds") %in% names(times))) {
      key <- paste(shortRubric$rubric, shortRubric$ruleId, sep = "\r")
      found <- match(key, paste(times$rubricId, times$ruleId, sep = "\r"))
      known <- !is.na(found) & is.finite(times$seconds[found]) & times$seconds[found] > 0
      previousSeconds[known] <- times$seconds[found[known]]
    }
  }
  isAeRule <- grepl("^AE", as.character(shortRubric$ruleId))
  if (hasSql) {
    heuristicSeconds <- vapply(seq_len(ruleCount), function(index) {
      10 * ithScoringRuleComplexityScore(shortRubric$sqlStatement[[index]], shortRubric$ruleId[[index]])
    }, numeric(1))
    estimatedSeconds <- ifelse(is.na(previousSeconds), heuristicSeconds, previousSeconds)
    return(order(-estimatedSeconds, is.na(previousSeconds), !isAeRule, seq_len(ruleCount)))
  }
  estimatedSeconds <- ifelse(is.na(previousSeconds), Inf, previousSeconds)
  order(-estimatedSeconds, !isAeRule, seq_len(ruleCount))
}

ithSaveScoringRuleTimes <- function(cohortDir, shortRubric, seconds, rowCounts) {
  times <- data.frame(
    rubricId = shortRubric$rubric,
    ruleId = shortRubric$ruleId,
    seconds = round(seconds, 2),
    rowCount = rowCounts,
    recordedAt = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  times <- times[is.finite(times$seconds), , drop = FALSE]
  if (nrow(times) > 0L) {
    utils::write.csv(times, file.path(cohortDir, ithScoringRuleTimesFileName), row.names = FALSE)
  }
}

# Runs one scoring query on a worker. Each worker keeps one connection per database and writes the
# query it is running to its own file under lastQuery/. Errors are returned, not raised, so the main
# process can run the LLM SQL correction and retries.
ithRunScoringQueryOnWorker <- function(task) {
  state <- get0("ithScoringWorkerState", envir = globalenv(), inherits = FALSE)
  if (is.null(state)) {
    state <- list(connections = list())
  }
  startedAt <- Sys.time()
  errorMessage <- NULL
  result <- tryCatch({
    conn <- state$connections[[task$databaseId]]
    if (is.null(conn)) {
      conn <- suppressMessages(DatabaseConnector::connect(task$connectionDetails))
      state$connections[[task$databaseId]] <- conn
      assign("ithScoringWorkerState", state, envir = globalenv())
    }
    dir.create(task$lastQueryDir, recursive = TRUE, showWarnings = FALSE)
    writeLines(task$sql, con = file.path(task$lastQueryDir, paste0("query_worker", Sys.getpid(), ".sql")))
    DatabaseConnector::querySql(conn, task$sql, snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE)
  }, error = function(e) {
    errorMessage <<- conditionMessage(e)
    NULL
  })
  list(
    result = result,
    errorMessage = errorMessage,
    seconds = as.numeric(difftime(Sys.time(), startedAt, units = "secs"))
  )
}
