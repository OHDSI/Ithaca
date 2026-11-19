# Probe: does each rubric rule separate people with the condition from people without it?
#
# No LLM. The rule's own scoring SQL (from the latest short rubric) is run twice, once with each of
# two populations as its base cohort, and the hit rates are compared. Both populations are built the
# same way, by running ER2's SQL on a base cohort of each person's first diagnosis date:
#   POSITIVE: base = first date with a code from <condition>_DIAGNOSIS_CODES; ER2 as written. The
#     people it returns are the positives.
#   NEGATIVE: base = first date with a code from <condition>_RELEVANT_DIAGNOSIS_CODES (the differential
#     diagnoses used by AE1), for people who never have a code from <condition>_DIAGNOSIS_CODES; ER2
#     with its <condition>_DIAGNOSIS_CODES concept set replaced by the relevant-diagnosis set. The
#     people it returns are the negatives (they meet ER2's pattern for a differential diagnosis).
# The populations are written to a scratch cohort table (cohortDatabaseSchema in the config) that is
# dropped at the end. Nothing is written to the rubric files.
#
# A rule with a positive score should be more common in POSITIVE (likelihood ratio above 1); a rule
# with a negative score (hard exclusion, alternative explanation) should be more common in NEGATIVE.
# ER2 defines the positives, so a rule that exists to remove false positives can look wrong here;
# read exclusion rules with that in mind.
#
# Example:
#   pkgload::load_all("E:/Ithaca")
#   source("extras/rubricPopulationProbe.R")
#   probe <- probeRulePopulations("asthma", excludeRuleIds = c("ER1", "ER2", "AE1", "AE5", "AE6"))
#   probe$results      # hit rates in each population, likelihood ratio, and whether the direction fits
#   probe$populations  # size of each population

# Renders a rule's SQL for the database, resolves its concept-set placeholders, and translates it.
# Returns list(sql, unresolved) where unresolved names sets that fell back to a dummy concept.
probeRenderRuleSql <- function(sql, database, cohortTable, cohortId, conceptSets) {
  sql <- gsub("(?s)--[^\n]*|/\\*.*?\\*/", " ", as.character(sql), perl = TRUE)
  plugins <- unique(stringr::str_extract_all(sql, "\\B@[A-Za-z0-9_]+")[[1]])
  plugins <- plugins[!(plugins %in% c("@databaseSchema", "@cohortSchema", "@cohortTable", "@cohortId"))]
  plugins <- plugins[order(-nchar(plugins))]
  if (length(plugins) > 0L && !is.null(conceptSets$error)) {
    stop("concept sets could not be loaded: ", conceptSets$error, call. = FALSE)
  }
  rendered <- suppressWarnings(SqlRender::render(
    sql, databaseSchema = database$cdmDatabaseSchema, cohortSchema = database$workDatabaseSchema,
    cohortTable = cohortTable, cohortId = cohortId
  ))
  unresolved <- character(0)
  for (plugin in plugins) {
    detail <- Ithaca:::ithResolveConceptSetPluginDetails(
      substr(plugin, 2, nchar(plugin)), matchedConceptSets = conceptSets$matched,
      conceptSetJsonLibrary = conceptSets$library, database = database
    )
    if (!grepl("^resolved", as.character(detail$status))) {
      unresolved <- c(unresolved, paste0(substr(plugin, 2, nchar(plugin)), "(", detail$status, ")"))
    }
    rendered <- gsub(plugin, detail$value, rendered, fixed = TRUE)
  }
  list(sql = SqlRender::translate(rendered, database$connectionDetails$dbms), unresolved = unresolved)
}

probeStripSemicolon <- function(sql) trimws(sub(";\\s*$", "", trimws(sql)))

# Compares hit rates between the populations. direction: +1 when the rule should be more common in
# POSITIVE (positive score), -1 when it should be more common in NEGATIVE.
probeCompareRates <- function(hitsPos, nPos, hitsNeg, nNeg, direction) {
  if (hitsPos == 0 && hitsNeg == 0) {
    return(data.frame(
      proportionPositive = 0, proportionNegative = 0, riskDifference = 0, likelihoodRatio = NA_real_,
      lrLower = NA_real_, lrUpper = NA_real_, logLr = NA_real_, direction = direction, verdict = "no hits",
      stringsAsFactors = FALSE
    ))
  }
  pPos <- hitsPos / nPos
  pNeg <- hitsNeg / nNeg
  # Katz interval for the ratio; 0.5 added to zero cells so the ratio and its interval exist.
  a <- ifelse(hitsPos == 0, 0.5, hitsPos)
  b <- ifelse(hitsNeg == 0, 0.5, hitsNeg)
  logLr <- log((a / nPos) / (b / nNeg))
  se <- sqrt(1 / a - 1 / nPos + 1 / b - 1 / nNeg)
  lower <- exp(logLr - 1.96 * se)
  upper <- exp(logLr + 1.96 * se)
  fits <- if (lower > 1) 1L else if (upper < 1) -1L else 0L
  verdict <- if (fits == 0L) "inconclusive" else if (fits == direction) "supports" else "refutes"
  data.frame(
    proportionPositive = pPos, proportionNegative = pNeg, riskDifference = pPos - pNeg,
    likelihoodRatio = exp(logLr), lrLower = lower, lrUpper = upper, logLr = logLr, direction = direction,
    verdict = verdict, stringsAsFactors = FALSE
  )
}

# Runs one SQL task: list(kind = "exec" or "count", sql, connectionDetails, connection). Used in this
# session (with its connection) and on workers (one connection per worker, kept for reuse); returns
# list(ok, value, error, seconds) instead of raising.
probeRunSqlTask <- function(task) {
  startedAt <- Sys.time()
  outcome <- tryCatch({
    connection <- task[["connection"]]
    if (is.null(connection)) {
      state <- get0("ithScoringWorkerState", envir = globalenv(), inherits = FALSE)
      if (is.null(state)) state <- list(connections = list())
      key <- if (is.null(task[["connectionKey"]])) "probe" else paste0("probe_", task[["connectionKey"]])
      connection <- state$connections[[key]]
      if (is.null(connection)) {
        connection <- suppressMessages(DatabaseConnector::connect(task$connectionDetails))
        state$connections[[key]] <- connection
        assign("ithScoringWorkerState", state, envir = globalenv())
      }
    }
    value <- if (identical(task$kind, "count")) {
      as.numeric(DatabaseConnector::querySql(connection, task$sql, snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE)[[1]][[1]])
    } else {
      DatabaseConnector::executeSql(connection, task$sql, progressBar = FALSE, reportOverallTime = FALSE)
      NA_real_
    }
    list(ok = TRUE, value = value, error = NULL)
  }, error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e)))
  outcome$seconds <- as.numeric(difftime(Sys.time(), startedAt, units = "secs"))
  outcome
}

#' Compare each rule's hit rate in ER2-defined positives and differential-diagnosis negatives
#'
#' @param conditionName Condition/rubric name (its output folder and concept-set naming).
#' @param excludeRuleIds Rule IDs to leave out; every other rule is tested. The default skips the
#'   fixed rules (ER2 defines the positives). Use `character(0)` to test every rule.
#' @param databaseId Database to run against.
#' @param cdm Named list of database id -> CDM schema name, used when `databaseId` is not in the
#'   config's `general.cdm` (OptumDod is commented out there).
#' @param personModulus Keep only people whose person_id is a multiple of this number (1 keeps everyone;
#'   100 keeps about 1 percent) to make the run faster.
#' @param workers Number of parallel worker processes that run the SQL (the two candidate inserts, the
#'   ER2 population inserts, and the two counts per rule); `NULL` uses `general.parallelWorkers.scoring`
#'   from the config and 1 runs everything in this session. Each worker opens its own connection.
#' @param keepTable When `TRUE`, keep the scratch cohort table.
#' @param normalize When `TRUE`, also rescale every rule's score to -6..6 with `probeNormalizeScores()`.
#' @param capLr Likelihood ratio that earns the top normalized score (see `probeNormalizeScores()`).
#' @param configPath Path to config.yaml.
#' @param outputDirectory Root output directory; `NULL` uses the config's.
#' @param saveResults When `TRUE`, save the results as a CSV in `<outputDirectory>/rubricEvidenceProbe/`.
#' @param verbose Print progress.
#' @return A list with `results` (one row per rule), `normalized` (scores rescaled to -6..6 beside the
#'   original scores, or `NULL`), `populations` (sizes), `savedTo` and `table`.
probeRulePopulations <- function(conditionName,
                                 excludeRuleIds = c("ER1", "ER2", "AE1"),
                                 databaseId = "OptumDod",
                                 cdm = list(OptumDod = "cdm_optum_extended_dod_v4155"),
                                 personModulus = 1L,
                                 workers = NULL,
                                 keepTable = FALSE,
                                 normalize = TRUE,
                                 capLr = 100,
                                 configPath = "config.yaml",
                                 outputDirectory = NULL,
                                 saveResults = TRUE,
                                 verbose = TRUE) {
  config <- suppressMessages(Ithaca:::ithReadIthacaWorkflowConfig(configPath))
  if (is.null(outputDirectory)) outputDirectory <- as.character(config$outputDirectory[[1]])
  conditionDir <- file.path(outputDirectory, conditionName)
  say <- function(...) Ithaca:::ithMaybeMessage(verbose, ...)

  cdmMap <- if (databaseId %in% names(config$cdm)) config$cdm[databaseId] else cdm[databaseId]
  if (is.null(cdmMap[[1]])) stop("No CDM schema for database '", databaseId, "' in the config or in cdm")
  config$cdm <- cdmMap
  database <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = globalenv())[[databaseId]]

  shortRubric <- Ithaca:::ithLoadLatestShortRubric(conditionName, outputDirectory)
  concise <- Ithaca:::ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
  if (is.null(concise)) stop("No concise rubric found for condition: ", conditionName)
  info <- concise[, intersect(c("rubric", "ruleId", "type", "score", "criteria", "neededConceptSets"), names(concise)), drop = FALSE]
  shortRubric <- merge(shortRubric[, c("rubric", "ruleId", "sqlStatement")], info, by = c("rubric", "ruleId"), all.x = TRUE, sort = FALSE)
  shortRubric <- shortRubric[order(shortRubric$rubric, Ithaca:::ithRuleIdOrder(shortRubric$ruleId)), , drop = FALSE]

  conceptSets <- tryCatch(list(
    library = Ithaca:::ithLoadConceptSetLibraryForScoring(
      outputDirectory = outputDirectory, seedLibraryPath = config$seedLibraryPath, libraryVersion = config$conceptLibraryVersion
    )[[1]],
    matched = Ithaca:::ithLoadLatest(conditionDir, "matchedConceptSets.rds")
  ), error = function(e) list(error = conditionMessage(e)))

  conditionSet <- Ithaca:::ithAe1ConditionConceptSetName(conditionName)
  relevantSet <- Ithaca:::ithAe1ConceptSetName(conditionName)
  modulus <- as.integer(personModulus)
  table <- tolower(paste0("ithacaprobe_rulepop_", databaseId, "_", format(Sys.time(), "%Y%m%d%H%M%S")))
  qualified <- paste0(database$workDatabaseSchema, ".", table)

  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  exec <- function(sql) {
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  }
  on.exit({
    if (!isTRUE(keepTable)) {
      try(exec(SqlRender::translate(paste0("DROP TABLE IF EXISTS ", qualified), database$connectionDetails$dbms)), silent = TRUE)
    }
    suppressWarnings(try(DatabaseConnector::disconnect(connection), silent = TRUE))
  }, add = TRUE)

  workers <- if (is.null(workers)) Ithaca:::ithConfigParallelWorkers(config, "scoring") else max(1L, as.integer(workers))
  cluster <- NULL
  if (workers > 1L) {
    say("Starting ", workers, " worker(s) for the SQL calls")
    cluster <- Ithaca:::ithStartWorkerCluster(workers)
    on.exit(Ithaca:::ithStopWorkerCluster(cluster), add = TRUE)
  }
  asOutcome <- function(value) {
    if (is.list(value) && !is.null(value$ok)) value else list(ok = FALSE, value = NULL, error = paste(as.character(value), collapse = " "), seconds = NA_real_)
  }
  # Runs list(kind, sql, label) tasks on the workers (or one at a time here); outcomes come back in task order.
  runTasks <- function(tasks) {
    tasks <- lapply(tasks, function(task) {
      task$connectionDetails <- database$connectionDetails
      if (is.null(cluster)) task$connection <- connection
      task
    })
    finished <- function(index, outcome) {
      say("  ", tasks[[index]]$label, if (isTRUE(outcome$ok)) "" else paste0(" FAILED - ", substr(outcome$error, 1, 120)), " (", round(outcome$seconds, 1), "s)")
    }
    if (is.null(cluster)) {
      return(lapply(seq_along(tasks), function(index) {
        outcome <- probeRunSqlTask(tasks[[index]])
        finished(index, outcome)
        outcome
      }))
    }
    values <- Ithaca:::ithRunOnCluster(
      cluster, tasks, probeRunSqlTask,
      onResult = function(index, value, done, total) finished(index, asOutcome(value))
    )
    lapply(values, asOutcome)
  }
  stopOnFailed <- function(tasks, outcomes) {
    failed <- which(!vapply(outcomes, function(outcome) isTRUE(outcome$ok), logical(1)))
    if (length(failed) > 0L) {
      stop(paste0(vapply(failed, function(i) paste0(tasks[[i]]$label, " failed: ", outcomes[[i]]$error), character(1)), collapse = "\n"), call. = FALSE)
    }
  }
  render <- function(sql, cohortId = NA) {
    probeRenderRuleSql(sql, database, table, cohortId, conceptSets)
  }
  insertHeader <- "INSERT INTO @cohortSchema.@cohortTable (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)\n"
  modulusFilter <- if (modulus > 1L) paste0(" AND co.person_id % ", modulus, " = 0") else ""

  say("Building populations in ", qualified, " on ", databaseId, " (", database$cdmDatabaseSchema, ")")
  # Written in Spark SQL directly: SqlRender turns CREATE TABLE into a SELECT ... INTO for Spark.
  exec(paste0(
    "CREATE TABLE ", qualified,
    " (cohort_definition_id INT, subject_id BIGINT, cohort_start_date DATE, cohort_end_date DATE) USING DELTA"
  ))

  baseId <- 90L
  negativeBaseId <- 91L
  baseTasks <- list()
  built <- render(paste0(
    insertHeader, "SELECT ", baseId, ", co.person_id, MIN(co.condition_start_date), MIN(co.condition_start_date)\n",
    "FROM @databaseSchema.condition_occurrence co\n",
    "WHERE co.condition_concept_id IN (@", conditionSet, ")", modulusFilter, "\nGROUP BY co.person_id"
  ), baseId)
  if (length(built$unresolved) > 0L) stop("Concept set not resolved: ", paste(built$unresolved, collapse = ", "), call. = FALSE)
  baseTasks[[1]] <- list(kind = "exec", sql = built$sql, label = "candidate people (first diagnosis date)")

  built <- render(paste0(
    insertHeader, "SELECT ", negativeBaseId, ", r.person_id, r.first_date, r.first_date\n",
    "FROM (SELECT co.person_id, MIN(co.condition_start_date) AS first_date\n",
    "      FROM @databaseSchema.condition_occurrence co\n",
    "      WHERE co.condition_concept_id IN (@", relevantSet, ")", modulusFilter, "\n",
    "      GROUP BY co.person_id) r\n",
    "WHERE NOT EXISTS (SELECT 1 FROM @databaseSchema.condition_occurrence c2\n",
    "                  WHERE c2.person_id = r.person_id AND c2.condition_concept_id IN (@", conditionSet, "))"
  ), negativeBaseId)
  if (length(built$unresolved) > 0L) stop("Concept set not resolved: ", paste(built$unresolved, collapse = ", "), call. = FALSE)
  baseTasks[[2]] <- list(kind = "exec", sql = built$sql, label = "candidate negatives (first differential diagnosis date, never the condition)")
  stopOnFailed(baseTasks, runTasks(baseTasks))

  rubricIds <- unique(as.character(shortRubric$rubric))
  positiveIds <- stats::setNames(10L * seq_along(rubricIds) + 1L, rubricIds)
  negativeIds <- stats::setNames(10L * seq_along(rubricIds) + 2L, rubricIds)
  conditionPattern <- paste0("@", conditionSet, "\\b")
  populationTasks <- list()
  for (rubricId in rubricIds) {
    er2 <- shortRubric[shortRubric$rubric == rubricId & toupper(trimws(shortRubric$ruleId)) == "ER2", , drop = FALSE]
    if (nrow(er2) == 0L) stop("Rubric ", rubricId, " has no ER2 rule to define the populations", call. = FALSE)
    er2Text <- er2$sqlStatement[[1]]
    if (!grepl(conditionPattern, er2Text, ignore.case = TRUE)) {
      stop("ER2's SQL for rubric ", rubricId, " does not use @", conditionSet, ", so the negative population cannot be built from it", call. = FALSE)
    }
    for (population in c("POSITIVE", "NEGATIVE")) {
      isPositive <- population == "POSITIVE"
      sqlText <- if (isPositive) er2Text else gsub(conditionPattern, paste0("@", relevantSet), er2Text, ignore.case = TRUE)
      targetId <- if (isPositive) positiveIds[[rubricId]] else negativeIds[[rubricId]]
      er2Sql <- render(sqlText, if (isPositive) baseId else negativeBaseId)
      if (length(er2Sql$unresolved) > 0L) stop("ER2 concept set not resolved: ", paste(er2Sql$unresolved, collapse = ", "), call. = FALSE)
      populationTasks[[length(populationTasks) + 1L]] <- list(
        kind = "exec",
        sql = paste0(
          "INSERT INTO ", qualified, " (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)\n",
          "SELECT ", targetId, ", subject_id, cohort_start_date, cohort_start_date FROM (",
          probeStripSemicolon(er2Sql$sql), ") er2_subjects"
        ),
        label = paste0(population, " population for rubric ", rubricId, " (ER2 on the ", if (isPositive) "condition" else "relevant-diagnosis", " base)")
      )
    }
  }
  stopOnFailed(populationTasks, runTasks(populationTasks))

  sizes <- DatabaseConnector::querySql(
    connection,
    SqlRender::translate(paste0("SELECT cohort_definition_id, COUNT(*) AS persons FROM ", qualified, " GROUP BY cohort_definition_id"), database$connectionDetails$dbms),
    snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE
  )
  # integer64 arithmetic truncates the 0.5 continuity correction to 0, so use plain numbers.
  sizes$persons <- as.numeric(sizes$persons)
  sizes$population <- ifelse(sizes$cohortDefinitionId == baseId, "CANDIDATE (first diagnosis)",
    ifelse(sizes$cohortDefinitionId == negativeBaseId, "CANDIDATE NEGATIVE (first differential diagnosis)",
      ifelse(sizes$cohortDefinitionId %in% negativeIds, "NEGATIVE", "POSITIVE")))
  sizes$rubric <- c(names(positiveIds), names(negativeIds))[match(sizes$cohortDefinitionId, c(positiveIds, negativeIds))]
  emptyIds <- setdiff(c(positiveIds, negativeIds), sizes$cohortDefinitionId)
  if (length(emptyIds) > 0L) {
    stop(
      "Empty population(s) (no ER2 subjects): ",
      paste(c(paste0("POSITIVE ", names(positiveIds)), paste0("NEGATIVE ", names(negativeIds)))[match(emptyIds, c(positiveIds, negativeIds))], collapse = ", "),
      call. = FALSE
    )
  }
  say("Population sizes: ", paste(paste0(sizes$population, ifelse(is.na(sizes$rubric), "", paste0(" ", sizes$rubric)), "=", format(sizes$persons, big.mark = ",")), collapse = "; "))

  selected <- which(!(toupper(trimws(shortRubric$ruleId)) %in% toupper(trimws(excludeRuleIds))))
  say("Testing ", length(selected), " rule(s), excluding ", if (length(excludeRuleIds) > 0L) paste(excludeRuleIds, collapse = ", ") else "none")

  # Both counts of every rule are independent SQL calls, so they all go out together.
  ruleParts <- lapply(selected, function(row) {
    sql <- shortRubric$sqlStatement[[row]]
    rubricId <- as.character(shortRubric$rubric[[row]])
    tryCatch({
      counted <- lapply(c(positiveIds[[rubricId]], negativeIds[[rubricId]]), function(cohortId) {
        built <- render(sql, cohortId)
        list(sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM (", probeStripSemicolon(built$sql), ") rule_subjects"), unresolved = built$unresolved)
      })
      list(positive = counted[[1]], negative = counted[[2]], error = NULL)
    }, error = function(e) list(error = conditionMessage(e)))
  })
  countTasks <- list()
  taskIndex <- list()
  for (position in seq_along(selected)) {
    if (!is.null(ruleParts[[position]]$error)) next
    row <- selected[[position]]
    label <- paste0(shortRubric$rubric[[row]], "/", shortRubric$ruleId[[row]])
    for (side in c("positive", "negative")) {
      countTasks[[length(countTasks) + 1L]] <- list(kind = "count", sql = ruleParts[[position]][[side]]$sql, label = paste0(label, " ", side))
      taskIndex[[paste0(position, side)]] <- length(countTasks)
    }
  }
  say("Running ", length(countTasks), " count query(ies)", if (!is.null(cluster)) paste0(" on ", workers, " worker(s)") else "")
  countOutcomes <- runTasks(countTasks)

  rows <- list()
  for (position in seq_along(selected)) {
    row <- selected[[position]]
    rubricId <- as.character(shortRubric$rubric[[row]])
    ruleId <- as.character(shortRubric$ruleId[[row]])
    score <- suppressWarnings(as.numeric(shortRubric$score[[row]]))
    type <- as.character(shortRubric$type[[row]])
    direction <- if (!is.na(score) && score < 0 || type %in% c("hard_exclusion", "alternativeExplanations")) -1L else 1L
    status <- "ok"
    message <- ""
    seconds <- NA_real_
    comparison <- NULL
    hitsPos <- hitsNeg <- NA_real_
    nPos <- sizes$persons[match(positiveIds[[rubricId]], sizes$cohortDefinitionId)]
    nNeg <- sizes$persons[match(negativeIds[[rubricId]], sizes$cohortDefinitionId)]
    parts <- ruleParts[[position]]
    if (!is.null(parts$error)) {
      status <- "error"
      message <- parts$error
    } else {
      pos <- countOutcomes[[taskIndex[[paste0(position, "positive")]]]]
      neg <- countOutcomes[[taskIndex[[paste0(position, "negative")]]]]
      seconds <- round(sum(pos$seconds, neg$seconds, na.rm = TRUE), 1)
      if (!isTRUE(pos$ok) || !isTRUE(neg$ok)) {
        status <- "error"
        message <- paste(c(pos$error, neg$error), collapse = " | ")
      } else {
        hitsPos <- pos$value
        hitsNeg <- neg$value
        comparison <- tryCatch(
          probeCompareRates(hitsPos, nPos, hitsNeg, nNeg, direction),
          error = function(e) {
            status <<- "error"
            message <<- conditionMessage(e)
            NULL
          }
        )
        unresolved <- unique(c(parts$positive$unresolved, parts$negative$unresolved))
        if (!is.null(comparison) && comparison$verdict == "no hits") message <- "no hits in either population"
        if (length(unresolved) > 0L) {
          message <- paste("unresolved concept sets used a dummy concept:", paste(unresolved, collapse = ", "))
        }
      }
    }
    rows[[length(rows) + 1L]] <- cbind(
      data.frame(
        rubric = rubricId, ruleId = ruleId, type = type, score = score, criteria = as.character(shortRubric$criteria[[row]]),
        personsPositive = nPos, hitsPositive = hitsPos, personsNegative = nNeg, hitsNegative = hitsNeg,
        stringsAsFactors = FALSE
      ),
      if (is.null(comparison)) data.frame(
        proportionPositive = NA_real_, proportionNegative = NA_real_, riskDifference = NA_real_, likelihoodRatio = NA_real_,
        lrLower = NA_real_, lrUpper = NA_real_, logLr = NA_real_, direction = direction, verdict = "error", stringsAsFactors = FALSE
      ) else comparison,
      data.frame(status = status, message = substr(gsub("[\r\n]+", " ", message), 1, 300), seconds = seconds, stringsAsFactors = FALSE)
    )
    say("  ", rubricId, "/", ruleId, " (score ", score, "): ", if (status == "ok") sprintf(
      "positive %.4f%% vs negative %.4f%%, likelihood ratio %.2f (%.2f-%.2f), %s", 100 * rows[[length(rows)]]$proportionPositive,
      100 * rows[[length(rows)]]$proportionNegative, rows[[length(rows)]]$likelihoodRatio, rows[[length(rows)]]$lrLower,
      rows[[length(rows)]]$lrUpper, rows[[length(rows)]]$verdict
    ) else paste0("error - ", substr(message, 1, 150)), " (", seconds, "s)")
  }

  results <- if (length(rows) > 0L) do.call(rbind, rows) else NULL
  usable <- if (is.null(results)) integer(0) else which(is.finite(results$logLr) & is.finite(results$score))
  if (length(usable) >= 3L) {
    say("Rank correlation between score and log likelihood ratio across ", length(usable), " rules: ",
        round(stats::cor(results$score[usable], results$logLr[usable], method = "spearman"), 2))
  }

  normalized <- NULL
  if (isTRUE(normalize) && !is.null(results)) {
    normalized <- tryCatch(
      probeNormalizeScores(results, capLr = capLr, verbose = verbose),
      error = function(e) {
        say("Scores were not normalized: ", conditionMessage(e))
        NULL
      }
    )
  }

  savedTo <- NULL
  if (isTRUE(saveResults) && !is.null(results)) {
    folder <- file.path(outputDirectory, "rubricEvidenceProbe")
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    savedTo <- file.path(folder, paste0(conditionName, "_rulePopulations_", databaseId, "_", stamp, ".csv"))
    utils::write.csv(results, savedTo, row.names = FALSE)
    if (!is.null(normalized)) {
      normalizedPath <- file.path(folder, paste0(conditionName, "_normalizedScores_", databaseId, "_", stamp, ".csv"))
      utils::write.csv(normalized, normalizedPath, row.names = FALSE)
      savedTo <- c(results = savedTo, normalized = normalizedPath)
    }
    say("Saved: ", paste(savedTo, collapse = " ; "))
  }

  list(results = results, normalized = normalized, populations = sizes, savedTo = savedTo, table = if (isTRUE(keepTable)) qualified else NULL)
}

#' Rescale rule scores to -scaleMax..scaleMax from the population results, next to the original scores
#'
#' Every rule that was tested (ER, HE and AE) gets a score proportional to the log of its likelihood
#' ratio between the positive and negative populations. A ratio above 1 scores positive and a ratio
#' below 1 scores negative, with `capLr` (or, when `capLr` is not finite, the strongest rule) at
#' `scaleMax` or `-scaleMax`; a ratio of 1 scores 0. A second, conservative score uses the end of the
#' ratio's 95 percent interval nearest 1 (0 when the interval includes 1). Rules that errored are
#' left out with a message.
#'
#' @param results Output of `probeRulePopulations()`, its `results` data frame, a path to a saved
#'   `_rulePopulations_` CSV, or `NULL` to read the latest saved one for `conditionName`.
#' @param conditionName Condition name, needed only when `results` is `NULL`.
#' @param scaleMax Size of the new scale (default 6).
#' @param capLr Likelihood ratio that earns `scaleMax`; stronger rules are capped there so one extreme
#'   rule does not squash the rest. Use `Inf` to scale by the strongest rule instead.
#' @param digits Decimal places for the new scores.
#' @param configPath Path to config.yaml (only to find the output directory).
#' @param outputDirectory Root output directory; `NULL` uses the config's.
#' @param verbose Print what was left out.
#' @return Data frame, one row per rescaled rule: `score` (original), `normalizedScore`,
#'   `normalizedScoreConservative`, `scoreDifference`, `originalRank`, `normalizedRank` (within rubric,
#'   1 = highest score), and the likelihood ratio, hits, proportions and verdict.
probeNormalizeScores <- function(results = NULL,
                                   conditionName = NULL,
                                   scaleMax = 6,
                                   capLr = 100,
                                   digits = 1L,
                                   configPath = "config.yaml",
                                   outputDirectory = NULL,
                                   verbose = TRUE) {
  say <- function(...) Ithaca:::ithMaybeMessage(verbose, ...)
  if (is.list(results) && !is.data.frame(results) && !is.null(results$results)) results <- results$results
  if (is.null(results) || is.character(results)) {
    path <- results
    if (is.null(path)) {
      if (is.null(conditionName)) stop("Give results, or conditionName to read the latest saved results")
      if (is.null(outputDirectory)) {
        config <- suppressMessages(Ithaca:::ithReadIthacaWorkflowConfig(configPath))
        outputDirectory <- as.character(config$outputDirectory[[1]])
      }
      folder <- file.path(outputDirectory, "rubricEvidenceProbe")
      files <- sort(list.files(folder, pattern = "_rulePopulations_.*\\.csv$", full.names = TRUE))
      files <- files[startsWith(basename(files), paste0(conditionName, "_rulePopulations_"))]
      if (length(files) == 0L) stop("No saved rule population results found for '", conditionName, "' in ", folder)
      path <- files[[length(files)]]
    }
    say("Reading ", path)
    results <- utils::read.csv(path, stringsAsFactors = FALSE)
  }
  missingColumns <- setdiff(c("rubric", "ruleId", "score", "logLr", "lrLower", "lrUpper", "status"), names(results))
  if (length(missingColumns) > 0L) stop("results is missing column(s): ", paste(missingColumns, collapse = ", "))

  failed <- results$status != "ok" | !is.finite(results$logLr)
  if (any(failed)) {
    say(
      "Excluded ", sum(failed), " rule(s) with errors: ",
      paste0(results$rubric[failed], "/", results$ruleId[failed], " (", ifelse(nzchar(results$message[failed]), results$message[failed], results$status[failed]), ")", collapse = "; ")
    )
  }
  er <- results[!failed, , drop = FALSE]
  if (nrow(er) == 0L) stop("No rules with usable results")

  top <- if (is.finite(capLr)) log(capLr) else max(abs(er$logLr))
  rescale <- function(logLr) {
    clipped <- sign(logLr) * pmin(abs(logLr), if (is.finite(capLr)) log(capLr) else Inf)
    if (top <= 0) rep(0, length(clipped)) else scaleMax * clipped / top
  }
  # Interval end nearest 1 on the log scale; 0 when the interval includes 1.
  nearestToOne <- ifelse(er$lrLower > 1, log(er$lrLower), ifelse(er$lrUpper < 1, log(er$lrUpper), 0))
  er$normalizedScore <- round(rescale(er$logLr), digits)
  er$normalizedScoreConservative <- round(rescale(nearestToOne), digits)
  er$scoreDifference <- er$normalizedScore - er$score
  er$originalRank <- stats::ave(-er$score, er$rubric, FUN = function(x) rank(x, ties.method = "min"))
  er$normalizedRank <- stats::ave(-er$normalizedScore, er$rubric, FUN = function(x) rank(x, ties.method = "min"))
  er <- er[order(er$rubric, Ithaca:::ithRuleIdOrder(er$ruleId)), , drop = FALSE]

  say(
    "Scale: likelihood ratio 1 = 0, ", if (is.finite(capLr)) paste0(capLr, " or more") else "the strongest rule", " = ", scaleMax,
    ", below 1 negative (down to -", scaleMax, "); ", sum(er$normalizedScore > 0), " rule(s) positive, ",
    sum(er$normalizedScore < 0), " negative, ", sum(er$normalizedScore == 0), " at 0"
  )
  signDisagrees <- sign(er$normalizedScore) != sign(er$score) & er$normalizedScore != 0 & er$score != 0
  if (any(signDisagrees)) {
    say("Sign differs from the original score for: ", paste0(er$rubric[signDisagrees], "/", er$ruleId[signDisagrees], collapse = ", "))
  }
  if (nrow(er) >= 3L) {
    say("Rank correlation between original and normalized scores: ", round(stats::cor(er$score, er$normalizedScore, method = "spearman"), 2))
  }
  keep <- c(
    "rubric", "ruleId", "score", "normalizedScore", "normalizedScoreConservative", "scoreDifference", "originalRank",
    "normalizedRank", "likelihoodRatio", "lrLower", "lrUpper", "hitsPositive", "hitsNegative", "proportionPositive",
    "proportionNegative", "verdict", "criteria"
  )
  out <- er[, intersect(keep, names(er)), drop = FALSE]
  rownames(out) <- NULL
  out
}

probeNormalizeErScores <- probeNormalizeScores
