# Applies data-driven rule scores to a condition's concise rubric. The evidence comes from
# ithacaDataDrivenEvidence.R.

ithDataDrivenDefaults <- function() {
  list(
    databaseIds = NULL,
    cdm = NULL,
    capLr = 10,
    scaleMax = 6,
    yesScore = 10,
    noScore = 2,
    gate = list(positiveYesMax = 0.95, positiveYesMin = 0.5, negativeYesMax = 0.10, separationMin = 0.30),
    excludeRuleIds = c("ER1", "ER2", "AE1"),
    personModulus = 1L,
    matrixPersons = 200000L,
    workers = NULL,
    reuseQueryResults = FALSE
  )
}

# Layers settings on top of each other; a layer's gate and cdm entries are merged by name, and a
# NULL (YAML null) gate limit removes that check.
ithDataDrivenMergeSettings <- function(settings, layer, where = "dataDrivenScoring") {
  if (length(layer) == 0L) {
    return(settings)
  }
  known <- names(ithDataDrivenDefaults())
  unknown <- setdiff(names(layer), c(known, "conditions"))
  if (length(unknown) > 0L) {
    stop("Unknown ", where, " setting(s): ", paste(unknown, collapse = ", "), ". Known settings: ", paste(known, collapse = ", "), call. = FALSE)
  }
  for (name in setdiff(names(layer), "conditions")) {
    value <- layer[[name]]
    if (name == "gate") {
      settings$gate <- utils::modifyList(settings$gate, as.list(value))
    } else if (name == "cdm") {
      settings$cdm <- utils::modifyList(ithCoalesce(settings$cdm, list()), as.list(value))
    } else if (name %in% c("databaseIds", "excludeRuleIds")) {
      settings[[name]] <- if (is.null(value)) NULL else as.character(unlist(value))
    } else {
      settings[name] <- list(value)
    }
  }
  settings
}

# Settings for one condition: built-in defaults, then config dataDrivenScoring, then
# dataDrivenScoring.conditions.<name>, then `overrides` (arguments of the calling function).
ithDataDrivenSettings <- function(config, conditionName, overrides = list()) {
  settings <- ithDataDrivenDefaults()
  section <- ithCoalesce(config$dataDrivenScoring, list())
  settings <- ithDataDrivenMergeSettings(settings, section[setdiff(names(section), "conditions")])
  conditions <- section$conditions
  if (length(conditions) > 0L) {
    index <- match(tolower(trimws(conditionName)), tolower(trimws(names(conditions))))
    if (!is.na(index)) {
      settings <- ithDataDrivenMergeSettings(
        settings, ithCoalesce(conditions[[index]], list()), paste0("dataDrivenScoring.conditions.", names(conditions)[[index]])
      )
    }
  }
  overrides <- overrides[!vapply(overrides, is.null, logical(1))]
  if (length(overrides[["gate"]]) == 0L) overrides[["gate"]] <- NULL
  settings <- ithDataDrivenMergeSettings(settings, overrides, "argument")
  ithDataDrivenCheckSettings(settings, conditionName)
}

ithDataDrivenCheckSettings <- function(settings, conditionName = "") {
  label <- if (nzchar(conditionName)) paste0(" for condition '", conditionName, "'") else ""
  single <- function(name, lower = -Inf, strictLower = FALSE) {
    value <- settings[[name]]
    bad <- !is.numeric(value) || length(value) != 1L || is.na(value) || (if (strictLower) value <= lower else value < lower)
    if (bad) stop("dataDrivenScoring ", name, " must be a single number", if (is.finite(lower)) paste0(" ", if (strictLower) "above " else "of at least ", lower), label, call. = FALSE)
  }
  single("capLr", 1, strictLower = TRUE)
  single("scaleMax", 0, strictLower = TRUE)
  single("yesScore")
  single("noScore")
  if (settings$yesScore <= settings$noScore) {
    stop("dataDrivenScoring yesScore must be greater than noScore", label, call. = FALSE)
  }
  single("personModulus", 1)
  single("matrixPersons", 2)
  if (!is.logical(settings$reuseQueryResults) || length(settings$reuseQueryResults) != 1L || is.na(settings$reuseQueryResults)) {
    stop("dataDrivenScoring reuseQueryResults must be true or false", label, call. = FALSE)
  }
  for (name in names(settings$gate)) {
    value <- settings$gate[[name]]
    if (!(name %in% names(ithDataDrivenDefaults()$gate))) {
      stop("Unknown dataDrivenScoring gate limit '", name, "'", label, ". Known limits: ", paste(names(ithDataDrivenDefaults()$gate), collapse = ", "), call. = FALSE)
    }
    if (!is.null(value) && !is.na(value) && (!is.numeric(value) || length(value) != 1L || value < 0 || value > 1)) {
      stop("dataDrivenScoring gate limit ", name, " must be a share between 0 and 1 (or null to skip it)", label, call. = FALSE)
    }
  }
  settings
}

# Writes the proposed scores (and types, following the sign of the score) into the latest concise rubric
# and applies it as an edit. A retyped rule gets a new ID and keeps its SQL. Returns the number of
# rules rescaled and retyped.
ithDataDrivenApplyScores <- function(conditionName, outputDirectory, recs, llmClient, promptDir, adjustableRuleId, updateSummary, verbose) {
  changes <- recs[recs$recommendation %in% c("RESCALE", "CHANGE_TYPE"), , drop = FALSE]
  if (nrow(changes) == 0L) {
    return(list(rescaled = 0L, retyped = 0L))
  }
  concise <- ithLoadLatest(file.path(outputDirectory, conditionName), paste0(conditionName, "_conciseRubric.csv"))
  if (is.null(concise)) stop("No concise rubric found for condition: ", conditionName, call. = FALSE)
  row <- match(
    paste(changes$rubric, toupper(trimws(changes$ruleId))),
    paste(concise$rubric, toupper(trimws(as.character(concise$ruleId))))
  )
  if (anyNA(row)) {
    stop("Rules not found in the latest concise rubric: ", paste(changes$ruleId[is.na(row)], collapse = ", "), call. = FALSE)
  }
  retyped <- changes$proposedType != changes$type
  concise$score[row] <- changes$proposedScore
  concise$type[row] <- changes$proposedType

  editedPath <- tempfile(pattern = paste0(conditionName, "_dataDrivenEdit_"), fileext = ".csv")
  on.exit(unlink(editedPath), add = TRUE)
  utils::write.csv(concise, editedPath, row.names = FALSE)
  ithApplyConciseRubricEdits(
    conditionName = conditionName,
    outputDirectory = outputDirectory,
    editedConciseRubricPath = editedPath,
    llmClient = llmClient,
    promptDir = promptDir,
    updateSummary = if (is.null(updateSummary)) any(retyped) else isTRUE(updateSummary),
    verbose = verbose,
    adjustableRuleId = adjustableRuleId,
    repairConceptSetMismatches = FALSE
  )
  list(rescaled = sum(!retyped), retyped = sum(retyped))
}

# One condition: evidence, gate, and (when the gate passes) the rubric edit.
ithDataDrivenApplyOne <- function(conditionName, config, settings, outputDirectory, applyChanges, llmClient, promptDir,
                                  adjustableRuleId, updateSummary, saveResults, verbose) {
  conditionDir <- file.path(outputDirectory, conditionName)
  if (isTRUE(applyChanges)) {
    concise <- ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
    if (!is.null(concise) && isTRUE(ithDiffConciseRubric(concise, ithLoadLatestShortRubric(conditionName, outputDirectory))$differs)) {
      stop(
        "The latest concise rubric has edits that are not applied yet. Run applyConciseRubricEditsFromConfig() first, ",
        "so the scores are measured on the rubric that scoring uses.",
        call. = FALSE
      )
    }
  }

  databaseIds <- settings$databaseIds
  if (is.null(databaseIds)) databaseIds <- unique(c(names(config$cdm), names(settings$cdm)))
  if (length(databaseIds) == 0L) {
    stop("No databases to use: set dataDrivenScoring.databaseIds or general.cdm in the config", call. = FALSE)
  }

  evidence <- ithDataDrivenEvidence(
    conditionName = conditionName, config = config, outputDirectory = outputDirectory, databaseIds = databaseIds,
    cdm = settings$cdm, excludeRuleIds = settings$excludeRuleIds, personModulus = settings$personModulus,
    workers = settings$workers, matrixPersons = settings$matrixPersons, scaleMax = settings$scaleMax, capLr = settings$capLr,
    yesScore = settings$yesScore, noScore = settings$noScore, gate = settings$gate,
    reuseQueryResults = settings$reuseQueryResults, saveResults = saveResults, verbose = verbose
  )
  recs <- evidence$recommendations
  notScored <- sum(recs$skipped)

  status <- "reportOnly"
  detail <- ""
  rescaled <- 0L
  retyped <- 0L
  if (isTRUE(applyChanges)) {
    failedRows <- evidence$gate$table[!evidence$gate$table$passed, , drop = FALSE]
    if (!isTRUE(evidence$gate$passed)) {
      status <- "gateFailed"
      detail <- paste(paste0(failedRows$database, " ", failedRows$rubric, ": ", failedRows$reason), collapse = " | ")
    } else if (!any(recs$recommendation %in% c("RESCALE", "CHANGE_TYPE"))) {
      status <- "noChanges"
    } else {
      applied <- ithDataDrivenApplyScores(conditionName, outputDirectory, recs, llmClient, promptDir, adjustableRuleId, updateSummary, verbose)
      status <- "applied"
      rescaled <- applied$rescaled
      retyped <- applied$retyped
    }
  }
  ithMaybeMessage(
    verbose, "Data-driven scores for '", conditionName, "': ", status,
    if (status == "applied") paste0(" (", rescaled, " rescaled, ", retyped, " retyped)") else "",
    if (nzchar(detail)) paste0(": ", detail) else "",
    if (notScored > 0L) paste0(". ", notScored, " rule(s) could not be scored from the data and keep their score; see the report.") else ""
  )
  list(
    row = data.frame(
      conditionName = conditionName, status = status, rescaled = rescaled, retyped = retyped,
      notScored = notScored, detail = detail, stringsAsFactors = FALSE
    ),
    evidence = evidence
  )
}

ithDataDrivenRunConditions <- function(conditionNames, config, outputDirectory, overrides, applyChanges, llmClient, promptDir,
                                       adjustableRuleId, updateSummary, saveResults, verbose) {
  results <- lapply(seq_along(conditionNames), function(index) {
    conditionName <- conditionNames[[index]]
    if (length(conditionNames) > 1L) {
      ithMaybeMessage(verbose, "==== Condition: ", conditionName, " (", index, "/", length(conditionNames), ") ====")
    }
    spec <- tryCatch(ithFindConfigConditionSpec(config, conditionName), error = function(e) NULL)
    tryCatch({
      settings <- ithDataDrivenSettings(config, conditionName, overrides)
      ithDataDrivenApplyOne(
        conditionName = conditionName, config = config, settings = settings, outputDirectory = outputDirectory,
        applyChanges = applyChanges,
        llmClient = if (is.null(llmClient)) ithGetLlmClientFromConfig(config, "reasoning") else llmClient,
        promptDir = if (is.null(promptDir)) config$promptDir else promptDir,
        adjustableRuleId = if (!is.null(adjustableRuleId)) adjustableRuleId else if (!is.null(spec)) spec$adjustableRuleId,
        updateSummary = updateSummary, saveResults = saveResults, verbose = verbose
      )
    }, error = function(e) {
      where <- if (is.null(conditionCall(e))) "" else paste0(" [in: ", substr(deparse(conditionCall(e))[1], 1, 150), "]")
      list(
        row = data.frame(
          conditionName = conditionName, status = paste0("error: ", conditionMessage(e), where), rescaled = 0L, retyped = 0L,
          notScored = NA_integer_, detail = "", stringsAsFactors = FALSE
        ),
        evidence = NULL
      )
    })
  })
  table <- do.call(rbind, lapply(results, function(result) result$row))
  attr(table, "evidence") <- stats::setNames(lapply(results, function(result) result$evidence), conditionNames)
  failed <- startsWith(table$status, "error")
  if (any(failed)) {
    warning(sum(failed), " condition(s) failed; see the status column.", call. = FALSE)
  }
  table
}

#' Set rule scores from the data and apply them to the rubric
#'
#' Runs each rule of the condition's rubric on constructed populations in the databases (people meeting
#' ER2 around their first diagnosis, and people with a differential diagnosis who never have the condition)
#' and proposes each rule's score from its mean likelihood ratio across the databases:
#' `round(scaleMax * ln(LR) / ln(capLr))`, limited to plus or minus `scaleMax`. The type follows the sign
#' (a positive score makes an evidence rule, a negative score an alternative explanation; a score of 0
#' keeps the type). The new scores replace the current ones.
#'
#' Run this after the concept sets are built and matched. Rules that cannot be scored from the data
#' (0 records in both populations, a failed query, or an unresolved concept set) and hard exclusions
#' keep their score; the report lists them. ER1, ER2 and AE1 are never changed.
#'
#' The new scores are applied only if a gate passes in every database: with the proposed scores, cohort
#' members (ER1 + ER2 points) must reach YES within the limits for positives and differential negatives
#' (no ER points) must rarely reach YES. If any limit fails, nothing is changed for that condition.
#'
#' A retyped rule gets a new rule ID and keeps its SQL (see [applyConciseRubricEdits()]). The latest
#' concise rubric must have no unapplied edits.
#'
#' Settings come from the config's `dataDrivenScoring` section (and its `conditions.<name>` overrides);
#' any argument given here overrides them. Built-in defaults: `capLr` 10, `scaleMax` 6, `yesScore` 10,
#' `noScore` 2, `personModulus` 1, `matrixPersons` 200000, `reuseQueryResults` FALSE, `excludeRuleIds`
#' ER1, ER2 and AE1, and gate limits `positiveYesMax` 0.95, `positiveYesMin` 0.5, `negativeYesMax` 0.10
#' and `separationMin` 0.30 (provisional values to tune). `databaseIds` defaults to the databases in
#' `general.cdm` plus those in `cdm`.
#'
#' @param conditionName One or more condition names. Conditions run one after the other; a failure is
#'   reported in the result and the others still run.
#' @param databaseIds Databases to use as evidence; use at least two so a rule's effect has to replicate.
#' @param cdm Named list of database id to CDM schema name, for databases that are not in `general.cdm`.
#' @param capLr Likelihood ratio that earns `scaleMax` points.
#' @param scaleMax Largest absolute score a rule can get from the data.
#' @param yesScore,noScore Thresholds used by the gate. They are not written to the rubric.
#' @param gate Named list of gate limits; unnamed limits keep their default and `NA` skips a check.
#'   Limits are shares: `positiveYesMax`, `positiveYesMin` (cohort members reaching YES),
#'   `negativeYesMax` (differential negatives reaching YES) and `separationMin` (cohort share minus
#'   negative share).
#' @param excludeRuleIds Rule IDs that are not tested.
#' @param personModulus Keep only people whose person_id is a multiple of this number (1 keeps everyone).
#'   Use a prime such as 97 for a quick test.
#' @param matrixPersons Approximate number of sampled persons used for the gate and the report checks.
#' @param workers Parallel worker processes for the SQL; `NULL` uses `general.parallelWorkers.scoring`.
#' @param reuseQueryResults When `TRUE`, use the latest saved query results for each database instead of
#'   running the SQL, if they were made with the same rules, rule SQL, CDM schema, `personModulus` and
#'   `matrixPersons`. Query results are always saved under `rubricEvidenceProbe/queryResults`.
#' @param applyChanges When `FALSE`, only report: nothing is applied.
#' @param llmClient LLM client used when a summary is refreshed; `NULL` uses the config's reasoning client.
#' @param promptDir Optional prompt folder override; `NULL` uses the config's.
#' @param adjustableRuleId Rule IDs re-scored at report time; `NULL` uses the condition's value in the
#'   config's conditionSpecs.
#' @param updateSummary Refresh the concise rubric summary text after applying; `NULL` refreshes it only when
#'   a rule was retyped.
#' @param configPath Path to config.yaml.
#' @param outputDirectory Root output directory; `NULL` uses the config's.
#' @param saveResults When `TRUE`, save the report, the rules table and a flagged copy of the concise
#'   rubric under `<outputDirectory>/rubricEvidenceProbe/`.
#' @param verbose Print progress and the report.
#'
#' @return Invisibly, a data frame with one row per condition: `conditionName`, `status` (`applied`,
#'   `gateFailed`, `noChanges`, `reportOnly` or `error: ...`), `rescaled`, `retyped`, `notScored` and
#'   `detail` (why the gate failed). The attribute `evidence` holds each condition's recommendations,
#'   gate table, report and per-database analysis.
#' @examples
#' \dontrun{
#' # After createRubricConceptSetsFromConfig.R:
#' applyDataDrivenRuleScores("Seizure", databaseIds = c("OptumDod", "Ccae"))
#'
#' # Look only, then reuse the saved queries with a different scale
#' applyDataDrivenRuleScores("Seizure", applyChanges = FALSE)
#' applyDataDrivenRuleScores("Seizure", reuseQueryResults = TRUE, capLr = 5)
#' }
#' @export
applyDataDrivenRuleScores <- function(conditionName,
                                      databaseIds = NULL,
                                      cdm = NULL,
                                      capLr = NULL,
                                      scaleMax = NULL,
                                      yesScore = NULL,
                                      noScore = NULL,
                                      gate = list(),
                                      excludeRuleIds = NULL,
                                      personModulus = NULL,
                                      matrixPersons = NULL,
                                      workers = NULL,
                                      reuseQueryResults = NULL,
                                      applyChanges = TRUE,
                                      llmClient = NULL,
                                      promptDir = NULL,
                                      adjustableRuleId = NULL,
                                      updateSummary = NULL,
                                      configPath = "config.yaml",
                                      outputDirectory = NULL,
                                      saveResults = TRUE,
                                      verbose = TRUE) {
  if (!is.character(conditionName) || length(conditionName) == 0L || anyNA(conditionName)) {
    stop("conditionName must be one or more condition names")
  }
  ithCheckConditionNamesNotConfig(conditionName)
  config <- ithReadIthacaWorkflowConfig(configPath)
  if (is.null(outputDirectory)) outputDirectory <- as.character(config$outputDirectory[[1]])
  overrides <- list(
    databaseIds = databaseIds, cdm = cdm, capLr = capLr, scaleMax = scaleMax, yesScore = yesScore, noScore = noScore,
    gate = gate, excludeRuleIds = excludeRuleIds, personModulus = personModulus, matrixPersons = matrixPersons,
    workers = workers, reuseQueryResults = reuseQueryResults
  )
  results <- ithDataDrivenRunConditions(
    unique(conditionName), config, outputDirectory, overrides, applyChanges, llmClient, promptDir,
    adjustableRuleId, updateSummary, saveResults, verbose
  )
  invisible(results)
}

#' Set rule scores from the data for the conditions in the config
#'
#' Runs [applyDataDrivenRuleScores()] for every condition in the config (or the named ones), using the
#' config's `dataDrivenScoring` section and each condition's `adjustableRuleId`. Run it after the concept
#' sets are built and matched.
#'
#' @param configPath Path to config.yaml.
#' @param conditionNames Optional condition names to run; `NULL` runs every configured condition.
#' @param applyChanges When `FALSE`, only report: nothing is applied.
#'
#' @return Invisibly, a data frame with one row per condition (see [applyDataDrivenRuleScores()]).
#' @examples
#' \dontrun{
#' results <- applyDataDrivenRuleScoresFromConfig("config.yaml", conditionNames = "Seizure")
#' }
#' @export
applyDataDrivenRuleScoresFromConfig <- function(configPath = "config.yaml",
                                                conditionNames = NULL,
                                                applyChanges = TRUE) {
  ithCheckConditionNamesNotConfig(conditionNames)
  config <- ithReadIthacaWorkflowConfig(configPath)
  outputDirectory <- as.character(config$outputDirectory[[1]])
  selectedNames <- vapply(
    config$conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    if (!is.character(conditionNames)) {
      stop("conditionNames must be NULL or a character vector")
    }
    missingNames <- setdiff(conditionNames, selectedNames)
    if (length(missingNames) > 0) {
      stop("Requested conditionNames are not configured: ", paste(missingNames, collapse = ", "))
    }
    selectedNames <- selectedNames[selectedNames %in% conditionNames]
  }
  results <- ithDataDrivenRunConditions(
    selectedNames, config, outputDirectory, list(), applyChanges, NULL, NULL, NULL, NULL, TRUE,
    isTRUE(ithCoalesce(config$verbose, TRUE))
  )
  invisible(results)
}
