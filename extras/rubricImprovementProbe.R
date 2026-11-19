# Probe: recommend rubric scores from the data in several databases, without changing the rubric.
#
# A thin wrapper over the package's data-driven scoring (R/ithacaDataDrivenEvidence.R). To apply the
# scores use applyDataDrivenRuleScores(). The report, rules CSV and flagged concise rubric copy are
# saved under <outputDirectory>/rubricEvidenceProbe/, and query results under queryResults/.
#
# Example:
#   pkgload::load_all("E:/Ithaca")
#   source("extras/rubricImprovementProbe.R")
#   probe <- probeRubricImprovement("autism")
#   cat(probe$report, sep = "\n")
#   probe$recommendations

#' Recommend rubric scores for one condition (nothing is applied)
#'
#' @param conditionName Condition/rubric name.
#' @param databaseIds Databases to run, in order.
#' @param cdm Named list of database id -> CDM schema name, used for ids not in the config's `general.cdm`.
#' @param excludeRuleIds Rule IDs left out of the analysis (the fixed rules by default).
#' @param personModulus Keep only people whose person_id is a multiple of this number (1 keeps everyone).
#' @param workers Parallel worker processes for the SQL; `NULL` uses `general.parallelWorkers.scoring`.
#' @param matrixPersons Approximate number of sampled persons used for the total-score and PPV checks.
#' @param scaleMax,capLr Score scale: a likelihood ratio of `capLr` earns `scaleMax` points.
#' @param reuseQueryResults When `TRUE`, use the latest saved query results that match the current rules and settings.
#' @param configPath Path to config.yaml.
#' @param outputDirectory Root output directory; `NULL` uses the config's.
#' @param saveResults When `TRUE`, save the report, CSV and the flagged concise rubric copy.
#' @param verbose Print progress.
#' @return A list with `recommendations`, `gate`, `report`, `perDatabase` and `savedTo`.
probeRubricImprovementOne <- function(conditionName,
                                      databaseIds = c("OptumDod", "Ccae"),
                                      cdm = list(OptumDod = "cdm_optum_extended_dod_v4155", Ccae = "cdm_merative_ccae_v4149"),
                                      excludeRuleIds = c("ER1", "ER2", "AE1"),
                                      personModulus = 1L,
                                      workers = NULL,
                                      matrixPersons = 200000L,
                                      scaleMax = 6,
                                      capLr = 10,
                                      reuseQueryResults = FALSE,
                                      configPath = "config.yaml",
                                      outputDirectory = NULL,
                                      saveResults = TRUE,
                                      verbose = TRUE) {
  config <- suppressMessages(Ithaca:::ithReadIthacaWorkflowConfig(configPath))
  if (is.null(outputDirectory)) outputDirectory <- as.character(config$outputDirectory[[1]])
  Ithaca:::ithDataDrivenEvidence(
    conditionName = conditionName, config = config, outputDirectory = outputDirectory, databaseIds = databaseIds, cdm = cdm,
    excludeRuleIds = excludeRuleIds, personModulus = personModulus, workers = workers, matrixPersons = matrixPersons,
    scaleMax = scaleMax, capLr = capLr, gate = Ithaca:::ithDataDrivenDefaults()$gate, reuseQueryResults = reuseQueryResults,
    saveResults = saveResults, verbose = verbose
  )
}

#' Recommend rubric scores for one or more conditions (nothing is applied)
#'
#' Runs the analysis once per condition, one after the other. With one condition the result is that
#' condition's result. With several, a condition that fails is reported and skipped, and the others still run.
#'
#' @param conditionName One condition name or a character vector of them.
#' @param ... Arguments passed on to the single-condition analysis: `databaseIds`, `cdm`,
#'   `excludeRuleIds`, `personModulus`, `workers`, `matrixPersons`, `scaleMax`, `capLr`, `reuseQueryResults`,
#'   `configPath`, `outputDirectory`, `saveResults`, `verbose`.
#' @return For one condition, a list with `recommendations`, `gate`, `report`, `perDatabase` and `savedTo`.
#'   For several, a list with `perCondition` (that list for each condition), `recommendations`
#'   (all conditions, with a `condition` column), `summary` (rule counts by recommendation per condition),
#'   and `failed` (condition and error message).
probeRubricImprovement <- function(conditionName, ...) {
  conditionName <- unique(as.character(conditionName))
  if (length(conditionName) == 1L) {
    return(probeRubricImprovementOne(conditionName, ...))
  }
  perCondition <- list()
  failed <- list()
  for (name in conditionName) {
    message("==== Condition: ", name, " (", match(name, conditionName), "/", length(conditionName), ") ====")
    perCondition[[name]] <- tryCatch(
      probeRubricImprovementOne(name, ...),
      error = function(e) {
        where <- if (is.null(conditionCall(e))) "" else paste0(" [in: ", substr(deparse(conditionCall(e))[1], 1, 150), "]")
        message("Condition '", name, "' failed and was skipped: ", conditionMessage(e), where)
        failed[[name]] <<- paste0(conditionMessage(e), where)
        NULL
      }
    )
  }
  perCondition <- Filter(Negate(is.null), perCondition)
  recommendations <- if (length(perCondition) > 0L) {
    do.call(rbind, lapply(names(perCondition), function(name) cbind(condition = name, perCondition[[name]]$recommendations)))
  }
  summary <- if (!is.null(recommendations)) {
    as.data.frame.matrix(table(recommendations$condition, recommendations$recommendation))
  }
  if (!is.null(summary)) {
    summary <- cbind(condition = rownames(summary), summary, row.names = NULL)
  }
  failedFrame <- if (length(failed) > 0L) data.frame(condition = names(failed), error = unlist(failed), row.names = NULL) else NULL
  if (!is.null(failedFrame)) message("Skipped ", nrow(failedFrame), " condition(s): ", paste(failedFrame$condition, collapse = ", "))
  invisible(list(perCondition = perCondition, recommendations = recommendations, summary = summary, failed = failedFrame))
}
