# Re-label scoring for every condition and database after editing rule scores and/or yes/no
# thresholds, without re-querying the databases. Run applyConciseRubricEdits() first.
# Not valid if rules were added or removed or any criteria changed; rerun scoreSubjectsMultipleDatabases.R.
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("backfillScoringCaseLabelsFromConfig.R")

library(Ithaca)

config <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")

databaseIds <- names(config$cdm)
if (length(databaseIds) == 0L) {
  stop("config.yaml must define at least one database under general.cdm")
}

results <- list()
for (conditionSpec in config$conditionSpecs) {
  conditionName <- as.character(
    Ithaca:::ithCoalesce(conditionSpec$rubricName, conditionSpec$testCondition)[[1]]
  )

  for (databaseId in databaseIds) {
    results[[paste(conditionName, databaseId, sep = "::")]] <- backfillScoringCaseLabels(
      conditionName = conditionName,
      databaseId = databaseId,
      randomCohortDefinitionId = conditionSpec$randomCohortDefinitionId,
      outputDirectory = as.character(config$outputDirectory[[1]]),
      saveSummary = TRUE,
      verbose = isTRUE(Ithaca:::ithCoalesce(config$verbose, TRUE)),
      recomputeFromRuleScores = TRUE,
      skipIfUnchanged = TRUE
    )
  }
}

skipped <- vapply(results, function(result) isTRUE(result$skipped), logical(1))
message(sum(!skipped), " updated, ", sum(skipped), " skipped (no change).")

str(results, max.level = 2)
