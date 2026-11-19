# Run phenotype algorithm rubric reports for every condition and database.
# Conditions and phenotype cohort IDs come from conditionSpecsCsvPath in config.yaml.
# The latest sumCohortScores artifact is used for each condition/database pair.
# When phenotypeReportScoring sets rules for a condition, the latest fullScoringFile is
# also re-scored with those rules (sumCohortScores still supplies the subject list).
# Empty or failing phenotype cohorts get a report row with NA metrics and the run continues.
# To run only what changed since the last run, use runIthacaPipeline() (runIthacaPipeline.R).
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("testPhenotypeAlgorithmRubricReportMultipleDatabases.R")

library(Ithaca)

# A session that loaded Ithaca before this function existed reloads the package source.
if (!exists("testPhenotypeAlgorithmRubricReportFromConfig", mode = "function")) {
  message("Reloading Ithaca from E:/Ithaca (testPhenotypeAlgorithmRubricReportFromConfig not loaded).")
  pkgload::load_all("E:/Ithaca", quiet = TRUE)
}

results <- testPhenotypeAlgorithmRubricReportFromConfig(configPath = "config.yaml")

str(results, max.level = 2)
