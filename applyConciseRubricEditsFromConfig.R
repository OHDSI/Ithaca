# Apply your concise rubric edits for every condition in config.yaml, only where the latest
# <condition>_conciseRubric_V*.csv differs from the latest short rubric.
# Edit the latest concise rubric CSV in place, then run this. Next, rerun the scoring backfill
# (backfillScoringCaseLabelsFromConfig.R), or scoreSubjectsMultipleDatabases.R if rules or criteria changed.
#
# Example:
# devtools::load_all("E:/Ithaca")
# source("applyConciseRubricEditsFromConfig.R")

library(Ithaca)

results <- applyConciseRubricEditsFromConfig("config.yaml")
print(results)
