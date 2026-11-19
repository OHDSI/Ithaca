#correcting issues if needed

#use to change the rubric parameters AFTER editing conconciseRubric csv file (if changes are required) then run this
#will scan across all the conditions in the conditions file and only change the ones that were changed
#use for changes to the concise rubric file
source("applyConciseRubricEditsFromConfig.R")
#use for changes to the concise rubric review file
source("applyConciseRubricReviewsFromConfig.R")

#use this to re-score a previously scored file quickly without sql queries - only for changes in yes/no threshold
#and individual rule scoring changes - for others (adding/deleting rules), re-run full scoring
source("backfillScoringCaseLabelsFromConfig.R")

#use to combine the criteria and rationale from the concise rubric file with the scored rules for each subject in random cohort
combineFullScoringFilesFromConfig(
  configPath = "config.yaml",
  outputDirectory = "P:/llm/rubrics/combinedScoring"
)

#use to check that all the concepts sets needed for a run are accounted for
csCheck <- checkSqlConceptSetsInNeededConceptsFromConfig()

#use this to review a single conditions - this uses the config.yaml for its information but overrides can be done
  reviewConciseRubricsFromConfig(c("Interstitial lung disease"))

#to apply changes from edits to rubric and those accepted in the rubric review file
  applyPendingRubricChangesFromConfig("COPD Exacerbation")

#use to clean up the needed concept set names in the rubrics (for misformed names)
results <- cleanRubricConceptSetNamesFromConfig()

#qc functions
#get a resolved query to test
ruleSql <- getRubricRuleSqlFromConfig(
  conditionName = "Rheumatoid arthritis",
  rubricId = "R1",
  ruleId = "ER1"
)

cat(ruleSql$sql, file = "clipboard")

review <- reviewConciseRubric("Major Depressive Disorder")

#use to add ae1 (no conditions test) to an old rubric
addZeroOccurrencePenaltyRule()

#QUICK SCORE CHANGE --
#changing scoring after sql query after concise rubric edit
source("applyConciseRubricEditsFromConfig.R")
source("backfillScoringCaseLabelsFromConfig.R")
