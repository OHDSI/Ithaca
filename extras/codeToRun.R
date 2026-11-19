devtools::load_all("E:/Ithaca")

message(Sys.time())

#use to create rubrics for all the conditions in your conditionSpec.csv file (or individual ones if csv file is null)
source("getRubricsFromConfig.R")

#NOTE: best to check the conciseRubric now to provide human-in-the-loop verification that the rules
#were created in a clinically sound way.

#one way to start is to have the llm review the rubrics using
# source("reviewConciseRubricsFromConfig.R")
#it will make suggestions and allow you to make changes easily (TODO)

#Other changes can be made by editing the conciseRubric_VN.csv file
#to apply changes, see correction method in extras/rubricHelpFunctions

#use to create the concept sets needed for all the rubrics just created
source("createRubricConceptSetsFromConfig.R")

#use to create the rules's scores from a data-driven approach (replacing the llm determined scores)
# applyDataDrivenRuleScoresFromConfig()

#use to build the evaluation cohort
source("buildRandomOmopCohortMultipleDatabases.R")

#use to score the evaluation cohort
source("scoreSubjectsMultipleDatabases.R")

#use to combine the criteria and rationale from the concise rubric file with the scored rules for each subject in random cohort
# combineFullScoringFilesFromConfig(
#   configPath = "config.yaml",
#   outputDirectory = "P:/llm/rubrics/combinedScoring"
# )

#use to summarize the scoring based on subjects with one or more rules followed
summarizeRuleCombinationsFromConfig()

#use to compare the designated cohort definitions with the evaluation cohort
source("testPhenotypeAlgorithmRubricReportMultipleDatabases.R")

#use to run the 3 steps: build random cohorts, score subjects, and test phenotypes
# plan <- runIthacaPipeline(adoptExisting = TRUE)

message(Sys.time())
