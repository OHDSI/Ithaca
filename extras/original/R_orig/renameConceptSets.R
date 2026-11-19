acuteLiverFailure <- list(testCondition = "acute liver failure",
                          dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                          additionalInformation = "")

conditionList <- list(acuteLiverFailure)

database <- CCAE_OHDA_DBSpec

outputDirectory <- file.path(paste0(drive, ":"), "llm/rubrics")

conditionUp <- 1

conditionRubricDir <- file.path(outputDirectory, conditionList[[conditionUp]]$testCondition)

# if(is.null(existingMatchedConcepts) | createNew == TRUE) { #create matched concept sets
cat(paste0("Correcting concept sets for: ", conditionList[[conditionUp]]$testCondition, "\n"))

# Extract required concept sets from rubrics
latestRubricFile <- findLatestFileVersion(conditionRubricDir, "rubricList.rds")

neededConceptSets <- readRDS(file.path(conditionRubricDir, latestRubricFile))$neededConcepts
# change the needed concept names in the needed concept set df
neededConceptSets$conceptSetsNeeded <- gsub("acute_liver_failure_condition_codes",
                                            "liver_failure_condition_codes", neededConceptSets$conceptSetsNeeded,
                                            fixed = TRUE)

# change the needed concept names in the rubric list
fullRubric <- readRDS(file.path(conditionRubricDir, latestRubricFile))$rubricList

for(ruleUp in 1:length(fullRubric)) {
  fullRubric[[ruleUp]]$hard_exclusions$concept_sets_needed <- gsub("acute_liver_failure_condition_codes",
                                                                   "liver_failure_condition_codes",
                                                                   fullRubric[[ruleUp]]$hard_exclusions$concept_sets_needed,
                                                                   fixed = TRUE)

  fullRubric[[ruleUp]]$alternative_explanations$concept_sets_needed <- gsub("acute_liver_failure_condition_codes",
                                                                            "liver_failure_condition_codes",
                                                                            fullRubric[[ruleUp]]$alternative_explanations$concept_sets_needed,
                                                                            fixed = TRUE)

  fullRubric[[ruleUp]]$evidence_rules$concept_sets_needed <- gsub("acute_liver_failure_condition_codes",
                                                                  "liver_failure_condition_codes",
                                                                  fullRubric[[ruleUp]]$evidence_rules$concept_sets_needed,
                                                                  fixed = TRUE)
}

#update the rubric list file
latestRubricFile <- findLatestFileVersion(conditionRubricDir, "rubricList.rds")

saveVersionedFile(conditionRubricDir,
                  objectToSave = list(rubricList = fullRubric, neededConcepts = neededConceptSets),
                  fileName = "rubricList.rds")

# change the needed concept names in the short rubric df
latestShortRubricFile <- findLatestFileVersion(conditionRubricDir, "acute liver failure_shortRubric.rds")
shortRubric <- readRDS(file.path(conditionRubricDir, latestShortRubricFile))$shortRubric

#change in concept sets needed
shortRubric$neededConceptSets <- gsub("acute_liver_failure_condition_codes",
                                      "liver_failure_condition_codes",
                                      shortRubric$neededConceptSets,
                                      fixed = TRUE)

#change in the sql statements
shortRubric$sqlStatement <- gsub("acute_liver_failure_condition_codes",
                                 "liver_failure_condition_codes",
                                 shortRubric$sqlStatement,
                                 fixed = TRUE)

saveVersionedFile(conditionRubricDir,
                  objectToSave = list(shortRubric = shortRubric),
                  fileName = "acute liver failure_shortRubric.rds")


# change the needed concept sets in the concise file
latestConciseRubricFile <- findLatestFileVersion(conditionRubricDir, "acute liver failure_conciseRubric.csv")
conciseRubric <- read.csv(file.path(conditionRubricDir, latestConciseRubricFile))
conciseRubric$neededConceptSets <- gsub("acute_liver_failure_condition_codes",
                                      "liver_failure_condition_codes",
                                      conciseRubric$neededConceptSets,
                                      fixed = TRUE)

saveVersionedFile(conditionRubricDir,
                  objectToSave = conciseRubric,
                  fileName = "acute liver failure_conciseRubric.csv")
