library(jsonlite)
library(SqlRender)
library(DatabaseConnector)
library(Helios)
library(dplyr)

options(scipen = 999)

codeFolder <- "PhenotypeLibrary"
drive <- "p"

source(paste0(drive, ":/", codeFolder, "/cdmSettings.R"), echo = FALSE)
source(paste0("./functionsPhenotype.R"), echo = FALSE)

atopicDermatitis <- list(testCondition = "Atopic dermatitis", specificCohort = 11088, sensitiveCohort = 4263,
                         excludedConditionCode = NULL, chronic = T, cohortsToTest = list(20739))

conditionList <- list(atopicDermatitis)

parentOutputDirectory <- file.path(paste0(drive, ":"), "llm/rubrics")

shortRubricFile <- findLatestFileVersion(saveDirectory=file.path(parentOutputDirectory, conditionList[[1]]$condition),
                                         fileName = paste0(conditionList[[1]]$condition, "_shortRubric.rds"))
badRow <- 1

rubricData <- readRDS(file.path(parentOutputDirectory, conditionList[[1]]$condition, shortRubricFile))$shortRubric

rubricData$sqlStatement[rubricData$rubric == "R3" & rubricData$ruleId == "ER1"] <-
  rubricData$sqlStatement[rubricData$rubric == "R1" & rubricData$ruleId == "ER1"]

#correct the sql statement if necessary
correctedSql <- correctSqlStatement(rubricData$sqlStatement[[badRow]])

#replace the old sql
rubricData$sqlStatement[[badRow]] <- correctedSql$correctedQuery

shortRubric <- rubricData
toRds <- list(shortRubric)
names(toRds)[1] <- "shortRubric"
saveRDS(object = toRds, file.path(parentOutputDirectory, conditionList[[1]]$testCondition, shortRubricFile))

saveRDS(object = data.frame(shortRubric), file.path(parentOutputDirectory, conditionList[[1]]$testCondition, shortRubricFile))


#############################################
scoringDir <- file.path(paste0(drive,":"),"llm/rubrics",conditionList[[conditionUp]]$rubricName)
shortRubricFile <- findLatestFileVersion(saveDirectory=file.path(scoringDir),
                                         fileName = paste0(conditionList[[1]]$rubricName, "_shortRubric.rds"))

rubricData <- readRDS(file.path(scoringDir, shortRubricFile))$shortRubric
if(!is.data.frame(rubricData)) { #convert from list
  rubricData <- rubricData[[1]]
}

#change scores within short rubric file - either have to re-score to take effect or change fullScoringFile file
rubricUp <- "R1"; ruleUp <- "ER1";
rubricData$score[rubricData$rubric == rubricUp & rubricData$ruleId == ruleUp] <-
  rubricData$score[rubricData$rubric == rubricUp & rubricData$ruleId == ruleUp] + 1

rubricData$score[rubricData$rubric == rubricUp & rubricData$ruleId == ruleUp]

# rubricData$sqlStatement[rubricData$rubric == "R6" & rubricData$ruleId == "ER1"] <-
#   rubricData$sqlStatement[rubricData$rubric == "R7" & rubricData$ruleId == "ER1"]

savedFile <- saveVersionedFile(saveDirectory = file.path(scoringDir),
                               objectToSave = list(shortRubric = rubricData),
                               fileName = paste0(conditionList[[conditionUp]]$rubricName, "_shortRubric.rds"))

#############################################
#change the rubric scoring
scoringDir <- file.path(paste0(drive,":"),"llm/rubrics",conditionList[[conditionUp]]$rubricName)

latestfullScoring <- findLatestFileVersion(cohortDir, paste0("fullScoringFile_Cohort",
                                                             unlist(conditionList[[conditionUp]]$cohortId), ".rds"))
latestScoringFull <- readRDS(file.path(cohortDir, latestfullScoring))

rubricUp <- "R1"; ruleUp <- "ER1";
latestScoringFull$score[latestScoringFull$rubricId == rubricUp & latestScoringFull$ruleId == ruleUp] <-
  latestScoringFull$score[latestScoringFull$rubricId == rubricUp & latestScoringFull$ruleId == ruleUp] + 1

saveVersionedFile(saveDirectory = cohortDir, objectToSave = latestScoringFull,
                  fileName = paste0("fullScoringFile_Cohort", unlist(conditionList[[conditionUp]]$cohortId), ".rds"),
                  replace = F)

#############################################
#change the scores in the file
scoringDir <- file.path(paste0(drive,":"),"llm/rubrics",conditionList[[conditionUp]]$rubricName)
latestRubricFileFull <- findLatestFileVersion(scoringDir, paste0("rubricList.rds"))
fullRubricFile <- readRDS(file.path(scoringDir, latestRubricFileFull))
fullRubric <- fullRubricFile$rubricList

for(rubricUp in seq_along(fullRubric)) {
  if(fullRubric[[rubricUp]]$decision_thresholds$yes == 20) {
    fullRubric[[rubricUp]]$decision_thresholds$yes <- 6
  } else {
    fullRubric[[rubricUp]]$decision_thresholds$yes <- fullRubric[[rubricUp]]$decision_thresholds$yes + 1
  }
}

savedRubric <- saveVersionedFile(scoringDir, list(rubricList = fullRubric, neededConcepts = fullRubricFile$neededConcepts),
                                 "rubricList.rds", replace = F)

savedRubricTxt <- saveVersionedFile(scoringDir, writeListDoc(fullRubric, header = ""), "rubricList.txt", replace = F)
#############################################
scoringDir <- file.path(paste0(drive,":"),"llm/rubrics",conditionList[[conditionUp]]$testCondition)
shortRubricFile <- findLatestFileVersion(saveDirectory=file.path(scoringDir),
                                         fileName = paste0(conditionList[[1]]$testCondition, "_shortRubric.rds"))

rubricData <- readRDS(file.path(scoringDir, shortRubricFile))$shortRubric
if(!is.data.frame(rubricData)) { #convert from list
  rubricData <- rubricData[[1]]
}

rubricUp <- "R5"
ruleUp <- "AE1"
priorDays <- 0
postDays <- 7
sqlStatement <- rubricData$sqlStatement[rubricData$rubric == rubricUp & rubricData$ruleId == ruleUp]
cat(sqlStatement)
{
  originalPrompt <- paste(readLines("./docs/restrictConstraintDates.txt"), collapse = "\n")
  originalLines <- originalPrompt
  updated_lines <- gsub("<SQL_STATEMENT>", sqlStatement, originalPrompt)
  updated_lines <- gsub("<PRIOR_DAYS>", priorDays, updated_lines)
  updated_lines <- gsub("<POST_DAYS>", postDays, updated_lines)

  prompt <- paste(updated_lines, collapse = "\n")

  results <- queryLLM(llmClient = getLlmo3Client(), prompt = prompt, output = "text")
  cat(results)
  rubricData$sqlStatement[rubricData$rubric == rubricUp & rubricData$ruleId == ruleUp] <- results
}

savedFile <- saveVersionedFile(saveDirectory = file.path(scoringDir),
                               objectToSave = list(shortRubric = rubricData),
                               fileName = paste0(conditionList[[conditionUp]]$condition, "_shortRubric.rds"))
