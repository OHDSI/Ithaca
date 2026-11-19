library(jsonlite)
library(SqlRender)
library(DatabaseConnector)
library(ellmer)
library(dplyr)

options(scipen = 999)

codeFolder <- "PhenotypeLibrary"
dataFolder <- "ProjectData/PheValuator"
drive <- "p"

source(paste0(drive, ":/", codeFolder, "/cdmSettings.R"), echo = FALSE)
source(paste0("./functionsPhenotype.R"), echo = FALSE)

conditionUp <- "Bipolar disorder I"

scoringDir <- file.path(paste0(drive,":"),"llm/rubrics",conditionUp)

latestRubricFile <- findLatestFileVersion(scoringDir, paste0(conditionUp , "_shortRubric.rds"))
shortRubric <- readRDS(file.path(scoringDir, latestRubricFile))$shortRubric

##### select rule to change
ruleUp <- which(shortRubric$ruleId == "ER1")
#####

temp <- data.frame(
  condition = shortRubric$condition[[ruleUp]],
  rubric = shortRubric$rubric[[ruleUp]],
  rubricName = shortRubric$rubricName[[ruleUp]],
  rubricYesScore = shortRubric$rubricYesScore[[ruleUp]],
  rubricNoScore = shortRubric$rubricNoScore[[ruleUp]],
  ruleId = shortRubric$ruleId[[ruleUp]],
  type = shortRubric$type [[ruleUp]],
  score = shortRubric$score[[ruleUp]],
  criteria = shortRubric$criteria[[ruleUp]],
  rationale = shortRubric$rationale[[ruleUp]],
  neededConceptSets = shortRubric$neededConceptSets[[ruleUp]],
  stringsAsFactors = FALSE
)


############
##### change values here
temp$criteria <- "1 inpatient OR emergency department diagnosis of bipolar disorder I within -160 to +160 of index OR 2 outpatient diagnoses of bipolar disorder I within -60 to +60 of index separated by 1-60 days"
###########

#create sql statement (don't do manually)
sql <- createSqlStatement(criteria = temp$criteria, conceptSets = temp$neededConceptSets, llmClient = getLlmo3Client())

temp$sqlStatement <- sql
temp$okToUse <- TRUE


shortRubric[ruleUp, ] <- temp[1, ]
# cat(shortRubric$sqlStatement[ruleUp], file = "clipboard")

savedFile <- saveVersionedFile(saveDirectory = file.path(dir_path),
                               objectToSave = list(shortRubric = shortRubric),
                               fileName = paste0(conditionUp, "_shortRubric.rds"))

conciseRubric <- shortRubric[,c(1:12)]
savedFile <- saveVersionedFile(saveDirectory = file.path(dir_path),
                               objectToSave = conciseRubric,
                               fileName = paste0(conditionUp, "_conciseRubric.csv"))

