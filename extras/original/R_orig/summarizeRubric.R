library(jsonlite)
library(SqlRender)
library(DatabaseConnector)
library(Helios)
library(dplyr)
library(data.table)
library(stringr)

options(scipen = 999)

codeFolder <- "PhenotypeLibrary"
dataFolder <- "ProjectData/PheValuator"
drive <- "p"

source(paste0(drive, ":/", codeFolder, "/cdmSettings.R"), echo = FALSE)
source(paste0("./functionsPhenotype.R"), echo = FALSE)

conditionList <- list(list(condition = "multipleMyeloma", rubricName = "Multiple myeloma", cohortId = 15726),
                      list(condition = "bladderCancer", rubricName = "Bladder cancer", cohortId = 23784),
                      list(condition = "atrialFibrillation", rubricName = "Atrial fibrillation", cohortId = 23896),
                      list(condition = "majorDepressiveDisorder", rubricName = "Major Depressive Disorder", cohortId = 24046),
                      list(condition = "neutropenia", rubricName = "Neutropenia", cohortId = 24047),
                      list(condition = "urinaryTractInfection", rubricName = "Urinary tract infection", cohortId = 24048),
                      list(condition = "teStroke", rubricName = "Ischemic stroke", cohortId = 24049),
                      list(condition = "acuteLiverInjury", rubricName = "acute Liver Injury", cohortId = 24050),
                      list(condition = "seizure", rubricName = "Seizure", cohortId = 24051),
                      list(condition = "atopicDermatitis", rubricName = "Moderate to Severe Atopic dermatitis", cohortId = 24052))

# testCondition <- list(condition =  "atopicDermatitis", rubricName = "Moderate to Severe Atopic dermatitis", cohortId = 99999)
#
# conditionList <- list(testCondition)

summaryDir <- file.path(paste0(drive, ":"), "llm/rubrics/pears")
if (!dir.exists(summaryDir)) {
  dir.create(summaryDir, recursive = TRUE)
}

results <- NULL
for(conditionUp in 1:length(conditionList)) {
  cat(paste0("\nCreating rubric summary for ", conditionList[[conditionUp]]$rubricName, "\n"))

  scoringDir <- file.path(paste0(drive,":"),"llm/rubrics",conditionList[[conditionUp]]$rubricName)

  latestRubricFile <- findLatestFileVersion(scoringDir, paste0(conditionList[[conditionUp]]$rubricName , "_conciseRubric.csv"))
  conciseRubric <- read.csv(file.path(scoringDir, latestRubricFile))
  if(!is.data.frame(conciseRubric)) { #convert from list
    conciseRubric <- conciseRubric[[1]]
  }
  conciseRubric <- conciseRubric[with(conciseRubric, order(rubric, ruleId)),]

  originalPrompt <- paste(readLines("./docs/summarizeConciseRubric.txt"), collapse = "\n")

  originalLines <- originalPrompt
  updated_lines <- gsub("<CONCISE_RUBRIC>", toJSON(conciseRubric), originalPrompt)

  if(conditionUp == 1) {
    updated_lines <- gsub("<EXAMPLE_TEXT_RULE>", "", updated_lines)
    updated_lines <- gsub("<EXAMPLE_TEXT>", "", updated_lines)
  } else {
    exampleRule <- "Here is example text for your text.  Follow the format of the example text PRECISELY as show especially headers/subheaders. \n\n Sample Text: \n"
    updated_lines <- gsub("<EXAMPLE_TEXT_RULE>", "", updated_lines)
    updated_lines <- gsub("<EXAMPLE_TEXT>", results, updated_lines)
  }

  prompt <- paste(updated_lines, collapse = "\n")

  results <- queryLLM(llmClient = getLlmo3Client(), prompt = prompt, output = "text")

  writeLines(results, file.path(summaryDir, paste0(conditionList[[conditionUp]]$rubricName, "_RuleSummary.txt")))

}


