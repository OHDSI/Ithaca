library(jsonlite)
library(SqlRender)
library(DatabaseConnector)
library(Helios)
library(dplyr)
library(data.table)
library(stringr)
library(RubricConcepts)

options(scipen = 999)

codeFolder <- "PhenotypeLibrary"
dataFolder <- "ProjectData/PheValuator"
drive <- "p"

source(paste0(drive, ":/", codeFolder, "/cdmSettings.R"), echo = FALSE)

llmClient <- ellmer::chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_gpt4o_endpoint")),
  api_version = "2023-03-15-preview",
  model = "gpt-4o",
  api_args = list(temperature = 0,
                  top_p = 1,
                  seed = 1234),
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
llmClientNonReasoning <- llmClient

#set up your LLM client - create the LLM client for one model below
llmClient <- ellmer::chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
llmClientReasoning <- llmClient

currentCcaeVersion <- 3909
dbConnectionString <- paste("jdbc:databricks://",
                            Sys.getenv("DATABRICKS_HOST"),
                            ":443/default;transportMode=http;ssl=1;AuthMech=3;httpPath=",
                            Sys.getenv("DATABRICKS_HTTP_PATH"),
                            sep='')

ccaeConnection <- list(cdmDatabaseSchema = paste0("merative_ccae.cdm_merative_ccae_v", currentCcaeVersion),
                       connectionDetails = DatabaseConnector::createConnectionDetails(dbms = "spark",
                                                                                      connectionString =  dbConnectionString,
                                                                                      user = keyring::key_get(service = "dBuserName"),
                                                                                      password = keyring::key_get(service = "dBpassword")))
database <- ccaeConnection

# source(paste0(drive, ":/", codeFolder, "/cdmSettings.R"), echo = FALSE)
source(paste0("./functionsPhenotype.R"), echo = FALSE)

options(databaseConnectorInteger64AsNumeric = TRUE)

gastroesophagealRefluxDisease <- list(testCondition = "Gastroesophageal reflux disease",
                                      dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                                      additionalInformation = "")


conditionList <- list(gastroesophagealRefluxDisease)

database <- CCAE_OHDA_DBSpec

outputDirectory <- file.path(paste0(drive, ":"), "llm/rubrics")

for(conditionUp in 1:length(conditionList)) {
  conditionRubricDir <- file.path(outputDirectory, conditionList[[conditionUp]]$testCondition)

  rubricCsLibraryFile <- findLatestFileVersion(saveDirectory = outputDirectory,
                                               fileName = "rubricConceptSetLibrary.rds")
  rubricCsLibrary <- file.path(outputDirectory, rubricCsLibraryFile)


  # if(is.null(existingMatchedConcepts) | createNew == TRUE) { #create matched concept sets
  cat(paste0("Developing concept sets for: ", conditionList[[conditionUp]]$testCondition, "\n"))

  # Extract required concept sets from rubrics
  latestRubricFile <- findLatestFileVersion(conditionRubricDir, "rubricList.rds")
  neededConceptSets <- readRDS(file.path(conditionRubricDir, latestRubricFile))$neededConcepts
  neededConceptSets$conceptSetsNeeded <- gsub(";", ",", neededConceptSets$conceptSetsNeeded)
  neededConceptSets$conceptSetsNeeded <- gsub(" ", "_", neededConceptSets$conceptSetsNeeded) #replace spaces in strings with _ (llm error)
  neededConceptSets$conceptSetsNeeded <- gsub(",_", ",", neededConceptSets$conceptSetsNeeded) #replace spaces in strings with _ (llm error)
  neededConceptSets$conceptSetsNeeded <- gsub("([[:alpha:]]) ([[:alpha:]])", "\\1, \\2", neededConceptSets$conceptSetsNeeded,
                                              perl = TRUE)
  csList <- NULL
  for(rowUp in 1:nrow(neededConceptSets)) {
    temp <- data.frame(trimws(unlist(strsplit(neededConceptSets$conceptSetsNeeded[[rowUp]], ","))))
    if(nrow(temp) > 0) {
      names(temp) <- c("bucket_id")
      temp$bucket_id <- tolower(temp$bucket_id )
      temp$type <- tolower(gsub("_", " ", neededConceptSets$type[[rowUp]]))
      csList <- rbind(csList, temp)
    }
  }

  bucketList <- unique(csList)

  #test needed concept sets for ones already in the library
  csLibrary <- readRDS(rubricCsLibrary)
  conceptSetNamesLibrary <- csLibrary[[2]]

  #get all needed concept sets
  allCs <- NULL
  for(csUp in seq_len(nrow(neededConceptSets))) {
    cs <- c(trimws(strsplit(neededConceptSets$conceptSetsNeeded[[csUp]], ",")[[1]]))
    allCs <- c(allCs, cs)
  }

  allCs <- unique(allCs)

  #compare needed with existing concept sets in library
  results <- NULL
  for(neededUp in seq_along(allCs)) {
    message("Needed CS: ", allCs[[neededUp]])
    eVec <- gsub("_", " ", allCs[[neededUp]], fixed = TRUE)
    # eVec <- str_remove_all(eVec,
    #                        regex("\\b(codes|type|types)\\b\\s*", ignore_case = TRUE))
    eVec <- str_squish(str_replace_all(eVec, "\\s+([[:punct:]])", "\\1"))

    eVec <- getEmbeddingVectors(tolower(eVec),
                                reportTime = F, silent = T)
    for(csUp in seq_len(nrow(conceptSetNamesLibrary))) {
      coSim <- Helios::computeSimilarity(eVec[[1]], conceptSetNamesLibrary$embed[[csUp]])
      if(coSim > 0.940) {
        results <- rbind(results, data.frame(csName = allCs[[neededUp]], libCsName = conceptSetNamesLibrary$conceptSetName[[csUp]], coSim = coSim))
      }
    }
  }
  #get list of all those included with high certainty
  priorCs <- results %>%
    group_by(csName) %>%
    slice_max(coSim, n = 1, with_ties = FALSE) %>%
    ungroup()

  priorCs$csName <- tolower(priorCs$csName)
  bucketList$bucket_id <- tolower(bucketList$bucket_id)

  #remove already present concept sets from bucket list
  existingConceptSets <- unique(data.frame(bucketList[bucketList$bucket_id %in% c(priorCs$csName), "bucket_id", drop = FALSE]))
  neededBucketList <- unique(data.frame(bucketList[!(bucketList$bucket_id %in% c(priorCs$csName)), "bucket_id", drop = FALSE]))

  message("\nMatched needed concept sets to ", nrow(existingConceptSets), " exisiting concept sets.",
          "\nNeed to develop ", nrow(neededBucketList), " additional concept sets.\n")

  #create a unique list of concept sets needed
  neededBucketList$searchString <- gsub("_", " ", neededBucketList$bucket_id)

  #use phenelope to develop concept sets
  for(csUp in seq_len(nrow(neededBucketList))) {

    # for(csUp in 40:nrow(neededBucketList)) {
    message(paste0("Creating concept set for #", csUp, " (of ",
                   nrow(neededBucketList), ") - ", neededBucketList$searchString[[csUp]]))

    queryType <- unique(neededConceptSets$type[grepl(neededBucketList$bucket_id[[csUp]],
                                                    neededConceptSets$conceptSetsNeeded)])

    anyEr <- any(grepl("evidence rules", queryType))
    if(anyEr) { #if the concept set is used for evidence rules, apply the condition as the clinical context
      clinicalContextToUse <- conditionList[[conditionUp]]$testCondition
    } else {
      clinicalContextToUse <- "any clinical context"
    }

    searchString <- neededBucketList$searchString[[csUp]]
    csDirectory <- file.path(conditionRubricDir, searchString)
    finalConceptSet <- Phenelope::createConceptSet(conceptSetTarget = searchString,
                                                   llmClientReasoning = llmClientReasoning,
                                                   llmClientNonReasoning = llmClientNonReasoning,
                                                   connectionDetails = database$connectionDetails,
                                                   cdmDatabaseSchema = database$cdmDatabaseSchema,
                                                   clinicalContext =  "any clinical context", #clinicalContextToUse,
                                                   clinicalDefinition = "",
                                                   outputDirectory = csDirectory,
                                                   bucketSize = 100)

    #save the concept set to the library along with embedded vector string
    csLibrary <- saveConceptSet(cleanName = neededBucketList$searchString[[csUp]],
                                conceptSetName = neededBucketList$bucket_id[[csUp]],
                                finalConceptSet = finalConceptSet[[2]],
                                csLibraryName = csLibrary)
  }

  #build concept set matched file
  results <- NULL
  for(neededUp in seq_along(allCs)) {
    message("Confirming library match for: ", allCs[[neededUp]])
    eVec <- gsub("_", " ", allCs[[neededUp]], fixed = TRUE)
    eVec <- str_squish(str_replace_all(eVec, "\\s+([[:punct:]])", "\\1"))

    eVec <- getEmbeddingVectors(tolower(eVec),
                                reportTime = F, silent = T)
    for(csUp in seq_len(nrow(conceptSetNamesLibrary))) {
      coSim <- Helios::computeSimilarity(eVec[[1]], conceptSetNamesLibrary$embed[[csUp]])
      if(coSim > 0.940) {
        results <- rbind(results, data.frame(csName = allCs[[neededUp]], libCsName = conceptSetNamesLibrary$conceptSetName[[csUp]], coSim = coSim))
      }
    }
  }

  #save matched file
  saveVersionedFile(saveDirectory = conditionRubricDir,
                    objectToSave = results,
                    fileName = "matchedConceptSets.rds")

  saveVersionedFile(saveDirectory = outputDirectory,
                    objectToSave = csLibrary,
                    fileName = "rubricConceptSetLibrary.rds")

}
