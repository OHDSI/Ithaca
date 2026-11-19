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
source(paste0("./functionsPhenotype.R"), echo = FALSE)

database <- list(Panther_OHDA_DBSpec)
database <- list(DOD_OHDA_DBSpec)
database <- list(HV_OHDA_DBSpec)
database <- list(CCAE_OHDA_DBSpec)
database <- list(CCAE_OHDA_DBSpec, MDCR_OHDA_DBSpec, Panther_OHDA_DBSpec)
database <- list(Panther_OHDA_DBSpec)

gastroesophagealRefluxDisease <- list(testCondition = "Gastroesophageal reflux disease",
                                      dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                                      additionalInformation = "",
                                      rubricName = "Gastroesophageal reflux disease",
                                      cohortId = 14412)

conditionList <- list(acuteLiverFailure)

# useExisting <- TRUE
useExisting <- FALSE

generateCohort <- TRUE
# generateCohort <- FALSE

outputDirectory <- file.path(paste0(drive, ":"), "llm/rubrics")

for(conditionUp in 1:length(conditionList)) {
  cat(paste0("\nCreating subject scores for ", conditionList[[conditionUp]]$rubricName, "\n"))

  if(generateCohort) {
    generateCohorts(cohortList = c(unlist(conditionList[[conditionUp]]$cohortId)), dbList = database, drive)
  }

  scoringDir <- file.path(paste0(drive,":"),"llm/rubrics",conditionList[[conditionUp]]$rubricName)

  latestRubricFile <- findLatestFileVersion(scoringDir, paste0(conditionList[[conditionUp]]$rubricName , "_shortRubric.rds"))
  shortRubric <- readRDS(file.path(scoringDir, latestRubricFile))$shortRubric
  if(!is.data.frame(shortRubric)) { #convert from list
    shortRubric <- shortRubric[[1]]
  }
  shortRubric <- shortRubric[with(shortRubric, order(rubric, ruleId)), ]

  ############
  # shortRubric <- shortRubric[!(shortRubric$rubric == "R2" & shortRubric$ruleId == "HE2"),]
  # shortRubric <- shortRubric[!(shortRubric$rubric == "R3" & shortRubric$ruleId == "HE3"),]
  # shortRubric <- shortRubric[!(shortRubric$rubric == "R5" & shortRubric$ruleId == "HE1"),]
  ############

  latestRubricFileFull <- findLatestFileVersion(scoringDir, paste0("rubricList.rds"))
  fullRubric <- readRDS(file.path(scoringDir, latestRubricFileFull))$rubricList

  conditionRubricDir <- file.path(outputDirectory, conditionList[[conditionUp]]$condition)

  rubricCsLibraryFile <- findLatestFileVersion(saveDirectory = outputDirectory,
                                               fileName = "rubricConceptSetLibrary.rds")
  rubricCsLibrary <- file.path(outputDirectory, rubricCsLibraryFile)

  csLibrary <- readRDS(rubricCsLibrary)
  conceptSetJsonLibrary <- csLibrary[[1]]

  latestMatchFile <- findLatestFileVersion(scoringDir, "matchedConceptSets.rds")
  matchedConceptSets <- readRDS(file.path(scoringDir, latestMatchFile))

  changesMade <- FALSE
  for(dbUp in 1:length(database)) {
    cat(paste0("\n--Database: ", database[[dbUp]]$databaseId))
    cohortDir <- file.path(paste0(drive,":"),"llm/phenotypes",conditionList[[conditionUp]]$rubricName , database[[dbUp]]$databaseId)
    tempTable <- gsub("[.]", "_", paste0("pv_table_", database[[dbUp]]$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

    if(useExisting) {
      latestsubjectCount <- findLatestFileVersion(cohortDir, paste0("subjectCount_Cohort",
                                                                    unlist(conditionList[[conditionUp]]$cohortId), ".csv"))
      rubricSubjectCount <- read.csv(file.path(cohortDir, latestsubjectCount))

      latestfullScoring <- findLatestFileVersion(cohortDir, paste0("fullScoringFile_Cohort",
                                                                   unlist(conditionList[[conditionUp]]$cohortId), ".rds"))
      scoredFile <- readRDS(file.path(cohortDir, latestfullScoring))
      cat(paste0("\n--Using existing scoring file: ", file.path(cohortDir, latestfullScoring)))


    } else {
      scoredFile <- NULL
      for(cohortUp in 1:length(conditionList[[conditionUp]]$cohortId)) {
        cat(paste0("\n--Cohort ID: ", conditionList[[conditionUp]]$cohortId))
        connection <- suppressMessages(DatabaseConnector::connect(connectionDetails = database[[dbUp]]$connectionDetails))
        rubricSubjectCount <- NULL
        for(rubricUp in 1:nrow(shortRubric)) {
          cat(paste0("\r--Rubric: ", shortRubric$rubric[[rubricUp]], " Rule: ", shortRubric$ruleId[[rubricUp]], "..."))

          plugIns <- unique(stringr::str_extract_all(shortRubric$sqlStatement[[rubricUp]], "\\B@[A-Za-z0-9_]+")[[1]])
          plugIns <- plugIns[!(plugIns %in% c("@databaseSchema", "@cohortSchema", "@cohortTable","@cohortId"))]

          resolvedPlugIn <- NULL
          if (1 <= length(plugIns)) {
            for(resolvedUp in 1:length(plugIns)) {
              csToUse <- matchedConceptSets$libCsName[tolower(matchedConceptSets$csName) ==
                                                        tolower(as.character(substr(plugIns[[resolvedUp]],
                                                                                    2,nchar(plugIns[[resolvedUp]]))))]

              csJson <- conceptSetJsonLibrary$conceptSet[conceptSetJsonLibrary$conceptSetName == csToUse]

              if(!is.null(csJson$items)) {
                #first set all the ingredient level include Descendants to true
                if(!is.null(csJson$items$concept$CONCEPT_CLASS_ID)) {
                  csJson$items$includeDescendants[csJson$items$concept$DOMAIN_ID == "Drug"] <- TRUE
                }

                concepts <- Phenelope::resolveConceptSet(conceptSet = csJson,
                                                         connectionDetails = database[[dbUp]]$connectionDetails,
                                                         cdmDatabaseSchema = database[[dbUp]]$cdmDatabaseSchema)
                resolvedPlugIn[[resolvedUp]] <- paste(unique(concepts$conceptId), collapse = ",")
              } else {
                resolvedPlugIn[[resolvedUp]] <- "36433327" #dummy device conceptId if no concepts found
              }
            }
          }

          finalPlugIns <- NULL
          if(length(plugIns) >= 1) {
            for(plugInUp in 1:length(plugIns)) {
              temp <- NULL
              temp$replaceText <- plugIns[[plugInUp]]
              temp$values <- resolvedPlugIn[[plugInUp]]
              finalPlugIns <- rbind(finalPlugIns, data.frame(temp))
            }
          }

          retry_limit <- 10  # Maximum number of retries
          attempt <- 0      # Initial attempt counter
          success <- FALSE  # Flag to indicate success
          currentSql <- shortRubric$sqlStatement[[rubricUp]]
          while (attempt <= retry_limit && !success) { #llm may produce incorrect sql - chance to revise query
            tryCatch({
              attempt <- attempt + 1  # Increment the attempt count

              if(attempt > 1) { #error found, pass error for correction
                correctedSql <- correctSqlStatement(currentSql, error = errorMessage)
                currentSql <- correctedSql$correctedQuery
                cat(paste0("--Correction made: ", correctedSql$changesMade, "\n"))
                shortRubric$sqlStatement[[rubricUp]] <- correctedSql$correctedQuery
                changesMade <- TRUE
              } else {
                # correctedSql <- correctSqlStatement(currentSql)
                # currentSql <- correctedSql$correctedQuery
              }

              sql <- SqlRender::render(currentSql,
                                       databaseSchema = database[[dbUp]]$cdmDatabaseSchema,
                                       cohortSchema =   database[[dbUp]]$workDatabaseSchema,
                                       cohortTable = tempTable,
                                       cohortId = conditionList[[conditionUp]]$cohortId)

              #add in the final parameters not added by render
              if(!is.null(finalPlugIns)) {
                for(plugInUp in 1:nrow(finalPlugIns)) {
                  if(finalPlugIns$values[[plugInUp]] != "") {
                    sql <- gsub(finalPlugIns$replaceText[[plugInUp]], finalPlugIns$values[[plugInUp]], sql)
                  } else {
                    sql <- gsub(finalPlugIns$replaceText[[plugInUp]], "36433327", sql) #dummy 0 subject device code
                  }
                }
              }
              sql <- SqlRender::translate(sql, "spark")
              # cat(sql, file = "clipboard")

              results <- querySql(sql, connection = connection, snakeCaseToCamelCase = TRUE, integer64AsNumeric = F)
              success <- T

              if(nrow(results) > 0) {
                temp <- NULL
                temp$cohortDefinitionId <- results$cohortDefinitionId
                temp$subjectId <- results$subjectId
                temp$cohortStartDate <- results$cohortStartDate
                temp$rubricId <- shortRubric$rubric[[rubricUp]]
                temp$ruleId <- shortRubric$ruleId[[rubricUp]]
                temp$score <- shortRubric$score[[rubricUp]]

                scoredFile <- rbind(scoredFile, data.frame(temp))
              }
            },
            error = function(e) {
              # Handle the error: print a message and increment the attempt counter
              message(paste("Attempt", attempt, "failed with sql error "))
              if(grepl("abort", e$message, ignore.case=TRUE)) {
                cat("Stopping the run as requested.\n")
                stop("Execution stopped by user.")
              }
              if(attempt >= retry_limit) {
                message(paste("Reached attempt limit."))
                if(e$message != "Content filter triggered") { #only abort without asking for Content filter triggered
                  user_input <- readline(prompt = "Do you want to continue with the next concept? y/n ")
                  if(user_input == tolower('n')) {
                    cat("Stopping the run as requested.\n")
                    stop("Execution stopped by user.")
                  }
                }
              }
              errorMessage <<- e$message
              fullError <<- e
              return(NULL)  # Return NULL in case of error
            })
          }

          cat(paste0(nrow(results), " subjects found.\n"))
          temp <- NULL
          temp$cohortDefinitionId <- conditionList[[conditionUp]]$cohortId
          temp$rubricId <- shortRubric$rubric[[rubricUp]]
          temp$ruleId <- shortRubric$ruleId[[rubricUp]]
          temp$score <- shortRubric$score[[rubricUp]]
          temp$subjectCount <- nrow(results)
          temp$criteria <- shortRubric$criteria[[rubricUp]]

          rubricSubjectCount <- rbind(rubricSubjectCount, data.frame(temp))
        }
        disconnect(connection)

        # re-save rubric file if sql changes were made
        if(changesMade) {
          savedFile <- saveVersionedFile(saveDirectory = scoringDir,
                                         objectToSave = list(shortRubric = shortRubric),
                                         fileName = paste0(conditionList[[conditionUp]]$rubricName , "_shortRubric.rds"))
        }

        saveVersionedFile(saveDirectory = cohortDir, objectToSave = rubricSubjectCount,
                          fileName = paste0("subjectCount_Cohort", unlist(conditionList[[conditionUp]]$cohortId), ".csv"),
                          replace = F)

        saveVersionedFile(saveDirectory = cohortDir, objectToSave = scoredFile,
                          fileName = paste0("fullScoringFile_Cohort", unlist(conditionList[[conditionUp]]$cohortId), ".rds"),
                          replace = F)
      }
    }

    #sum up the scores for each subject
    allSubjects <- data.table::data.table(getCohortSubjects(cohort = unlist(conditionList[[conditionUp]]$cohortId),
                                                            database = database[[dbUp]]))
    scoredFile$subjectId <- as.double(scoredFile$subjectId)
    allSubjects$subjectId <- as.double(allSubjects$subjectId)
    scoredFile <- data.table::data.table(scoredFile)

    cohortScores <- scoredFile[
      allSubjects,
      on = .(cohortDefinitionId, subjectId, cohortStartDate)
    ]

    sumCohortScoresPre <- cohortScores[
      , .(total = sum(score, na.rm = TRUE)),
      by = .(cohortDefinitionId, subjectId, cohortStartDate, rubricId)
    ]

    #merge with rubric scores
    allScoring <- NULL
    for(rubricUp in 1:length(fullRubric)) { #NOTE added to end
      temp <- NULL
      temp$rubricId <- fullRubric[[rubricUp]]$rubric_id[[1]]
      temp$yesScore <- fullRubric[[rubricUp]]$decision_thresholds$yes[[1]]
      temp$noScore <- fullRubric[[rubricUp]]$decision_thresholds$no[[1]]

      allScoring <- rbind(allScoring, data.frame(temp))
    }
    allScoring <- data.table::data.table(allScoring)

    allSubjects[, tmp := 1]
    allScoring[, tmp := 1]

    # allSubjectsRubrics <- merge(allSubjects, allScoring, by = NULL) #get cartesian product
    allSubjectsRubrics  <- allSubjects[allScoring, on = "tmp", allow.cartesian = TRUE][, tmp := NULL]

    sumCohortScores <- merge(allSubjectsRubrics, sumCohortScoresPre, all.x = T)
    sumCohortScores$total[is.na(sumCohortScores$total)] <- 0

    sumCohortScores$case <- -1
    sumCohortScores$case[sumCohortScores$total >= sumCohortScores$yesScore] <- 1
    sumCohortScores$case[sumCohortScores$total > sumCohortScores$noScore &
                           sumCohortScores$total < sumCohortScores$yesScore] <- 0

    sumCohortScores <- unique(sumCohortScores)

    saveVersionedFile(saveDirectory = cohortDir, objectToSave = sumCohortScores,
                      fileName = paste0("sumCohortScores_Cohort", unlist(conditionList[[conditionUp]]$cohortId), ".rds"),
                      replace = F)

    finalScores <- sumCohortScores[
      , .(total = sum(case, na.rm = TRUE)),
      by = .(cohortDefinitionId, subjectId, cohortStartDate)
    ]

    finalScores$case <- 0
    finalScores$case[finalScores$total > 0] <- 1
    finalScores$case[finalScores$total < 0] <- -1

    saveVersionedFile(saveDirectory = cohortDir, objectToSave = finalScores,
                      fileName = paste0("finalScores_Cohort", unlist(conditionList[[conditionUp]]$cohortId), ".rds"),
                      replace = F)
  }
}

results <- finalScores %>%
  group_by(cohortDefinitionId) %>%
  summarize(
    case = sum(total > 0, na.rm = TRUE),
    insufficientEvidence = sum(total == "0", na.rm = TRUE),
    nonCase = sum(total < 0, na.rm = TRUE),
    total = n(),
    ppvAll = case/total,
    ppvYN = case/(case+nonCase),
    .groups = "drop"
  )
results



