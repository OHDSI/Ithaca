# ITHACA
# Inference and Testing for Healthcare Algorithms using Clinical-equivalent Adjudication
# PHAEA
# Phenotype Adjudication and Evaluation Analytics

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

chronicInflammatoryDemyelinatingPolyneuropathy <- list(testCondition = "Chronic Inflammatory Demyelinating Polyneuropathy",
                                      dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                                      additionalInformation = "")


# conditionList <- list(afib, ami, iStroke, mm, bladderCancer, majorDepressiveDisorder, Neutropenia, urinaryTractInfection, Seizure,
#                       acuteLiverInjury, modSevAtopicDermatitis)

conditionList <- list(chronicInflammatoryDemyelinatingPolyneuropathy)

source("./rubricTypes.r")
# source("./rubricTypesFirstOnly.R")
priorRubric <- NULL

parentOutputDirectory <- file.path(paste0(drive, ":"), "llm/rubrics")

# latestRubric <- findLatestFileVersion(file.path(parentOutputDirectory, conditionList[[1]]$testCondition), "rubricList.rds")
# priorRubric <- file.path(parentOutputDirectory, conditionList[[1]]$testCondition, latestRubric)

for(conditionUp in 1:length(conditionList)) {
  originalPrompt <- readLines("./docs/scoringRubric.txt")

  if (!"additionalInformation" %in% names(conditionList[[conditionUp]])) {
    conditionList[[conditionUp]][["additionalInformation"]] <- ""
  }

  dir_path <- file.path(parentOutputDirectory, conditionList[[conditionUp]]$testCondition)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  if(is.null(priorRubric)) {
    rubricListOutput <- list()
    neededConceptSets <- NULL

    message(paste0("Developing rubrics for ", conditionList[[conditionUp]]$testCondition), "\n")

    for(rubricUp in 1:length(rubricListInput)) {
      cat(paste0("\r--Developing rubric ", rubricUp ," ..."))

      originalLines <- originalPrompt
      updated_lines <- gsub("<PHENOTYPE_NAME>", conditionList[[conditionUp]]$testCondition, originalLines)
      updated_lines <- gsub("<RUBRIC_SPECIFICS>", rubricListInput[[rubricUp]], updated_lines)
      updated_lines <- gsub("<DX_CRITERA>", conditionList[[conditionUp]]$dxCriteria, updated_lines)
      updated_lines <- gsub("<ADDITIONAL_INFORMATION>", conditionList[[conditionUp]]$additionalInformation, updated_lines)

      #exclude labs
      updated_lines <- gsub("<EXCLUDE_LABS>", "- Lab values or results", updated_lines)
      updated_lines <- gsub("<INCLUDE_LABS>", "- the presence of a laboratory measurement (without lab values)", updated_lines)

      #include labs
      # updated_lines <- gsub("<EXCLUDE_LABS>", "", updated_lines)
      # updated_lines <- gsub("<INCLUDE_LABS>", "- Lab values and results", updated_lines)

      prompt <- paste(updated_lines, collapse = "\n")

      #create ellmer structure based on return from prompt
      ellmerTypeObject <- ellmer::type_object(
        phenotype = ellmer::type_string(),
        rubric_variation_name = ellmer::type_string(),
        rubric_id = ellmer::type_string(),
        validation_goal = ellmer::type_string(),
        subject_id = ellmer::type_string(),
        assumptions =  ellmer::type_object(
          index_condition_codes_removed = ellmer::type_boolean(),
          labs_unavailable = ellmer::type_boolean()
        ),
        decision_thresholds = ellmer::type_object(
          yes = ellmer::type_integer(),
          no = ellmer::type_integer(),
          insufficient = ellmer::type_string()
        ),
        hard_exclusions = ellmer::type_array(type_object(
          rule_id = ellmer::type_string(),
          rule_type = ellmer::type_string(),
          criteria = ellmer::type_string(),
          start_day = ellmer::type_integer(),
          end_day = ellmer::type_integer(),
          presence_or_absence = ellmer::type_string(),
          occurrences_needed = ellmer::type_integer(),
          minimum_time_after_first_occurrence_days = ellmer::type_integer(),
          maximum_time_after_first_occurrence_days = ellmer::type_integer(),
          action = ellmer::type_string(),
          rationale = ellmer::type_string(),
          concept_sets_needed = ellmer::type_string()
        )),
        alternative_explanations =  ellmer::type_array(type_object(
          rule_id = ellmer::type_string(),
          rule_type = ellmer::type_string(),
          criteria = ellmer::type_string(),
          start_day = ellmer::type_integer(),
          end_day = ellmer::type_integer(),
          presence_or_absence = ellmer::type_string(),
          occurrences_needed = ellmer::type_integer(),
          minimum_time_after_first_occurrence_days = ellmer::type_integer(),
          maximum_time_after_first_occurrence_days = ellmer::type_integer(),
          points = ellmer::type_integer(),
          rationale_for_points = ellmer::type_string(),
          cap_max_decision = ellmer::type_string(),
          rationale = ellmer::type_string(),
          concept_sets_needed = ellmer::type_string()
        )),
        evidence_rules =  ellmer::type_array(type_object(
          rule_id = ellmer::type_string(),
          rule_type = ellmer::type_string(),
          category = ellmer::type_string(),
          tier = ellmer::type_string(),
          points = ellmer::type_integer(),
          rationale_for_points = ellmer::type_string(),
          criteria = ellmer::type_string(),
          start_day = ellmer::type_integer(),
          end_day = ellmer::type_integer(),
          presence_or_absence = ellmer::type_string(),
          occurrences_needed = ellmer::type_integer(),
          minimum_time_after_first_occurrence_days = ellmer::type_integer(),
          maximum_time_after_first_occurrence_days = ellmer::type_integer(),
          notes = ellmer::type_string(),
          concept_sets_needed = ellmer::type_string()
        )),
        explainability_requirements =  ellmer::type_object(
          must_quote_record_items = ellmer::type_boolean(),
          must_list_points_by_rule = ellmer::type_boolean(),
          must_list_exclusions_checked = ellmer::type_boolean()
        )
      )

      rubricListOutput[rubricUp]$hard_exclusions <- NULL
      while(length(rubricListOutput[[rubricUp]]$hard_exclusions) == 0) {
        rubricListOutput[[rubricUp]] <- queryLLM(llmClient = getLlmo3Client(), prompt = prompt, output = "json",
                                                 ellmerTypeObject = ellmerTypeObject)
      }

      #todo - check on the needed concept sets
      #get the concept sets needed
      #hard exclusions
      for(hardExUp in 1:nrow(rubricListOutput[[rubricUp]]$hard_exclusions)) {
        temp <- NULL
        temp$type <- "hard_exclusions"
        temp$rubric <- rubricUp
        temp$rule <- hardExUp
        temp$criteria <- rubricListOutput[[rubricUp]]$hard_exclusions$criteria[[hardExUp]]
        temp$conceptSetsNeeded <- unlist(rubricListOutput[[rubricUp]]$hard_exclusions$concept_sets_needed[[hardExUp]])
        neededConceptSets <- rbind(neededConceptSets, data.frame(temp))
      }
      # alt. explanations
      for(altExpUp in 1:nrow(rubricListOutput[[rubricUp]]$alternative_explanations)) {
        temp <- NULL
        temp$type <- "alternative explanations"
        temp$rubric <- rubricUp
        temp$rule <- altExpUp
        temp$criteria <- rubricListOutput[[rubricUp]]$alternative_explanations$criteria[[altExpUp]]
        temp$conceptSetsNeeded <- unlist(rubricListOutput[[rubricUp]]$alternative_explanations$concept_sets_needed[[altExpUp]])
        neededConceptSets <- rbind(neededConceptSets, data.frame(temp))
      }
      #evidence rules
      for(evRulesUp in 1:nrow(rubricListOutput[[rubricUp]]$evidence_rules)) {
        temp <- NULL
        temp$type <- "evidence rules"
        temp$rubric <- rubricUp
        temp$rule <- evRulesUp
        temp$criteria <- rubricListOutput[[rubricUp]]$evidence_rules$criteria[[evRulesUp]]
        temp$conceptSetsNeeded <- unlist(rubricListOutput[[rubricUp]]$evidence_rules$concept_sets_needed[[evRulesUp]])
        neededConceptSets <- rbind(neededConceptSets, data.frame(temp))
      }

      cat(paste0("done.\n"))
    }

    #save the prompt used
    if(exists("prompt")) {
      cat(prompt, file = file.path(dir_path,"temp.txt"), sep = "\n")
      savedFile <- saveVersionedFile(saveDirectory = file.path(dir_path),
                                     objectToSave = file.path(dir_path,"temp.txt"),
                                     fileName = paste0(conditionList[[conditionUp]]$testCondition, "_rubricPrompt.txt"))
      unlink(savedFile)
      newFile <- file.rename(file.path(dir_path,"temp.txt"), savedFile)

    }
  } else { #read in a designated rubric
    cat(paste0("Loading existing rubric from ", priorRubric, "\n"))
    rubricListOutput <- readRDS(priorRubric)$rubricList
    neededConceptSets <- readRDS(priorRubric)[[2]]
  }

  #consolidate the needed concept sets
  message("--Consolidating needed concept sets...")
  allNeededConceptSets <- NULL
  for(csUp in 1:nrow(neededConceptSets)) {
    temp <- NULL
    temp$neededConceptSets <- data.frame(
      value = trimws(strsplit(neededConceptSets$conceptSetsNeeded[[csUp]], ",", fixed = TRUE)[[1]]),
      stringsAsFactors = FALSE)

    allNeededConceptSets <- rbind(allNeededConceptSets, data.frame(temp))
  }

  allNeededConceptSets <- unique(allNeededConceptSets)

  originalPrompt <- paste(readLines("./docs/consolidateConceptSetsNames.txt"), collapse = "\n")
  originalLines <- originalPrompt
  updated_lines <- gsub("<ALL_CONCEPT_SET_NAMES>", toJSON(allNeededConceptSets), originalPrompt)

  prompt <- paste(updated_lines, collapse = "\n")

  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    primaryconceptSet= ellmer::type_string(),
    associatedConceptSet = ellmer::type_string()
  )
  )

  results <- queryLLM(llmClient = getLlmo3Client(), prompt = prompt, output = "json",
                      ellmerTypeObject = ellmerTypeObject)

  #replace codes in rubric statements
  tempRubric <- rubricListOutput
  for(rubricUp in 1:length(rubricListInput)) {
    for(hardExUp in 1:nrow(rubricListOutput[[rubricUp]]$hard_exclusions)) {
      for(replaceUp in 1:nrow(results)) {
        oldString <- paste0("(^|\\s*,\\s*)\\Q", results$associatedConceptSet[replaceUp], "\\E(?=\\s*(?:,|$))")
        newString <- paste0("\\1", results$primaryconceptSet[replaceUp])
        tempRubric[[rubricUp]]$hard_exclusions$concept_sets_needed[[hardExUp]] <-
          gsub(oldString, newString, tempRubric[[rubricUp]]$hard_exclusions$concept_sets_needed[[hardExUp]],
               perl = TRUE, ignore.case = TRUE)
      }
    }

    # alt. explanations
    for(altExpUp in 1:nrow(rubricListOutput[[rubricUp]]$alternative_explanations)) {
      for(replaceUp in 1:nrow(results)) {
        oldString <- paste0("(^|\\s*,\\s*)\\Q", results$associatedConceptSet[replaceUp], "\\E(?=\\s*(?:,|$))")
        newString <- paste0("\\1", results$primaryconceptSet[replaceUp])
        tempRubric[[rubricUp]]$alternative_explanations$concept_sets_needed[[altExpUp]] <-
          gsub(oldString, newString, tempRubric[[rubricUp]]$alternative_explanations$concept_sets_needed[[altExpUp]],
               perl = TRUE, ignore.case = TRUE)
      }
    }

    #evidence rules
    for(evRulesUp in 1:nrow(rubricListOutput[[rubricUp]]$evidence_rules)) {
      for(replaceUp in 1:nrow(results)) {
        oldString <- paste0("(^|\\s*,\\s*)\\Q", results$associatedConceptSet[replaceUp], "\\E(?=\\s*(?:,|$))")
        newString <- paste0("\\1", results$primaryconceptSet[replaceUp])
        tempRubric[[rubricUp]]$evidence_rules$concept_sets_needed[[evRulesUp]] <-
          gsub(oldString, newString, tempRubric[[rubricUp]]$evidence_rules$concept_sets_needed[[evRulesUp]],
               perl = TRUE, ignore.case = TRUE)
      }
    }
  }
  rubricListOutput <- tempRubric

  #replace in needed concept sets
  tempNeededConceptSets <- neededConceptSets
  for(nCsUp in 1:nrow(neededConceptSets)) {
    for(replaceUp in 1:nrow(results)) {
      oldString <- paste0("(^|\\s*,\\s*)\\Q", results$associatedConceptSet[replaceUp], "\\E(?=\\s*(?:,|$))")
      newString <- paste0("\\1", results$primaryconceptSet[replaceUp])
      tempNeededConceptSets$conceptSetsNeeded[[nCsUp]] <-
        gsub(oldString, newString, tempNeededConceptSets$conceptSetsNeeded[[nCsUp]],
             perl = TRUE, ignore.case = TRUE)
    }
  }
  neededConceptSets <- tempNeededConceptSets

  #save output
  saveRDS(list(rubricList = rubricListOutput, neededConcepts = neededConceptSets), file.path(dir_path, "rubricList.rds"))
  saveVersionedFile(dir_path, list(rubricList = rubricListOutput, neededConcepts = neededConceptSets),
                    "rubricList.rds", replace = F)

  saveVersionedFile(dir_path, writeListDoc(rubricListOutput, header = ""), "rubricList.txt", replace = F)

  #create sql statement from criteria and needed concepts
  message(paste0("\nProcessing SQL statements for rules"))
  shortRubric <- NULL
  counter <- 1
  for(rubricUp in 1:length(rubricListOutput)) {
    message(paste0("\n\n-Rubric ", rubricUp, " (out of ", length(rubricListOutput), ")\n"))
    message(paste0("--Hard exclusion rules\n"))
    for(ruleUp in 1:nrow(rubricListOutput[[rubricUp]]$hard_exclusions)) {
      cat(paste0("\r---Rubric ", rubricUp ,", Hard exclusion ", ruleUp, " (out of ",
                 nrow(rubricListOutput[[rubricUp]]$hard_exclusions), ")"))

      temp <- NULL
      temp$condition <- rubricListOutput[[rubricUp]]$phenotype[[1]]
      temp$rubric <- rubricListOutput[[rubricUp]]$rubric_id[[1]]
      temp$rubricName <- rubricListOutput[[rubricUp]]$rubric_variation_name[[1]]
      temp$rubricYesScore <- rubricListOutput[[rubricUp]]$decision_thresholds$yes[[1]]
      temp$rubricNoScore <- rubricListOutput[[rubricUp]]$decision_thresholds$no[[1]]
      temp$ruleId <- rubricListOutput[[rubricUp]]$hard_exclusions$rule_id[[ruleUp]]
      temp$type <- "hard_exclusion"
      temp$score <- -100
      temp$criteria <- rubricListOutput[[rubricUp]]$hard_exclusions$criteria[[ruleUp]]
      temp$rationale <- rubricListOutput[[rubricUp]]$hard_exclusions$rationale[[ruleUp]]
      temp$neededConceptSets <- rubricListOutput[[rubricUp]]$hard_exclusions$concept_sets_needed[[ruleUp]]

      #create initial sql statement
      sql <- createSqlStatement(criteria = temp$criteria, conceptSets = temp$neededConceptSets, llmClient = getLlmo3Client())

      #correct the sql statement if necessary
      # correctedSql <- correctSqlStatement(sql)

      # temp$sqlStatement <- sql$correctedQuery
      temp$sqlStatement <- sql
      temp$okToUse <- TRUE

      shortRubric <- rbind(shortRubric, data.frame(temp))
    }

    message(paste0("\n--Alternative explanation rules\n"))
    for(ruleUp in 1:nrow(rubricListOutput[[rubricUp]]$alternative_explanations)) {
      cat(paste0("\r---Rubric ", rubricUp ,", Alternative explanation ", ruleUp, " (out of ",
                 nrow(rubricListOutput[[rubricUp]]$alternative_explanations), ")"))

      temp <- NULL
      temp$condition <- rubricListOutput[[rubricUp]]$phenotype[[1]]
      temp$rubric <- rubricListOutput[[rubricUp]]$rubric_id[[1]]
      temp$rubricName <- rubricListOutput[[rubricUp]]$rubric_variation_name[[1]]
      temp$rubricYesScore <- rubricListOutput[[rubricUp]]$decision_thresholds$yes[[1]]
      temp$rubricNoScore <- rubricListOutput[[rubricUp]]$decision_thresholds$no[[1]]
      temp$ruleId <- rubricListOutput[[rubricUp]]$alternative_explanations$rule_id[[ruleUp]]
      temp$type <- "alternative_explanations"
      temp$score <- rubricListOutput[[rubricUp]]$alternative_explanations$points[[ruleUp]]
      temp$criteria <- rubricListOutput[[rubricUp]]$alternative_explanations$criteria[[ruleUp]]
      temp$rationale <- rubricListOutput[[rubricUp]]$alternative_explanations$rationale[[ruleUp]]
      temp$neededConceptSets <- rubricListOutput[[rubricUp]]$alternative_explanations$concept_sets_needed[[ruleUp]]

      #create initial sql statement
      sql <- createSqlStatement(criteria = temp$criteria, conceptSets = temp$neededConceptSets, llmClient = getLlmo3Client())

      #correct the sql statement if necessary
      # correctedSql <- correctSqlStatement(sql)

      # temp$sqlStatement <- sql$correctedQuery
      temp$sqlStatement <- sql
      temp$okToUse <- TRUE

      shortRubric <- rbind(shortRubric, data.frame(temp))
    }

    message(paste0("\n--Evidence rules\n"))
    for(ruleUp in 1:nrow(rubricListOutput[[rubricUp]]$evidence_rules)) {
      cat(paste0("\r---Rubric ", rubricUp ,", Evidence rule ", ruleUp, " (out of ",
                 nrow(rubricListOutput[[rubricUp]]$evidence_rules), ")"))

      temp <- NULL
      temp$condition <- rubricListOutput[[rubricUp]]$phenotype[[1]]
      temp$rubric <- rubricListOutput[[rubricUp]]$rubric_id[[1]]
      temp$rubricName <- rubricListOutput[[rubricUp]]$rubric_variation_name[[1]]
      temp$rubricYesScore <- rubricListOutput[[rubricUp]]$decision_thresholds$yes[[1]]
      temp$rubricNoScore <- rubricListOutput[[rubricUp]]$decision_thresholds$no[[1]]
      temp$ruleId <- rubricListOutput[[rubricUp]]$evidence_rules$rule_id[[ruleUp]]
      temp$type <- "evidence_rules"
      temp$score <- rubricListOutput[[rubricUp]]$evidence_rules$points[[ruleUp]]
      temp$criteria <- rubricListOutput[[rubricUp]]$evidence_rules$criteria[[ruleUp]]
      temp$rationale <- rubricListOutput[[rubricUp]]$evidence_rules$notes[[ruleUp]]
      temp$neededConceptSets <- rubricListOutput[[rubricUp]]$evidence_rules$concept_sets_needed[[ruleUp]]

      #create initial sql statement
      sql <- createSqlStatement(criteria = temp$criteria, conceptSets = temp$neededConceptSets, llmClient = getLlmo3Client())

      temp$sqlStatement <- sql
      temp$okToUse <- TRUE

      shortRubric <- rbind(shortRubric, data.frame(temp))
    }
  }

  cat(paste0("...done\n"))
  savedFile <- saveVersionedFile(saveDirectory = file.path(dir_path),
                                 objectToSave = list(shortRubric = shortRubric),
                                 fileName = paste0(conditionList[[conditionUp]]$testCondition, "_shortRubric.rds"))

  conciseRubric <- shortRubric[,c(1:12)]
  savedFile <- saveVersionedFile(saveDirectory = file.path(dir_path),
                                 objectToSave = conciseRubric,
                                 fileName = paste0(conditionList[[conditionUp]]$testCondition, "_conciseRubric.csv"))


  #create a write-up of the rubric for clinical readers
  originalPrompt <- paste(readLines("./docs/summarizeConciseRubric.txt"), collapse = "\n")
  exampleText <- paste(readLines("./docs/exampleSummaryReport.txt"), collapse = "\n")

  originalLines <- originalPrompt
  conciseRubric <- shortRubric[,c(1:11)]
  updated_lines <- gsub("<CONCISE_RUBRIC>", toJSON(conciseRubric), originalPrompt)

  exampleRule <- "Here is example text for your text.  Follow the format of the example text PRECISELY as shown especially headers/subheaders. \n\n Sample Text: \n"
  updated_lines <- gsub("<EXAMPLE_TEXT_RULE>", "", updated_lines)
  updated_lines <- gsub("<EXAMPLE_TEXT>", exampleText, updated_lines)

  prompt <- paste(updated_lines, collapse = "\n")

  results <- queryLLM(llmClient = getLlmo3Client(), prompt = prompt, output = "text")

  savedFile <- saveVersionedFile(saveDirectory = file.path(dir_path),
                                 objectToSave = results,
                                 fileName = paste0(conditionList[[conditionUp]]$testCondition, "_RuleSummary.txt"))


}

