# A fake ellmer chat client: ithQueryLlm() calls $chat() for text and $chat_structured() for typed output.
# `structured` receives (kind, prompt, callNumber) where kind is review, consolidation, conceptSets, or other.
makeFakeLlm <- function(structured = function(kind, prompt, call) stop("unexpected structured call: ", kind),
                        text = function(prompt) "select 42 as fake_sql") {
  state <- new.env()
  state$prompts <- list()
  state$structuredCalls <- 0L
  state$kinds <- character()
  list(
    state = state,
    set_turns = function(...) invisible(NULL),
    chat = function(prompt, echo = "none") {
      state$prompts[[length(state$prompts) + 1L]] <- prompt
      text(prompt)
    },
    chat_structured = function(prompt, echo = "none", type) {
      state$structuredCalls <- state$structuredCalls + 1L
      state$prompts[[length(state$prompts) + 1L]] <- prompt
      properties <- tryCatch(names(type@properties), error = function(e) NULL)
      if (is.null(properties)) {
        properties <- tryCatch(names(type@items@properties), error = function(e) character())
      }
      kind <- if ("droppedFindings" %in% properties) {
        "consolidation"
      } else if ("findings" %in% properties) {
        "review"
      } else if ("conceptSetsNeeded" %in% properties) {
        "conceptSets"
      } else if ("evidenceRules" %in% properties) {
        "rubric"
      } else if ("requestedConceptSetName" %in% properties) {
        "hintDecisions"
      } else if ("selectedConceptSetName" %in% properties) {
        "adjudication"
      } else if ("relatedTerms" %in% properties) {
        "ae1Terms"
      } else if ("relatedConceptIds" %in% properties) {
        "ae1Selection"
      } else {
        "other"
      }
      state$kinds <- c(state$kinds, kind)
      structured(kind, prompt, state$structuredCalls)
    }
  )
}

# Structured answers for the apply path: a valid concept-set name for any rule whose criteria changed.
fakeConceptSets <- function(kind, prompt, call) {
  if (kind == "conceptSets") {
    return(list(conceptSetsNeeded = "NewSet_DIAGNOSIS_CODES"))
  }
  stop("unexpected structured call: ", kind)
}

# One review finding as the LLM returns it; every schema field defaults to a blank string.
makeFakeFinding <- function(ruleId = "ER3", ...) {
  fields <- names(Ithaca:::ithRubricReviewFindingFields())
  row <- as.data.frame(
    stats::setNames(as.list(rep("", length(fields))), fields),
    stringsAsFactors = FALSE
  )
  row$ruleId <- ruleId
  row$category <- "weighting"
  row$severity <- "high"
  row$finding <- "a finding"
  row$recommendation <- "Lower the score"
  row$evidence <- "Some guideline"
  row$evidenceConfidence <- "high"
  overrides <- list(...)
  for (name in names(overrides)) {
    row[[name]] <- overrides[[name]]
  }
  row
}

makeFakeReviewResponse <- function(..., overall = "overall") {
  list(overallAssessment = overall, findings = do.call(rbind, list(...)))
}

# Writes a condition folder with concise and short rubric version 1 and returns the output directory.
makeRubricFixture <- function(conditionName = "COPD",
                              ruleIds = c("ER1", "ER2", "ER3", "AE1", "HE1"),
                              env = parent.frame()) {
  outputDirectory <- withr::local_tempdir(.local_envir = env)
  all <- data.frame(
    ruleId = c("ER1", "ER2", "ER3", "AE1", "HE1"),
    type = c("evidenceRules", "evidenceRules", "evidenceRules", "alternativeExplanations", "hard_exclusion"),
    score = c(3, 2, 4, -4, -100),
    stringsAsFactors = FALSE
  )
  rules <- all[match(ruleIds, all$ruleId), , drop = FALSE]
  shortRubric <- data.frame(
    condition = conditionName, rubric = "R1", rubricName = "x", rubricYesScore = 10, rubricNoScore = 5,
    ruleId = rules$ruleId, type = rules$type, score = rules$score,
    criteria = paste("crit", rules$ruleId), rationale = "r",
    neededConceptSets = paste0("Rule", rules$ruleId, "_DIAGNOSIS_CODES"),
    sqlStatement = paste("select 1 --", rules$ruleId),
    okToUse = TRUE,
    stringsAsFactors = FALSE
  )
  Ithaca:::ithSaveShortRubric(shortRubric, conditionName, outputDirectory, yesCombinationReport = FALSE)
  outputDirectory
}

writeReviewFile <- function(outputDirectory, review, conditionName = "COPD", version = 1L) {
  path <- file.path(outputDirectory, conditionName, paste0(conditionName, "_conciseRubricReview_V", version, ".csv"))
  utils::write.csv(review, path, row.names = FALSE, na = "")
  path
}

conditionFile <- function(outputDirectory, name, conditionName = "COPD") {
  file.path(outputDirectory, conditionName, name)
}

readConcise <- function(outputDirectory, version, conditionName = "COPD") {
  utils::read.csv(
    conditionFile(outputDirectory, paste0(conditionName, "_conciseRubric_V", version, ".csv"), conditionName),
    stringsAsFactors = FALSE
  )
}
