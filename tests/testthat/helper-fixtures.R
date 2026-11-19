makeConciseRubric <- function() {
  data.frame(
    rubric = "R1", rubricName = "x", rubricYesScore = 10, rubricNoScore = 5,
    ruleId = c("ER3", "AE1", "HE1"),
    type = c("evidenceRules", "alternativeExplanations", "hard_exclusion"),
    score = c(4, -4, -100),
    criteria = c("crit ER3", "crit AE1", "crit HE1"),
    rationale = "", neededConceptSets = "",
    stringsAsFactors = FALSE
  )
}

# One review row; any column can be overridden.
makeReviewRow <- function(ruleId = "ER3", ...) {
  concise <- makeConciseRubric()
  row <- data.frame(
    condition = "x", Accept = "Y", newCriteria = "", timesFlagged = "3/3", rubric = "R1",
    ruleId = ruleId,
    originalCriteria = concise$criteria[match(ruleId, concise$ruleId)],
    originalScore = "", category = "weighting", severity = "high", finding = "f",
    recommendation = "Lower the score", option = "", optionGroup = "",
    suggestedCriteria = "", suggestedScore = "", suggestedType = "", applied = "",
    stringsAsFactors = FALSE
  )
  row$originalCriteria[is.na(row$originalCriteria)] <- ""
  overrides <- list(...)
  for (name in names(overrides)) {
    row[[name]] <- overrides[[name]]
  }
  row
}

makeReview <- function(...) {
  do.call(rbind, list(...))
}

makeShortRubricRow <- function(id, type, score, criteria, sql = "select 1", yes = 10) {
  data.frame(
    rubric = "R1", ruleId = id, type = type, score = score, criteria = criteria,
    rationale = "r", neededConceptSets = "a_DIAGNOSIS_CODES", sqlStatement = sql,
    rubricYesScore = yes, rubricNoScore = 5,
    stringsAsFactors = FALSE
  )
}
