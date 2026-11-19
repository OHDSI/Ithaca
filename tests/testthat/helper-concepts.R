# Runs `expr`, muffling its messages, and returns them with the value.
captureMessages <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    expr,
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = messages)
}

# A unit vector whose cosine similarity with c(1, 0, 0) is `cosine`.
vecWithCosine <- function(cosine) {
  c(cosine, sqrt(1 - cosine^2), 0)
}

# Replaces the embedding service: looks the text up in a named list of vectors; unknown text is an error.
fakeEmbeddingFunction <- function(vectors = list(), fallback = NULL) {
  force(vectors)
  calls <- new.env()
  calls$texts <- character()
  fn <- function(text, reportTime = TRUE, silent = FALSE) {
    calls$texts <- c(calls$texts, text)
    vector <- vectors[[text]]
    if (is.null(vector)) {
      vector <- fallback
    }
    if (is.null(vector)) {
      stop("no fake embedding for '", text, "'")
    }
    list(vector)
  }
  attr(fn, "calls") <- calls
  fn
}

# A library table as ithMatchConceptSets() receives it: conceptSetName plus an embedding per row.
makeEmbeddingLibrary <- function(...) {
  entries <- list(...)
  data.frame(
    conceptSetName = names(entries),
    cleanName = gsub("_", " ", names(entries)),
    embed = I(unname(entries)),
    stringsAsFactors = FALSE
  )
}

adjudicationResponse <- function(selected, confidence = "0.9", decision = "select_candidate") {
  function(kind, prompt, call) {
    list(decision = decision, selectedConceptSetName = selected, confidence = confidence, rationale = "because")
  }
}

# A Circe concept set with one included concept (and its descendants).
conceptSetWithConcept <- function(conceptId = 192671L, name = "Gastrointestinal hemorrhage") {
  list(items = list(list(
    concept = list(
      CONCEPT_ID = as.integer(conceptId), CONCEPT_NAME = name, DOMAIN_ID = "Condition",
      VOCABULARY_ID = "SNOMED", CONCEPT_CLASS_ID = "Clinical Finding", STANDARD_CONCEPT = "S",
      CONCEPT_CODE = "74474003", INVALID_REASON = "V"
    ),
    isExcluded = FALSE, includeDescendants = TRUE, includeMapped = FALSE
  )))
}

# Writes rubricList_V1.rds with just the needed concept sets, as concept-set creation reads it.
writeNeededConcepts <- function(outputDirectory, conditionName, conceptSetNames) {
  needed <- data.frame(
    type = "evidenceRules", rubric = 1L, rule = seq_along(conceptSetNames),
    criteria = paste("criteria", seq_along(conceptSetNames)),
    conceptSetsNeeded = conceptSetNames, stringsAsFactors = FALSE
  )
  Ithaca:::ithSaveVersioned(
    file.path(outputDirectory, conditionName),
    list(rubricList = list(), neededConcepts = needed),
    "rubricList.rds"
  )
}

libraryPathFor <- function(outputDirectory) {
  file.path(outputDirectory, "rubricConceptSetLibrary.duckdb")
}

addLibraryConceptSet <- function(outputDirectory, name, conceptSet = conceptSetWithConcept(), embedding = c(1, 0, 0)) {
  Ithaca:::ithDuckdbUpsertConceptSet(
    libraryPath = libraryPathFor(outputDirectory), conceptSetName = name,
    cleanName = gsub("_", " ", name), conceptSet = conceptSet, embedding = embedding
  )
}

libraryHistory <- function(outputDirectory) {
  connection <- Ithaca:::ithConnectDuckdbConceptLibrary(libraryPathFor(outputDirectory), readOnly = TRUE)
  on.exit(Ithaca:::ithDisconnectDuckdbConceptLibrary(connection), add = TRUE)
  DBI::dbGetQuery(connection, "SELECT * FROM libraryVersionHistory ORDER BY libraryVersion")
}

libraryConceptSets <- function(outputDirectory) {
  Ithaca:::ithDuckdbGetConceptSetLibrary(libraryPathFor(outputDirectory))
}

# Builds a rubric response as the LLM returns it: a diagnosis rule, a few other rules, and thresholds.
makeFakeRubricResponse <- function(rubricId = "R1",
                                   evidence = NULL,
                                   alternative = NULL,
                                   hard = NULL,
                                   thresholds = list(yes = 10L, no = 2L, insufficient = "between")) {
  evidenceRow <- function(ruleId, criteria, sets, points = 3L) {
    data.frame(
      ruleId = ruleId, rule_type = "evidence_rules", category = "peri_index", tier = "moderate", points = points,
      rationaleForPoints = "because", criteria = criteria, start_day = -30L, end_day = 30L,
      presenceOrAbsence = "presence", occurrencesNeeded = 1L, minimumTimeAfterFirstOccurrenceDays = NA_integer_,
      maximumTimeAfterFirstOccurrenceDays = NA_integer_, notes = "note", conceptSets_needed = sets,
      stringsAsFactors = FALSE
    )
  }
  if (is.null(evidence)) {
    evidence <- do.call(rbind, list(
      evidenceRow("ER1", ">=1 COPD diagnosis within -5 to +5 days of index", "copd_DIAGNOSIS_CODES", 3L),
      evidenceRow("ER2", "dx pattern", "copd_DIAGNOSIS_CODES,outpatient_VISIT_CODES", 2L),
      evidenceRow("ER3", "Tiotropium fill within -30 to +30 days", "tiotropium_MEDICATION_CODES", 4L),
      evidenceRow("ER4", "Spirometry within -30 to +30 days", "spirometry_MEASUREMENT_CODES", 3L)
    ))
  }
  if (is.null(alternative)) {
    alternative <- data.frame(
      ruleId = "AE1", rule_type = "alternative_explanation", criteria = "Asthma diagnosis within -30 to +30 days",
      start_day = -30L, end_day = 30L, presenceOrAbsence = "presence", occurrencesNeeded = 1L,
      minimumTimeAfterFirstOccurrenceDays = NA_integer_, maximumTimeAfterFirstOccurrenceDays = NA_integer_,
      points = -4L, rationaleForPoints = "because", cap_max_decision = "none", rationale = "because",
      conceptSets_needed = "asthma_DIAGNOSIS_CODES", stringsAsFactors = FALSE
    )
  }
  if (is.null(hard)) {
    hard <- data.frame(
      ruleId = "HE1", rule_type = "hard_exclusion", criteria = "Cystic fibrosis diagnosis within -365 to 0 days",
      start_day = -365L, end_day = 0L, presenceOrAbsence = "presence", occurrencesNeeded = 1L,
      minimumTimeAfterFirstOccurrenceDays = NA_integer_, maximumTimeAfterFirstOccurrenceDays = NA_integer_,
      action = "exclude", rationale = "because", conceptSets_needed = "cystic_fibrosis_DIAGNOSIS_CODES",
      stringsAsFactors = FALSE
    )
  }
  list(
    phenotype = "COPD", rubricVariationName = paste("Variation", rubricId), rubricId = rubricId,
    validation_goal = "goal", subject_id = "id",
    assumptions = list(index_condition_codes_removed = FALSE, labs_unavailable = TRUE),
    decisionThresholds = thresholds,
    hardExclusions = hard, alternativeExplanations = alternative, evidenceRules = evidence
  )
}
