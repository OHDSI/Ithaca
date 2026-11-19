ithDefaultRubricTemplates <- function(rubricTypesFile = file.path(ithPromptDir(), "rubricTypes.R")) {
  if (!file.exists(rubricTypesFile)) {
    stop("rubric types file not found: ", rubricTypesFile)
  }

  env <- new.env(parent = baseenv())
  sys.source(rubricTypesFile, envir = env)
  if (!exists("rubricListInput", envir = env, inherits = FALSE)) {
    stop("rubricListInput not found in ", rubricTypesFile)
  }

  get("rubricListInput", envir = env, inherits = FALSE)
}

ithCollectNeededConcepts <- function(rubricItem, rubricIndex) {
  needed <- NULL
  safeRowCount <- function(x) {
    if (is.data.frame(x)) {
      return(nrow(x))
    }
    0L
  }

  if (safeRowCount(rubricItem$hardExclusions) > 0) {
    for (ruleUp in seq_len(nrow(rubricItem$hardExclusions))) {
      needed <- rbind(
        needed,
        data.frame(
          type = "hardExclusions",
          rubric = rubricIndex,
          rule = ruleUp,
          criteria = rubricItem$hardExclusions$criteria[[ruleUp]],
          conceptSetsNeeded = rubricItem$hardExclusions$conceptSets_needed[[ruleUp]],
          stringsAsFactors = FALSE
        )
      )
    }
  }

  if (safeRowCount(rubricItem$alternativeExplanations) > 0) {
    for (ruleUp in seq_len(nrow(rubricItem$alternativeExplanations))) {
      needed <- rbind(
        needed,
        data.frame(
          type = "alternativeExplanations",
          rubric = rubricIndex,
          rule = ruleUp,
          criteria = rubricItem$alternativeExplanations$criteria[[ruleUp]],
          conceptSetsNeeded = rubricItem$alternativeExplanations$conceptSets_needed[[ruleUp]],
          stringsAsFactors = FALSE
        )
      )
    }
  }

  if (safeRowCount(rubricItem$evidenceRules) > 0) {
    for (ruleUp in seq_len(nrow(rubricItem$evidenceRules))) {
      needed <- rbind(
        needed,
        data.frame(
          type = "evidenceRules",
          rubric = rubricIndex,
          rule = ruleUp,
          criteria = rubricItem$evidenceRules$criteria[[ruleUp]],
          conceptSetsNeeded = rubricItem$evidenceRules$conceptSets_needed[[ruleUp]],
          stringsAsFactors = FALSE
        )
      )
    }
  }

  needed
}

ithRubricStructuredType <- function() {
  ellmer::type_object(
    phenotype = ellmer::type_string(),
    rubricVariationName = ellmer::type_string(),
    rubricId = ellmer::type_string(),
    validation_goal = ellmer::type_string(),
    subject_id = ellmer::type_string(),
    assumptions = ellmer::type_object(
      index_condition_codes_removed = ellmer::type_boolean(),
      labs_unavailable = ellmer::type_boolean()
    ),
    decisionThresholds = ellmer::type_object(
      yes = ellmer::type_integer(),
      no = ellmer::type_integer(),
      insufficient = ellmer::type_string()
    ),
    hardExclusions = ellmer::type_array(ellmer::type_object(
      ruleId = ellmer::type_string(),
      rule_type = ellmer::type_string(),
      criteria = ellmer::type_string(),
      start_day = ellmer::type_integer(),
      end_day = ellmer::type_integer(),
      presenceOrAbsence = ellmer::type_string(),
      occurrencesNeeded = ellmer::type_integer(),
      minimumTimeAfterFirstOccurrenceDays = ellmer::type_integer(),
      maximumTimeAfterFirstOccurrenceDays = ellmer::type_integer(),
      action = ellmer::type_string(),
      rationale = ellmer::type_string(),
      conceptSets_needed = ellmer::type_string()
    )),
    alternativeExplanations = ellmer::type_array(ellmer::type_object(
      ruleId = ellmer::type_string(),
      rule_type = ellmer::type_string(),
      criteria = ellmer::type_string(),
      start_day = ellmer::type_integer(),
      end_day = ellmer::type_integer(),
      presenceOrAbsence = ellmer::type_string(),
      occurrencesNeeded = ellmer::type_integer(),
      minimumTimeAfterFirstOccurrenceDays = ellmer::type_integer(),
      maximumTimeAfterFirstOccurrenceDays = ellmer::type_integer(),
      points = ellmer::type_integer(),
      rationaleForPoints = ellmer::type_string(),
      cap_max_decision = ellmer::type_string(),
      rationale = ellmer::type_string(),
      conceptSets_needed = ellmer::type_string()
    )),
    evidenceRules = ellmer::type_array(ellmer::type_object(
      ruleId = ellmer::type_string(),
      rule_type = ellmer::type_string(),
      category = ellmer::type_string(),
      tier = ellmer::type_string(),
      points = ellmer::type_integer(),
      rationaleForPoints = ellmer::type_string(),
      criteria = ellmer::type_string(),
      start_day = ellmer::type_integer(),
      end_day = ellmer::type_integer(),
      presenceOrAbsence = ellmer::type_string(),
      occurrencesNeeded = ellmer::type_integer(),
      minimumTimeAfterFirstOccurrenceDays = ellmer::type_integer(),
      maximumTimeAfterFirstOccurrenceDays = ellmer::type_integer(),
      notes = ellmer::type_string(),
      conceptSets_needed = ellmer::type_string()
    ))
  )
}

ithConceptSetHintDecisionType <- function() {
  ellmer::type_array(
    ellmer::type_object(
      requestedConceptSetName = ellmer::type_string(),
      chosenConceptSetName = ellmer::type_string(),
      decisionSource = ellmer::type_string(),
      rationale = ellmer::type_string()
    )
  )
}

ithCollectNeededConceptsFromRubricList <- function(rubricList) {
  needed <- NULL

  for (rubricUp in seq_along(rubricList)) {
    rubricItem <- rubricList[[rubricUp]]
    if (is.null(rubricItem)) {
      next
    }
    needed <- rbind(needed, ithCollectNeededConcepts(rubricItem, rubricUp))
  }

  if (is.null(needed)) {
    needed <- data.frame(
      type = character(),
      rubric = character(),
      rule = integer(),
      criteria = character(),
      conceptSetsNeeded = character(),
      stringsAsFactors = FALSE
    )
  }

  needed
}

ithBuildConceptSetHintPrompt <- function(conditionName, hintTable) {
  hintJson <- jsonlite::toJSON(hintTable, auto_unbox = TRUE, pretty = TRUE)

  paste0(
    "You are revising concept set names for a rubric.\n",
    "Condition: ", conditionName, "\n\n",
    "Goals:\n",
    "1) Prefer high-quality existing concept set library names when appropriate.\n",
    "2) Do not force a library match if candidates do not fit clinically.\n",
    "3) You may create a new concept set name when needed.\n",
    "4) Keep names in lower_snake_case and include a suffix like _condition_codes, _procedure_codes, _medication_codes, _visit_codes, _measurement_codes, _observation_codes, or _device_codes.\n",
    "5) Do not invent broad replacements that change the clinical meaning.\n\n",
    "For each requested concept set, output one decision with:\n",
    "- requestedConceptSetName\n",
    "- chosenConceptSetName\n",
    "- decisionSource (library_preferred | adapted_candidate | create_new)\n",
    "- rationale\n\n",
    "Candidate hints table (top candidates and diagnostics):\n",
    hintJson
  )
}

ithNormalizeConceptSetHintDecisions <- function(decisionsRaw) {
  if (is.null(decisionsRaw)) {
    return(data.frame())
  }

  decisionsDf <- tryCatch(as.data.frame(decisionsRaw, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(decisionsDf) == 0) {
    return(data.frame())
  }

  required <- c("requestedConceptSetName", "chosenConceptSetName", "decisionSource", "rationale")
  for (nameUp in required) {
    if (!(nameUp %in% names(decisionsDf))) {
      decisionsDf[[nameUp]] <- ""
    }
  }

  decisionsDf <- decisionsDf[, required]
  decisionsDf$requestedConceptSetName <- as.character(decisionsDf$requestedConceptSetName)
  decisionsDf$chosenConceptSetName <- as.character(decisionsDf$chosenConceptSetName)
  decisionsDf$decisionSource <- as.character(decisionsDf$decisionSource)
  decisionsDf$rationale <- as.character(decisionsDf$rationale)

  decisionsDf$requestedConceptSetNormalized <- ithNormalizeConceptSetName(decisionsDf$requestedConceptSetName)
  decisionsDf$chosenConceptSetNormalized <- ithNormalizeConceptSetName(decisionsDf$chosenConceptSetName)
  decisionsDf <- decisionsDf[nzchar(decisionsDf$requestedConceptSetNormalized) & nzchar(decisionsDf$chosenConceptSetNormalized), ]
  decisionsDf <- decisionsDf[!duplicated(decisionsDf$requestedConceptSetNormalized), ]

  decisionsDf
}

ithRewriteConceptSetStringFromDecisionMap <- function(conceptSetString, decisionMap) {
  conceptNames <- ithParseConceptSets(conceptSetString)
  if (length(conceptNames) == 0) {
    return(conceptSetString)
  }

  mapped <- vapply(conceptNames, function(nameUp) {
    normalized <- ithNormalizeConceptSetName(nameUp)
    if (normalized %in% names(decisionMap)) {
      return(decisionMap[[normalized]])
    }
    normalized
  }, character(1))

  mapped <- unique(mapped[nzchar(mapped)])
  if (length(mapped) == 0) {
    return(conceptSetString)
  }

  paste(mapped, collapse = ", ")
}

ithApplyConceptSetDecisionsToRubricList <- function(rubricList, decisionsDf) {
  if (is.null(decisionsDf) || nrow(decisionsDf) == 0) {
    return(list(rubricList = rubricList, decisionMap = c()))
  }

  decisionMap <- setNames(as.character(decisionsDf$chosenConceptSetNormalized), as.character(decisionsDf$requestedConceptSetNormalized))
  decisionMap <- decisionMap[nzchar(names(decisionMap)) & nzchar(decisionMap)]

  if (length(decisionMap) == 0) {
    return(list(rubricList = rubricList, decisionMap = c()))
  }

  tableNames <- c("hardExclusions", "alternativeExplanations", "evidenceRules")

  for (rubricUp in seq_along(rubricList)) {
    rubricItem <- rubricList[[rubricUp]]
    if (is.null(rubricItem)) {
      next
    }

    for (tableNameUp in tableNames) {
      ruleTable <- rubricItem[[tableNameUp]]
      if (!is.data.frame(ruleTable) || nrow(ruleTable) == 0 || !("conceptSets_needed" %in% names(ruleTable))) {
        next
      }

      for (rowUp in seq_len(nrow(ruleTable))) {
        ruleTable$conceptSets_needed[[rowUp]] <- ithRewriteConceptSetStringFromDecisionMap(
          conceptSetString = ruleTable$conceptSets_needed[[rowUp]],
          decisionMap = decisionMap
        )
      }

      rubricItem[[tableNameUp]] <- ruleTable
    }

    rubricList[[rubricUp]] <- rubricItem
  }

  list(rubricList = rubricList, decisionMap = decisionMap)
}

ithRunRubricConceptSetHintPass <- function(rubricList,
                                           neededConcepts,
                                           conditionName,
                                           outputDirectory,
                                           conditionDir,
                                           llmClient,
                                           similarityThreshold = 0.94,
                                           topCandidates = 5,
                                           verbose = TRUE) {
  if (is.null(neededConcepts) || nrow(neededConcepts) == 0) {
    return(list(
      rubricList = rubricList,
      neededConcepts = neededConcepts,
      hintPath = NULL,
      decisionPath = NULL,
      decisionTable = data.frame()
    ))
  }

  csLibrary <- tryCatch(ithLoadLatestConceptSetLibrary(outputDirectory), error = function(e) NULL)
  if (is.null(csLibrary) || length(csLibrary) < 2 || nrow(csLibrary[[2]]) == 0) {
    ithMaybeMessage(verbose, "Concept set hint pass skipped: no concept set name library available.")
    return(list(
      rubricList = rubricList,
      neededConcepts = neededConcepts,
      hintPath = NULL,
      decisionPath = NULL,
      decisionTable = data.frame()
    ))
  }

  neededNames <- ithExtractNeededConceptSetNames(neededConcepts)
  if (length(neededNames) == 0) {
    return(list(
      rubricList = rubricList,
      neededConcepts = neededConcepts,
      hintPath = NULL,
      decisionPath = NULL,
      decisionTable = data.frame()
    ))
  }

  matchResult <- ithMatchConceptSets(
    neededNames,
    csLibrary[[2]],
    similarityThreshold = similarityThreshold,
    returnDiagnostics = TRUE,
    topCandidates = topCandidates,
    includeHecateCandidates = TRUE
  )

  diagnostics <- matchResult$diagnostics
  if (is.null(diagnostics) || nrow(diagnostics) == 0) {
    ithMaybeMessage(verbose, "Concept set hint pass skipped: no diagnostics produced from library matching.")
    return(list(
      rubricList = rubricList,
      neededConcepts = neededConcepts,
      hintPath = NULL,
      decisionPath = NULL,
      decisionTable = data.frame()
    ))
  }

  keep <- intersect(
    c(
      "csName",
      "csNameNormalized",
      "matchMethod",
      "matchedLibCsName",
      "bestCandidate",
      "bestCandidateEmbeddingSim",
      "bestCandidateFuzzySim",
      "bestCandidateCombinedSim",
      "libraryTopCandidates",
      "hecateSearchTerm",
      "hecateTopCandidatesAccepted",
      "hecateTopCandidatesRejected",
      "note"
    ),
    names(diagnostics)
  )
  hintTable <- diagnostics[, keep, drop = FALSE]
  hintPath <- ithSaveVersioned(
    saveDirectory = conditionDir,
    objectToSave = hintTable,
    fileName = "conceptSetCandidateHints.csv"
  )
  ithMaybeMessage(verbose, "Saved concept set candidate hints: ", hintPath)

  decisionPrompt <- ithBuildConceptSetHintPrompt(conditionName = conditionName, hintTable = hintTable)
  decisionsRaw <- ithQueryLlm(
    llmClient = llmClient,
    prompt = decisionPrompt,
    output = "list",
    ellmerTypeObject = ithConceptSetHintDecisionType(),
    silent = !isTRUE(verbose)
  )

  decisions <- ithNormalizeConceptSetHintDecisions(decisionsRaw)
  if (nrow(decisions) == 0) {
    ithMaybeMessage(verbose, "Concept set hint pass produced no usable decisions.")
    return(list(
      rubricList = rubricList,
      neededConcepts = neededConcepts,
      hintPath = hintPath,
      decisionPath = NULL,
      decisionTable = data.frame()
    ))
  }

  applyResult <- ithApplyConceptSetDecisionsToRubricList(rubricList = rubricList, decisionsDf = decisions)
  decisionMap <- applyResult$decisionMap
  decisions$applied <- decisions$requestedConceptSetNormalized %in% names(decisionMap)
  decisions$changed <- decisions$applied & (decisions$requestedConceptSetNormalized != decisions$chosenConceptSetNormalized)
  decisions$conditionName <- conditionName

  decisionPath <- ithSaveVersioned(
    saveDirectory = conditionDir,
    objectToSave = decisions,
    fileName = "rubricConceptSetDecisions.csv"
  )
  ithMaybeMessage(verbose, "Saved concept set decision audit: ", decisionPath)

  list(
    rubricList = applyResult$rubricList,
    neededConcepts = ithCollectNeededConceptsFromRubricList(applyResult$rubricList),
    hintPath = hintPath,
    decisionPath = decisionPath,
    decisionTable = decisions
  )
}

ithRubricCheckpointPath <- function(conditionDir, conditionName) {
  file.path(conditionDir, paste0(conditionName, "_rubricCheckpoint.rds"))
}

ithLoadRubricCheckpoint <- function(conditionDir, conditionName) {
  path <- ithRubricCheckpointPath(conditionDir, conditionName)
  if (!file.exists(path)) {
    return(NULL)
  }

  readRDS(path)
}

ithSaveRubricCheckpoint <- function(conditionDir, conditionName, checkpoint, verbose = TRUE) {
  path <- ithRubricCheckpointPath(conditionDir, conditionName)
  saveRDS(checkpoint, path)
  ithMaybeMessage(verbose, "Saved rubric checkpoint: ", path)
  invisible(path)
}

# Returns why an LLM rubric response is unusable; empty when it is usable.
ithRubricResponseProblems <- function(rubricItem) {
  isBlank <- function(x) {
    is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(trimws(as.character(x[[1]])))
  }
  if (!is.list(rubricItem) || length(rubricItem) == 0) {
    return("empty response")
  }

  problems <- character(0)
  if (isBlank(rubricItem$rubricId)) {
    problems <- c(problems, "missing rubricId")
  }
  if (isBlank(rubricItem$decisionThresholds$yes) || isBlank(rubricItem$decisionThresholds$no)) {
    problems <- c(problems, "missing decision thresholds")
  }
  if (!is.data.frame(rubricItem$evidenceRules) || nrow(rubricItem$evidenceRules) == 0) {
    problems <- c(problems, "no evidence rules")
  }
  problems
}

# Generated rubrics always use YES = 10 and NO = 2, ER1 = the index diagnosis rule (3 points)
# and ER2 = the condition's dxCriteria (2 + the choice's additionalPoints). Whatever the LLM returned for these is
# overwritten, other copies of the two diagnosis rules are dropped, and the LLM's remaining
# evidence rules are renumbered from ER3. A configured adjustableRuleScore is applied after those fixed rules are built.
ithEnforceFixedRubricRules <- function(rubricItem,
                                       conditionName,
                                       dxCriteria,
                                       dxAdditionalPoints = 0L,
                                       zeroOccurrencePenalty = NULL,
                                       adjustableRuleId = NULL,
                                       adjustableRuleScore = NULL) {
  changes <- character()
  if (!is.list(rubricItem) || length(rubricItem) == 0L) {
    return(list(item = rubricItem, changes = changes))
  }
  er2Points <- ithEr2Points(dxAdditionalPoints)
  conditionName <- trimws(as.character(conditionName[[1]]))
  dxText <- if (is.null(dxCriteria) || length(dxCriteria) == 0L || is.na(dxCriteria[[1]])) "" else trimws(as.character(dxCriteria[[1]]))
  if (!nzchar(dxText)) {
    stop("dxCriteria is blank for '", conditionName, "'; ER2 cannot be built.", call. = FALSE)
  }
  diagnosisSet <- paste0(gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", gsub("['\u2019]", "", tolower(conditionName)))), "_DIAGNOSIS_CODES")
  er1Criteria <- paste0(">=1 ", conditionName, " diagnosis within -5 to +5 days of index")
  scalar <- function(x) if (is.null(x) || length(x) == 0L) NA else x[[1]]

  thresholds <- rubricItem$decisionThresholds
  if (!is.list(thresholds)) {
    thresholds <- list()
  }
  for (side in c("yes", "no")) {
    target <- if (side == "yes") 10 else 2
    old <- suppressWarnings(as.numeric(scalar(thresholds[[side]])))
    if (is.na(old) || old != target) {
      changes <- c(changes, paste0(toupper(side), " threshold ", old, " -> ", target))
    }
  }
  thresholds$yes <- 10L
  thresholds$no <- 2L
  thresholds$insufficient <- "between 2 and 10 or unmet YES constraints"
  rubricItem$decisionThresholds <- thresholds

  rules <- rubricItem$evidenceRules
  if (!is.data.frame(rules)) {
    rules <- data.frame(ruleId = character(), criteria = character(), stringsAsFactors = FALSE)
  }
  ids <- if ("ruleId" %in% names(rules)) toupper(trimws(as.character(rules$ruleId))) else rep("", nrow(rules))
  ids[is.na(ids)] <- ""
  criteriaKey <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))
  ruleCriteria <- if ("criteria" %in% names(rules)) as.character(rules$criteria) else rep(NA_character_, nrow(rules))
  ruleSets <- if ("conceptSets_needed" %in% names(rules)) as.character(rules$conceptSets_needed) else rep(NA_character_, nrow(rules))
  # An LLM ER1/ER2 is only treated as the fixed rule when it is a diagnosis rule; anything else
  # (for example a medication rule numbered ER1) is kept and renumbered.
  isDiagnosisRule <- grepl("_DIAGNOSIS_CODES", toupper(ruleSets)) | grepl("diagnos", ruleCriteria, ignore.case = TRUE)
  isFixed <- ids %in% c("ER1", "ER2") & isDiagnosisRule

  # ER2 keeps the LLM's visit/provider sets for the care settings in dxCriteria.
  llmEr2Sets <- unlist(lapply(ruleSets[ids == "ER2" & isFixed], ithParseConceptSets))
  llmEr2Sets <- llmEr2Sets[!grepl("_DIAGNOSIS_CODES$", toupper(llmEr2Sets))]
  if (length(llmEr2Sets) == 0L) {
    settingSets <- c(
      inpatient = "inpatient_VISIT_CODES",
      "emergency department" = "emergency_department_VISIT_CODES",
      outpatient = "outpatient_VISIT_CODES"
    )
    llmEr2Sets <- unname(settingSets[vapply(names(settingSets), grepl, logical(1), x = dxText, ignore.case = TRUE)])
  }
  er2Sets <- unique(c(diagnosisSet, llmEr2Sets))

  windows <- regmatches(dxText, gregexpr("[-+]?[0-9]+\\s+to\\s+[-+]?[0-9]+", dxText))[[1]]
  windowDays <- suppressWarnings(as.integer(unlist(regmatches(windows, gregexpr("[-+]?[0-9]+", windows)))))
  er2Start <- if (length(windowDays) > 0L) min(windowDays) else NA_integer_
  er2End <- if (length(windowDays) > 0L) max(windowDays) else NA_integer_

  for (fixedId in c("ER1", "ER2")) {
    expected <- if (fixedId == "ER1") er1Criteria else dxText
    row <- which(ids == fixedId & isFixed)
    if (length(row) == 0L) {
      changes <- c(changes, paste0(fixedId, " added (LLM did not return it as a diagnosis rule)"))
    } else if (!identical(criteriaKey(ruleCriteria[[row[[1]]]]), criteriaKey(expected))) {
      changes <- c(changes, paste0(fixedId, " replaced (LLM criteria: ", ruleCriteria[[row[[1]]]], ")"))
    } else if ("points" %in% names(rules)) {
      expectedPoints <- if (fixedId == "ER1") 3L else er2Points
      oldPoints <- suppressWarnings(as.numeric(rules$points[[row[[1]]]]))
      if (is.na(oldPoints) || oldPoints != expectedPoints) {
        changes <- c(changes, paste0(fixedId, " points ", oldPoints, " -> ", expectedPoints))
      }
    }
  }

  isDuplicate <- !isFixed & criteriaKey(ruleCriteria) %in% criteriaKey(c(er1Criteria, dxText))
  if (any(isDuplicate)) {
    changes <- c(changes, paste0(ids[isDuplicate], " dropped (duplicates ER1/ER2)"))
  }
  others <- rules[!isFixed & !isDuplicate, , drop = FALSE]
  if (nrow(others) > 0L) {
    newIds <- paste0("ER", seq_len(nrow(others)) + 2L)
    oldIds <- if ("ruleId" %in% names(others)) toupper(trimws(as.character(others$ruleId))) else rep("", nrow(others))
    if (!identical(oldIds, newIds)) {
      changes <- c(changes, paste0("other evidence rules renumbered from ER3 (", paste0(oldIds, "->", newIds, collapse = ", "), ")"))
    }
    others$ruleId <- newIds
  }

  fixedRow <- function(ruleId, points, rationale, criteria, startDay, endDay, occurrences, conceptSets) {
    data.frame(
      ruleId = ruleId,
      rule_type = "evidence_rules",
      category = "peri_index",
      tier = "moderate",
      points = as.integer(points),
      rationaleForPoints = rationale,
      criteria = criteria,
      start_day = as.integer(startDay),
      end_day = as.integer(endDay),
      presenceOrAbsence = "presence",
      occurrencesNeeded = as.integer(occurrences),
      minimumTimeAfterFirstOccurrenceDays = NA_integer_,
      maximumTimeAfterFirstOccurrenceDays = NA_integer_,
      notes = "Fixed rule set by Ithaca; not generated by the LLM.",
      conceptSets_needed = conceptSets,
      stringsAsFactors = FALSE
    )
  }
  er1 <- fixedRow("ER1", 3L, "Fixed: index diagnosis is worth 3 of the 10 YES points.", er1Criteria, -5L, 5L, 1L, diagnosisSet)
  er2 <- fixedRow(
    "ER2", er2Points,
    paste0("Fixed: dxCriteria diagnosis pattern is worth ", er2Points, " of the 10 YES points (2 + additionalPoints ", er2Points - 2L, ")."),
    dxText, er2Start, er2End, NA_integer_, paste(er2Sets, collapse = ",")
  )
  rubricItem$evidenceRules <- ithBindRows(if (nrow(others) > 0L) list(er1, er2, others) else list(er1, er2))
  rownames(rubricItem$evidenceRules) <- NULL

  if (!is.null(adjustableRuleScore)) {
    scoreRuleIds <- ithParseRuleIdList(adjustableRuleId, rowLabel = conditionName)
    if (length(scoreRuleIds) == 0L) {
      scoreRuleIds <- "ER1"
    }
    scoreRuleIds <- unique(toupper(scoreRuleIds))
    rubricRuleIds <- toupper(trimws(as.character(rubricItem$evidenceRules$ruleId)))
    missingRuleIds <- setdiff(scoreRuleIds, rubricRuleIds)
    if (length(missingRuleIds) > 0L) {
      stop(
        "adjustableRuleId value(s) not found in generated rubric for '", conditionName, "': ",
        paste(missingRuleIds, collapse = ", "),
        call. = FALSE
      )
    }
    for (ruleId in scoreRuleIds) {
      ruleRows <- which(rubricRuleIds == ruleId)
      oldScores <- suppressWarnings(as.numeric(rubricItem$evidenceRules$points[ruleRows]))
      rubricItem$evidenceRules$points[ruleRows] <- adjustableRuleScore
      changes <- c(
        changes,
        paste0(ruleId, " points ", paste(unique(oldScores), collapse = "/"), " -> ", adjustableRuleScore)
      )
    }
  }

  # Fixed AE1 (zero-occurrence penalty) goes first; the LLM's alternative explanations start at AE2.
  penalty <- ithAe1ParsePenalty(zeroOccurrencePenalty)
  oldAe <- rubricItem$alternativeExplanations
  rubricItem$alternativeExplanations <- ithAe1ApplyToRuleTable(oldAe, conditionName, penalty)
  if (is.data.frame(oldAe) && nrow(oldAe) > 0L) {
    changes <- c(changes, "alternative explanations renumbered from AE2 (fixed AE1 zero-occurrence penalty added)")
  } else {
    changes <- c(changes, "fixed AE1 zero-occurrence penalty added")
  }

  list(item = rubricItem, changes = changes)
}

# ER2 points = 2 + the dxCriteria choice's additionalPoints (0-4; blank means 0).
ithEr2Points <- function(dxAdditionalPoints) {
  extra <- if (is.null(dxAdditionalPoints) || length(dxAdditionalPoints) == 0L || is.na(dxAdditionalPoints[[1]])) {
    0
  } else {
    suppressWarnings(as.numeric(dxAdditionalPoints[[1]]))
  }
  if (is.na(extra) || extra != round(extra) || extra < 0 || extra > 4) {
    stop("dxCriteria additionalPoints must be a whole number from 0 to 4; got ", dxAdditionalPoints[[1]], call. = FALSE)
  }
  2L + as.integer(extra)
}

ithLatestConciseRubricHasRules <- function(conditionDir, conditionName) {
  latest <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
  if (is.null(latest)) {
    return(FALSE)
  }
  path <- file.path(conditionDir, latest)
  rows <- tryCatch(nrow(utils::read.csv(path, stringsAsFactors = FALSE)), error = function(e) 0L)
  isTRUE(rows > 0)
}

ithRubricArtifactsExist <- function(conditionDir, conditionName) {
  rubricListExists <- !is.null(ithFindLatestVersion(conditionDir, "rubricList.rds"))
  shortRubricExists <- !is.null(ithFindLatestVersion(conditionDir, paste0(conditionName, "_shortRubric.rds")))
  conciseRubricExists <- !is.null(ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv")))

  rubricListExists && shortRubricExists && conciseRubricExists
}

ithResetRubricCheckpoint <- function(conditionName,
                                     outputDirectory,
                                     deleteArtifacts = FALSE,
                                     verbose = TRUE) {
  conditionDir <- file.path(outputDirectory, conditionName)
  checkpointPath <- ithRubricCheckpointPath(conditionDir, conditionName)

  removed <- list(
    checkpoint = character(0),
    artifacts = character(0)
  )

  if (file.exists(checkpointPath)) {
    file.remove(checkpointPath)
    removed$checkpoint <- checkpointPath
    ithMaybeMessage(verbose, "Removed rubric checkpoint: ", checkpointPath)
  } else {
    ithMaybeMessage(verbose, "No rubric checkpoint found at: ", checkpointPath)
  }

  if (isTRUE(deleteArtifacts) && dir.exists(conditionDir)) {
    pattern <- paste0(
      "^(rubricList_V[0-9]+\\.rds|",
      ithEscapeRegex(conditionName), "_shortRubric_V[0-9]+\\.rds|",
      ithEscapeRegex(conditionName), "_conciseRubric_V[0-9]+\\.csv|",
      ithEscapeRegex(conditionName), "_RuleSummary_V[0-9]+\\.(txt|text))$"
    )

    artifactFiles <- list.files(conditionDir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    if (length(artifactFiles) > 0) {
      file.remove(artifactFiles)
      removed$artifacts <- artifactFiles
      ithMaybeMessage(verbose, "Removed ", length(artifactFiles), " rubric artifact file(s) for condition: ", conditionName)
    } else {
      ithMaybeMessage(verbose, "No rubric artifacts found to remove for condition: ", conditionName)
    }
  }

  invisible(removed)
}

ithCreateShortRubric <- function(rubricList,
                                 llmClient,
                                 promptDir = NULL,
                                 verbose = TRUE,
                                 sqlLlmClientFactory = function() ithGetLlmClientFromConfig(list(), "reasoning"),
                                 existingShortRubric = NULL,
                                 checkpointCallback = NULL) {
  shortRubric <- existingShortRubric
  for (rubricUp in seq_along(rubricList)) {
    rubricItem <- rubricList[[rubricUp]]
    ithMaybeMessage(verbose, "Building short rubric entries for rubric ", rubricUp, "/", length(rubricList), ".")

    appendRules <- function(ruleDf, scoreCol, typeName, rationaleCol) {
      out <- NULL
      # An empty rule list from the LLM may be NULL or list(), not a zero-row data frame.
      if (!is.data.frame(ruleDf) || nrow(ruleDf) == 0) {
        return(out)
      }

      for (ruleUp in seq_len(nrow(ruleDf))) {
        existingRow <- integer(0)
        if (!is.null(shortRubric) && nrow(shortRubric) > 0) {
          existingRow <- which(
            shortRubric$rubric == rubricItem$rubricId[[1]] &
              shortRubric$ruleId == ruleDf$ruleId[[ruleUp]] &
              shortRubric$type == typeName
          )
        }

        if (length(existingRow) > 0) {
          ithMaybeMessage(
            verbose,
            "Reusing previously generated SQL for ", typeName,
            " rule ", ruleUp, "/", nrow(ruleDf),
            " (ruleId=", ruleDf$ruleId[[ruleUp]], ")."
          )
          out <- rbind(out, shortRubric[existingRow[[1]], , drop = FALSE])
          next
        }

        ithMaybeMessage(
          verbose,
          "Generating SQL for ", typeName,
          " rule ", ruleUp, "/", nrow(ruleDf),
          " (ruleId=", ruleDf$ruleId[[ruleUp]], ")."
        )
        isFixedAe1 <- typeName == "alternativeExplanations" &&
          ithAe1IsFixed(ruleDf$ruleId[[ruleUp]], ruleDf$conceptSets_needed[[ruleUp]])
        sql <- if (isFixedAe1) {
          ithAe1Sql(ithParseConceptSets(ruleDf$conceptSets_needed[[ruleUp]]))
        } else {
          sqlLlmClient <- if (is.null(sqlLlmClientFactory)) {
            llmClient
          } else {
            sqlLlmClientFactory()
          }
          ithCreateSqlStatement(
            criteria = ruleDf$criteria[[ruleUp]],
            conceptSets = ruleDf$conceptSets_needed[[ruleUp]],
            llmClient = sqlLlmClient,
            promptDir = promptDir
          )
        }

        scoreValue <- if (length(scoreCol) == 1 && is.character(scoreCol)) {
          ruleDf[[scoreCol]][[ruleUp]]
        } else {
          as.numeric(scoreCol)
        }

        newRow <- data.frame(
          condition = rubricItem$phenotype[[1]],
          rubric = rubricItem$rubricId[[1]],
          rubricName = rubricItem$rubricVariationName[[1]],
          rubricYesScore = rubricItem$decisionThresholds$yes[[1]],
          rubricNoScore = rubricItem$decisionThresholds$no[[1]],
          ruleId = ruleDf$ruleId[[ruleUp]],
          type = typeName,
          score = scoreValue,
          criteria = ruleDf$criteria[[ruleUp]],
          rationale = ruleDf[[rationaleCol]][[ruleUp]],
          neededConceptSets = ruleDf$conceptSets_needed[[ruleUp]],
          sqlStatement = sql,
          okToUse = TRUE,
          stringsAsFactors = FALSE
        )

        out <- rbind(out, newRow)

        if (is.function(checkpointCallback)) {
          checkpointCallback(rbind(shortRubric, out))
        }
      }

      out
    }

    shortRubric <- rbind(shortRubric, appendRules(rubricItem$hardExclusions, -100, "hard_exclusion", "rationale"))
    shortRubric <- rbind(shortRubric, appendRules(rubricItem$alternativeExplanations, "points", "alternativeExplanations", "rationale"))
    shortRubric <- rbind(shortRubric, appendRules(rubricItem$evidenceRules, "points", "evidenceRules", "notes"))
  }

  shortRubric
}

ithCreateRubrics <- function(conditionSpecs,
                               outputDirectory,
                               rubricTemplates = NULL,
                               llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                               promptDir = NULL,
                               includeLabs = FALSE,
                               saveSummary = TRUE,
                               useLibraryHints = FALSE,
                               libraryHintSimilarityThreshold = 0.94,
                               libraryHintTopCandidates = 5,
                               useExistingRubric = FALSE,
                               verbose = TRUE,
                               rubricResponseAttempts = 3) {
  ithMaybeMessage(verbose, "Starting rubric creation for ", length(conditionSpecs), " condition(s).")
  if (is.null(rubricTemplates)) {
    rubricTemplates <- ithDefaultRubricTemplates()
  }
  ithMaybeMessage(verbose, "Using ", length(rubricTemplates), " rubric template(s).")

  for (conditionUp in seq_along(conditionSpecs)) {
    condition <- conditionSpecs[[conditionUp]]
    ithMaybeMessage(verbose, "Creating rubric for condition ", conditionUp, "/", length(conditionSpecs), ": ", condition$testCondition)
    additionalInformation <- if (is.null(condition$additionalInformation) ||
      length(condition$additionalInformation) == 0L ||
      is.na(condition$additionalInformation[[1]]) ||
      !nzchar(trimws(as.character(condition$additionalInformation[[1]])))) {
      "<none>"
    } else {
      as.character(condition$additionalInformation[[1]])
    }
    ithMaybeMessage(verbose, "Additional information used: ", additionalInformation)
    conditionDir <- file.path(outputDirectory, condition$testCondition)
    dir.create(conditionDir, recursive = TRUE, showWarnings = FALSE)

    if (isTRUE(useExistingRubric) && ithRubricArtifactsExist(conditionDir, condition$testCondition)) {
      if (ithLatestConciseRubricHasRules(conditionDir, condition$testCondition)) {
        ithMaybeMessage(verbose, "Skipping rubric generation for condition '", condition$testCondition, "' because existing rubric artifacts were found and useExistingRubric=TRUE.")
        next
      }
      message(
        "Latest concise rubric for '", condition$testCondition,
        "' has no rules; generating a new rubric version despite useExistingRubric=TRUE."
      )
    }

    checkpoint <- ithLoadRubricCheckpoint(conditionDir, condition$testCondition)
    if (!is.null(checkpoint)) {
      if (identical(checkpoint$status, "completed") && !isTRUE(useExistingRubric)) {
        ithMaybeMessage(verbose, "Existing completed checkpoint found for condition '", condition$testCondition, "' but useExistingRubric=FALSE, so a fresh rubric run will be created.")
        checkpoint <- NULL
      } else {
        ithMaybeMessage(verbose, "Resuming rubric generation from checkpoint for condition: ", condition$testCondition)
      }
    }

    rubricList <- if (!is.null(checkpoint$rubricList)) checkpoint$rubricList else vector("list", length(rubricTemplates))
    if (length(rubricList) < length(rubricTemplates)) {
      length(rubricList) <- length(rubricTemplates)
    }

    needed <- checkpoint$neededConcepts
    shortRubricSeed <- checkpoint$shortRubric
    conceptSetHintsApplied <- isTRUE(checkpoint$conceptSetHintsApplied)

    for (templateUp in seq_along(rubricTemplates)) {
      if (!is.null(rubricList[[templateUp]])) {
        checkpointProblems <- ithRubricResponseProblems(rubricList[[templateUp]])
        if (length(checkpointProblems) == 0) {
          ithMaybeMessage(verbose, "Template ", templateUp, "/", length(rubricTemplates), " already exists in checkpoint; skipping regeneration.")
          next
        }
        message(
          "Template ", templateUp, "/", length(rubricTemplates), " in the checkpoint is incomplete (",
          paste(checkpointProblems, collapse = "; "), "); regenerating it."
        )
        rubricList[templateUp] <- list(NULL)
        if (!is.null(needed) && nrow(needed) > 0) {
          needed <- needed[needed$rubric != templateUp, , drop = FALSE]
        }
        if (!is.null(shortRubricSeed) && nrow(shortRubricSeed) > 0) {
          # Drop SQL built from the broken response; rule IDs from a new response may differ.
          shortRubricSeed <- NULL
        }
      }

      ithMaybeMessage(verbose, "Preparing rubric prompt template ", templateUp, "/", length(rubricTemplates), ".")
      template <- rubricTemplates[[templateUp]]
      prompt <- ithReadPrompt("scoringRubric.txt", promptDir = promptDir)
      prompt <- gsub("<PHENOTYPE_NAME>", condition$testCondition, prompt, fixed = TRUE)
      prompt <- gsub("<RUBRIC_SPECIFICS>", template, prompt, fixed = TRUE)
      prompt <- gsub("<DX_CRITERA>", condition$dxCriteria, prompt, fixed = TRUE)
      er2Points <- ithEr2Points(condition$dxAdditionalPoints)
      prompt <- gsub("<ER2_POINTS>", as.character(er2Points), prompt, fixed = TRUE)
      prompt <- gsub("<DX_RULE_POINTS>", as.character(3L + er2Points), prompt, fixed = TRUE)
      prompt <- gsub("<ADDITIONAL_INFORMATION>", condition$additionalInformation %||% "", prompt, fixed = TRUE)

      if (includeLabs) {
        prompt <- gsub("<EXCLUDE_LABS>", "", prompt, fixed = TRUE)
        prompt <- gsub("<INCLUDE_LABS>", "- Lab values and results", prompt, fixed = TRUE)
      } else {
        prompt <- gsub("<EXCLUDE_LABS>", "- Lab values or results", prompt, fixed = TRUE)
        prompt <- gsub("<INCLUDE_LABS>", "- the presence of a laboratory measurement (without lab values)", prompt, fixed = TRUE)
      }

      rubricResponse <- NULL
      responseProblems <- character(0)
      for (rubricAttempt in seq_len(rubricResponseAttempts)) {
        ithMaybeMessage(verbose, "Submitting rubric template ", templateUp, " to LLM (attempt ", rubricAttempt, "/", rubricResponseAttempts, ")...")
        rubricResponse <- ithQueryLlm(
          llmClient = llmClient,
          prompt = prompt,
          output = "list",
          ellmerTypeObject = ithRubricStructuredType(),
          silent = !isTRUE(verbose)
        )
        if (length(ithRubricResponseProblems(rubricResponse)) == 0L) {
          fixed <- ithEnforceFixedRubricRules(
            rubricResponse,
            condition$testCondition,
            condition$dxCriteria,
            condition$dxAdditionalPoints,
            condition$zeroOccurrencePenalty,
            condition$adjustableRuleId,
            condition$adjustableRuleScore
          )
          rubricResponse <- fixed$item
          if (length(fixed$changes) > 0L) {
            message("Enforced fixed ER1/ER2/AE1 and YES/NO thresholds in template ", templateUp, ": ", paste(fixed$changes, collapse = "; "))
          }
        }
        cleaned <- ithCleanRubricResponseConceptSets(
          rubricResponse,
          knownNames = if (is.null(needed)) character() else unlist(lapply(needed$conceptSetsNeeded, ithParseConceptSets))
        )
        rubricResponse <- cleaned$item
        if (length(cleaned$repairs) > 0L) {
          message("Repaired concept-set name(s) in template ", templateUp, ": ", paste(cleaned$repairs, collapse = "; "))
        }
        responseProblems <- ithRubricResponseProblems(rubricResponse)
        if (length(cleaned$problems) > 0L) {
          responseProblems <- c(responseProblems, paste0("invalid concept-set names: ", paste(cleaned$problems, collapse = "; ")))
        }
        if (length(responseProblems) == 0) {
          break
        }
        message(
          "Rubric response for template ", templateUp, " is incomplete (",
          paste(responseProblems, collapse = "; "), ")."
        )
      }
      if (length(responseProblems) > 0) {
        stop(
          "The LLM returned an incomplete rubric for '", condition$testCondition, "' (template ", templateUp,
          ") on all ", rubricResponseAttempts, " attempt(s): ", paste(responseProblems, collapse = "; "),
          ". Nothing was saved for this template; re-run to try again.",
          call. = FALSE
        )
      }
      rubricList[[templateUp]] <- rubricResponse
      ithMaybeMessage(verbose, "Received rubric response for template ", templateUp, ".")

      neededCurrent <- ithCollectNeededConcepts(rubricList[[templateUp]], templateUp)
      needed <- rbind(needed, neededCurrent)
      ithMaybeMessage(verbose, "Collected ", nrow(neededCurrent), " needed concept row(s) from template ", templateUp, ".")

      checkpoint <- list(
        status = "collectingTemplates",
        updatedAt = as.character(Sys.time()),
        rubricList = rubricList,
        neededConcepts = needed,
        shortRubric = shortRubricSeed,
        conceptSetHintsApplied = conceptSetHintsApplied
      )
      ithSaveRubricCheckpoint(conditionDir, condition$testCondition, checkpoint, verbose = verbose)
    }

    if (isTRUE(useLibraryHints) && !isTRUE(conceptSetHintsApplied)) {
      ithMaybeMessage(verbose, "Running concept set candidate hint pass for condition: ", condition$testCondition)
      hintPass <- ithRunRubricConceptSetHintPass(
        rubricList = rubricList,
        neededConcepts = needed,
        conditionName = condition$testCondition,
        outputDirectory = outputDirectory,
        conditionDir = conditionDir,
        llmClient = llmClient,
        similarityThreshold = libraryHintSimilarityThreshold,
        topCandidates = libraryHintTopCandidates,
        verbose = verbose
      )
      rubricList <- hintPass$rubricList
      needed <- hintPass$neededConcepts
      conceptSetHintsApplied <- TRUE

      checkpoint <- list(
        status = "appliedConceptSetHints",
        updatedAt = as.character(Sys.time()),
        rubricList = rubricList,
        neededConcepts = needed,
        shortRubric = shortRubricSeed,
        conceptSetHintsApplied = conceptSetHintsApplied,
        artifacts = list(
          conceptSetCandidateHintsPath = hintPass$hintPath,
          rubricConceptSetDecisionsPath = hintPass$decisionPath
        )
      )
      ithSaveRubricCheckpoint(conditionDir, condition$testCondition, checkpoint, verbose = verbose)
    }

    ithMaybeMessage(verbose, "Generating short rubric table and SQL statements for condition: ", condition$testCondition)

    checkpointUpdater <- function(currentShortRubric) {
      shortRubricSeed <<- currentShortRubric
      checkpoint <<- list(
        status = "buildingShortRubric",
        updatedAt = as.character(Sys.time()),
        rubricList = rubricList,
        neededConcepts = needed,
        shortRubric = shortRubricSeed,
        conceptSetHintsApplied = conceptSetHintsApplied
      )
      ithSaveRubricCheckpoint(conditionDir, condition$testCondition, checkpoint, verbose = verbose)
      invisible(NULL)
    }

    shortRubric <- ithCreateShortRubric(
      rubricList = rubricList,
      llmClient = llmClient,
      promptDir = promptDir,
      verbose = verbose,
      sqlLlmClientFactory = function() ithGetLlmClientFromConfig(list(), "reasoning"),
      existingShortRubric = shortRubricSeed,
      checkpointCallback = checkpointUpdater
    )

    if (!is.data.frame(shortRubric) || nrow(shortRubric) == 0) {
      stop(
        "Short rubric for '", condition$testCondition, "' has no rules; no rubric artifacts were saved.",
        call. = FALSE
      )
    }

    synced <- ithSyncSqlConceptSetsToNeeded(shortRubric, rubricList)
    if (nrow(synced$mismatches) > 0L) {
      shortRubric <- synced$shortRubric
      rubricList <- synced$rubricList
      needed <- ithCollectNeededConceptsFromRubricList(rubricList)
      message(
        "Added SQL concept sets missing from neededConceptSets for '", condition$testCondition, "': ",
        paste0(synced$mismatches$rubric, " ", synced$mismatches$ruleId, " (", synced$mismatches$missingConceptSets, ")", collapse = "; ")
      )
    }

    checkpoint <- list(
      status = "shortRubricBuilt",
      updatedAt = as.character(Sys.time()),
      rubricList = rubricList,
      neededConcepts = needed,
      shortRubric = shortRubric,
      conceptSetHintsApplied = conceptSetHintsApplied
    )
    ithSaveRubricCheckpoint(conditionDir, condition$testCondition, checkpoint, verbose = verbose)

    rubricListPath <- ithSaveVersioned(
      saveDirectory = conditionDir,
      objectToSave = list(rubricList = rubricList, neededConcepts = needed),
      fileName = "rubricList.rds"
    )
    ithMaybeMessage(verbose, "Saved rubric list artifact: ", rubricListPath)

    shortRubricPath <- ithSaveVersioned(
      saveDirectory = conditionDir,
      objectToSave = list(shortRubric = shortRubric),
      fileName = paste0(condition$testCondition, "_shortRubric.rds")
    )
    ithMaybeMessage(verbose, "Saved short rubric artifact: ", shortRubricPath)

    conciseRubricPath <- ithSaveVersioned(
      saveDirectory = conditionDir,
      objectToSave = shortRubric[, c(1:12)],
      fileName = paste0(condition$testCondition, "_conciseRubric.csv")
    )
    ithMaybeMessage(verbose, "Saved concise rubric artifact: ", conciseRubricPath)

    ithRunYesCombinationReport(condition$testCondition, outputDirectory, verbose = verbose)

    if (saveSummary) {
      ithMaybeMessage(verbose, "Generating concise rubric summary for condition: ", condition$testCondition)
      summaryTemplate <- ithReadPrompt("summarizeConciseRubric.txt", promptDir = promptDir)
      exampleText <- ithReadPrompt("exampleSummaryReport.txt", promptDir = promptDir)

      summaryPrompt <- gsub("<CONCISE_RUBRIC>", jsonlite::toJSON(shortRubric[, c(1:11)]), summaryTemplate, fixed = TRUE)
      summaryPrompt <- gsub("<EXAMPLE_TEXT_RULE>", "", summaryPrompt, fixed = TRUE)
      summaryPrompt <- gsub("<EXAMPLE_TEXT>", exampleText, summaryPrompt, fixed = TRUE)
      summaryText <- ithQueryLlm(llmClient, summaryPrompt, output = "text", silent = !isTRUE(verbose))

      summaryPath <- ithSaveVersioned(
        saveDirectory = conditionDir,
        objectToSave = summaryText,
        fileName = paste0(condition$testCondition, "_RuleSummary.txt")
      )
      ithMaybeMessage(verbose, "Saved rubric summary artifact: ", summaryPath)
    }

    checkpoint <- list(
      status = "completed",
      updatedAt = as.character(Sys.time()),
      rubricList = rubricList,
      neededConcepts = needed,
      shortRubric = shortRubric,
      conceptSetHintsApplied = conceptSetHintsApplied,
      artifacts = list(
        rubricListPath = rubricListPath,
        shortRubricPath = shortRubricPath,
        conciseRubricPath = conciseRubricPath
      )
    )
    ithSaveRubricCheckpoint(conditionDir, condition$testCondition, checkpoint, verbose = verbose)

    ithMaybeMessage(verbose, "Completed rubric artifacts for condition: ", condition$testCondition)
  }

  ithMaybeMessage(verbose, "Rubric creation completed.")
}

ithLoadLatestShortRubric <- function(conditionName, outputDirectory) {
  conditionDir <- file.path(outputDirectory, conditionName)
  fileName <- paste0(conditionName, "_shortRubric.rds")
  latest <- ithLoadLatest(conditionDir, fileName)
  if (is.null(latest)) {
    stop("No short rubric found for condition: ", conditionName)
  }
  latest$shortRubric
}

ithValidateEditedConciseRubric <- function(editedRubric, conditionName = NULL) {
  requiredColumns <- c(
    "condition", "rubric", "rubricName", "rubricYesScore", "rubricNoScore",
    "ruleId", "type", "score", "criteria", "rationale", "neededConceptSets"
  )

  missingColumns <- setdiff(requiredColumns, names(editedRubric))
  if (length(missingColumns) > 0) {
    stop("Edited concise rubric is missing required columns: ", paste(missingColumns, collapse = ", "))
  }

  keyColumns <- c("rubric", "ruleId", "type")
  for (colName in keyColumns) {
    bad <- is.na(editedRubric[[colName]]) | !nzchar(trimws(as.character(editedRubric[[colName]])))
    if (any(bad)) {
      stop("Edited concise rubric has empty values in required key column: ", colName)
    }
  }

  normalizedConditionName <- tolower(trimws(as.character(
    if (is.null(conditionName) || length(conditionName) == 0L) "" else conditionName[[1]]
  )))
  nonEmptyRequired <- c("criteria", "neededConceptSets", "rubricName")
  if (identical(normalizedConditionName, "multiple myeloma")) {
    nonEmptyRequired <- setdiff(nonEmptyRequired, "neededConceptSets")
  }
  for (colName in nonEmptyRequired) {
    bad <- is.na(editedRubric[[colName]]) | !nzchar(trimws(as.character(editedRubric[[colName]])))
    if (any(bad)) {
      stop("Edited concise rubric has empty values in required column: ", colName)
    }
  }

  duplicatedKey <- duplicated(paste(editedRubric$rubric, editedRubric$ruleId, editedRubric$type, sep = "||"))
  if (any(duplicatedKey)) {
    dupRows <- which(duplicatedKey)
    stop("Edited concise rubric has duplicate rubric/ruleId/type rows. Duplicate row indices: ", paste(dupRows, collapse = ", "))
  }

  invisible(TRUE)
}

# SQL is not compared because SQL edits in the concise rubric are ignored by apply.
ithDiffConciseRubric <- function(conciseRubric, shortRubric) {
  missingColumns <- setdiff(c("rubric", "ruleId", "type"), names(conciseRubric))
  if (length(missingColumns) > 0) {
    stop("Concise rubric is missing required columns: ", paste(missingColumns, collapse = ", "))
  }

  normalizeText <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(gsub("\r\n?", "\n", x))
  }
  keyOf <- function(x) paste(normalizeText(x$rubric), normalizeText(x$ruleId), normalizeText(x$type), sep = "||")

  conciseKey <- keyOf(conciseRubric)
  shortKey <- keyOf(shortRubric)
  common <- intersect(conciseKey, shortKey)
  edited <- conciseRubric[match(common, conciseKey), , drop = FALSE]
  existing <- shortRubric[match(common, shortKey), , drop = FALSE]
  bothHave <- function(column) column %in% names(edited) && column %in% names(existing)

  textChanged <- function(column) {
    bothHave(column) && any(normalizeText(edited[[column]]) != normalizeText(existing[[column]]))
  }
  numberChanged <- function(column) {
    if (!bothHave(column)) {
      return(FALSE)
    }
    a <- suppressWarnings(as.numeric(edited[[column]]))
    b <- suppressWarnings(as.numeric(existing[[column]]))
    any(is.na(a) != is.na(b) | (!is.na(a) & !is.na(b) & abs(a - b) > 1e-9))
  }
  flagChanged <- function(column) {
    if (!bothHave(column)) {
      return(FALSE)
    }
    a <- as.logical(edited[[column]])
    b <- as.logical(existing[[column]])
    any(is.na(a) != is.na(b) | (!is.na(a) & !is.na(b) & a != b))
  }

  changes <- list(
    rulesChanged = length(setdiff(conciseKey, shortKey)) > 0 || length(setdiff(shortKey, conciseKey)) > 0,
    criteriaChanged = textChanged("criteria"),
    conceptSetsChanged = textChanged("neededConceptSets"),
    scoresChanged = numberChanged("score"),
    thresholdsChanged = numberChanged("rubricYesScore") || numberChanged("rubricNoScore"),
    otherChanged = textChanged("rationale") || textChanged("rubricName") || textChanged("condition") || flagChanged("okToUse")
  )
  changes$differs <- any(unlist(changes))
  changes
}

ithBuildNeededConceptsFromShortRubric <- function(shortRubric) {
  if (is.null(shortRubric) || nrow(shortRubric) == 0) {
    return(data.frame(
      type = character(),
      rubric = character(),
      rule = integer(),
      criteria = character(),
      conceptSetsNeeded = character(),
      stringsAsFactors = FALSE
    ))
  }

  typeMap <- c(
    hard_exclusion = "hardExclusions",
    alternativeExplanations = "alternativeExplanations",
    evidenceRules = "evidenceRules"
  )

  out <- data.frame(
    type = ifelse(shortRubric$type %in% names(typeMap), typeMap[shortRubric$type], shortRubric$type),
    rubric = shortRubric$rubric,
    rule = seq_len(nrow(shortRubric)),
    criteria = shortRubric$criteria,
    conceptSetsNeeded = shortRubric$neededConceptSets,
    stringsAsFactors = FALSE
  )

  out
}

ithInferConceptSetsFromCriteria <- function(criteria,
                                            currentConceptSets,
                                            llmClient,
                                            promptDir = NULL) {
  currentText <- if (is.null(currentConceptSets) || !nzchar(as.character(currentConceptSets[[1]]))) {
    "(none)"
  } else {
    as.character(currentConceptSets[[1]])
  }
  prompt <- paste0(
    "Extract the concept-set names needed to implement this single clinical rule.\n",
    "Return only a semicolon-separated list of explicit concept-set names; return NONE if no concept set is needed.\n",
    "Use full words, underscores, and exactly one controlled suffix per name: DIAGNOSIS_CODES, PROCEDURE_CODES, MEDICATION_CODES, VISIT_CODES, MEASUREMENT_CODES, DEVICE_CODES, or PROVIDER_CODES.\n",
    "Do not use abbreviations, INFUSION_CODES, or mixed type names.\n",
    "Rule criteria:\n", as.character(criteria[[1]]), "\n",
    "Current concept sets for context:\n", currentText
  )
  typeObject <- ellmer::type_object(
    conceptSetsNeeded = ellmer::type_string()
  )
  result <- ithQueryLlm(
    llmClient = llmClient,
    prompt = prompt,
    output = "list",
    ellmerTypeObject = typeObject,
    silent = TRUE
  )
  value <- result$conceptSetsNeeded
  if (is.null(value) || length(value) == 0L) {
    stop("LLM did not return conceptSetsNeeded")
  }
  value <- trimws(as.character(value[[1]]))
  if (toupper(value) == "NONE" || !nzchar(value)) {
    return("")
  }
  parsed <- unlist(strsplit(value, "[;,]", perl = TRUE), use.names = FALSE)
  parsed <- unique(trimws(parsed[nzchar(trimws(parsed))]))
  if (length(parsed) == 0L) {
    stop("LLM returned no usable concept-set names")
  }
  paste(parsed, collapse = ";")
}

ithFindConceptSetTypeMismatches <- function(criteria, conceptSets) {
  criteriaText <- tolower(as.character(criteria[[1]]))
  names <- unlist(strsplit(as.character(conceptSets[[1]]), "[;,]", perl = TRUE), use.names = FALSE)
  names <- trimws(names[nzchar(trimws(names))])
  if (length(names) == 0L) {
    return(character())
  }

  hasDiagnosis <- grepl("diagnos", criteriaText)
  hasProcedure <- grepl("procedur", criteriaText)
  mismatches <- character()
  if (hasDiagnosis && !hasProcedure) {
    mismatches <- c(mismatches, names[grepl("_PROCEDURE_CODES$", toupper(names))])
  }
  if (hasProcedure && !hasDiagnosis) {
    mismatches <- c(mismatches, names[grepl("_DIAGNOSIS_CODES$", toupper(names))])
  }
  unique(mismatches)
}

ithControlledConceptSetSuffixes <- c(
  "DIAGNOSIS_CODES", "PROCEDURE_CODES", "MEDICATION_CODES", "VISIT_CODES",
  "MEASUREMENT_CODES", "DEVICE_CODES", "PROVIDER_CODES"
)

# Returns "" when the concept-set name is usable, otherwise the reason it is not.
ithConceptSetNameProblem <- function(name, requireControlledSuffix = TRUE) {
  if (!grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)*$", name)) {
    return("is not a single underscore-separated name (letters, digits, underscores only)")
  }
  suffixPattern <- paste0("_(", paste(ithControlledConceptSetSuffixes, collapse = "|"), ")$")
  if (isTRUE(requireControlledSuffix) && !grepl(suffixPattern, toupper(name))) {
    return(paste0("does not end in a controlled suffix (", paste(ithControlledConceptSetSuffixes, collapse = ", "), ")"))
  }
  ""
}

# Checks one rule's concept-set list. A malformed name such as "p pulmonology_PROVIDER_CODES" is
# repaired when exactly one well-formed name in knownNames (the rule's SQL, the rest of the rubric)
# appears inside it. legacyNames are exempt from the controlled-suffix rule so rubrics created
# before that rule can still be edited. The list text is only rewritten when something is repaired.
ithCheckRuleConceptSets <- function(conceptSets,
                                    knownNames = character(),
                                    legacyNames = character(),
                                    requireControlledSuffix = TRUE) {
  value <- if (is.null(conceptSets) || length(conceptSets) == 0L || is.na(conceptSets[[1]])) "" else as.character(conceptSets[[1]])
  names <- ithParseConceptSets(value)
  originalNames <- names
  names <- vapply(names, ithCollapseDuplicateConceptSetTokens, character(1))
  knownNames <- unique(as.character(knownNames))
  knownValid <- knownNames[vapply(knownNames, function(x) !nzchar(ithConceptSetNameProblem(x, FALSE)), logical(1))]
  legacyKeys <- tolower(legacyNames)
  out <- names
  repairs <- if (any(names != originalNames)) {
    paste0("collapsed duplicate adjacent token(s): ", paste(originalNames[names != originalNames], collapse = ", "))
  } else {
    character()
  }
  problems <- character()

  for (i in seq_along(names)) {
    nm <- names[[i]]
    problem <- ithConceptSetNameProblem(nm, requireControlledSuffix && !(tolower(nm) %in% legacyKeys))
    if (!nzchar(problem)) {
      next
    }
    if (!nzchar(ithConceptSetNameProblem(nm, FALSE))) {
      problems <- c(problems, paste0("'", nm, "' ", problem))
      next
    }
    tokens <- tolower(unlist(strsplit(nm, "[^A-Za-z0-9_]+")))
    tokens <- c(tokens[nzchar(tokens)], tolower(gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", nm))))
    candidates <- knownValid[tolower(knownValid) %in% tokens]
    candidates <- candidates[!duplicated(tolower(candidates))]
    if (length(candidates) == 1L) {
      out[[i]] <- candidates
      repairs <- c(repairs, paste0("'", nm, "' -> '", candidates, "'"))
    } else {
      problems <- c(problems, paste0(
        "'", nm, "' ", problem,
        if (length(candidates) > 1L) paste0(" (could be ", paste(candidates, collapse = " or "), ")") else ""
      ))
    }
  }

  if (length(repairs) > 0L) {
    value <- paste(out[!duplicated(tolower(out))], collapse = if (grepl(";", value, fixed = TRUE)) ";" else ",")
  }
  list(value = value, repairs = repairs, problems = problems)
}

# Applies ithCheckRuleConceptSets to every rule in one LLM rubric response.
ithCleanRubricResponseConceptSets <- function(rubricItem, knownNames = character()) {
  sections <- c("hardExclusions", "alternativeExplanations", "evidenceRules")
  if (!is.list(rubricItem)) {
    return(list(item = rubricItem, repairs = character(), problems = character()))
  }
  hasConceptSets <- function(df) is.data.frame(df) && "conceptSets_needed" %in% names(df)
  for (section in sections) {
    if (hasConceptSets(rubricItem[[section]])) {
      knownNames <- c(knownNames, unlist(lapply(rubricItem[[section]]$conceptSets_needed, ithParseConceptSets)))
    }
  }

  repairs <- character()
  problems <- character()
  for (section in sections) {
    df <- rubricItem[[section]]
    if (!hasConceptSets(df)) {
      next
    }
    for (rowUp in seq_len(nrow(df))) {
      check <- ithCheckRuleConceptSets(df$conceptSets_needed[[rowUp]], knownNames)
      label <- paste0(section, " ", if ("ruleId" %in% names(df)) df$ruleId[[rowUp]] else rowUp)
      if (length(check$repairs) > 0L) {
        df$conceptSets_needed[[rowUp]] <- check$value
        repairs <- c(repairs, paste0(label, ": ", check$repairs))
      }
      if (length(check$problems) > 0L) {
        problems <- c(problems, paste0(label, ": ", check$problems))
      }
    }
    rubricItem[[section]] <- df
  }
  list(item = rubricItem, repairs = repairs, problems = problems)
}

# ruleIds are owned by the system: an edited CSV may keep, drop, or retype existing rules but
# cannot rename them. Blank ruleIds (new rules) and rows whose type changed get the next ID for
# their type, above every ID used in any saved concise rubric version, so removed IDs are never
# reused.
ithAssignConciseRuleIds <- function(editedConcise, existingShortRubric, conditionDir, conditionName, verbose = TRUE) {
  ruleIds <- toupper(trimws(as.character(editedConcise$ruleId)))
  ruleIds[is.na(ruleIds)] <- ""
  rubrics <- trimws(as.character(editedConcise$rubric))

  types <- vapply(as.character(editedConcise$type), ithNormalizeReviewRuleType, character(1), USE.NAMES = FALSE)
  badType <- is.na(types) | !(types %in% names(ithReviewTypePrefixes))
  if (any(badType)) {
    stop(
      "Edited concise rubric has unrecognized type(s) on row(s) ", paste(which(badType), collapse = ", "),
      ": ", paste(unique(editedConcise$type[badType]), collapse = ", "),
      ". Use evidenceRules, alternativeExplanations, or hard_exclusion.",
      call. = FALSE
    )
  }
  editedConcise$type <- types

  existingKey <- paste(trimws(existingShortRubric$rubric), toupper(trimws(existingShortRubric$ruleId)), sep = "||")
  existingType <- setNames(as.character(existingShortRubric$type), existingKey)
  editedKey <- paste(rubrics, ruleIds, sep = "||")

  unknown <- nzchar(ruleIds) & !(editedKey %in% existingKey)
  if (any(unknown)) {
    stop(
      "Edited concise rubric has ruleId(s) that are not in the current rubric: ",
      paste(unique(paste(rubrics[unknown], ruleIds[unknown])), collapse = ", "),
      ". Rule IDs are assigned automatically: leave ruleId blank for a new rule, and do not rename existing rules.",
      call. = FALSE
    )
  }
  dupRows <- nzchar(ruleIds) & duplicated(editedKey)
  if (any(dupRows)) {
    stop(
      "Edited concise rubric lists the same rule more than once: ",
      paste(unique(paste(rubrics[dupRows], ruleIds[dupRows])), collapse = ", "),
      ". Leave ruleId blank on copied rows to add them as new rules.",
      call. = FALSE
    )
  }

  needsId <- !nzchar(ruleIds) | (nzchar(ruleIds) & types != unname(existingType[editedKey]))
  if (!any(needsId)) {
    editedConcise$ruleId <- ruleIds
    return(editedConcise)
  }

  # Every ruleId ever saved for this condition, so a removed rule's ID is not handed out again.
  escapedName <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", conditionName)
  historyFiles <- list.files(
    conditionDir,
    pattern = paste0("^", escapedName, "_conciseRubric_V[0-9]+\\.csv$"),
    ignore.case = TRUE,
    full.names = TRUE
  )
  history <- do.call(rbind, c(
    list(data.frame(rubric = trimws(existingShortRubric$rubric), ruleId = toupper(trimws(existingShortRubric$ruleId)), stringsAsFactors = FALSE)),
    list(data.frame(rubric = rubrics, ruleId = ruleIds, stringsAsFactors = FALSE)),
    lapply(historyFiles, function(path) {
      old <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"), error = function(e) NULL)
      if (is.null(old) || !all(c("rubric", "ruleId") %in% names(old))) {
        return(NULL)
      }
      data.frame(rubric = trimws(old$rubric), ruleId = toupper(trimws(old$ruleId)), stringsAsFactors = FALSE)
    })
  ))

  nextId <- function(rubric, type) {
    prefix <- ithReviewTypePrefixes[[type]]
    ids <- history$ruleId[history$rubric == rubric]
    numbers <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", ids[grepl(paste0("^", prefix, "[0-9]+$"), ids)])))
    newId <- paste0(prefix, if (length(numbers) == 0L) 1L else max(numbers) + 1L)
    history <<- rbind(history, data.frame(rubric = rubric, ruleId = newId, stringsAsFactors = FALSE))
    newId
  }

  for (row in which(needsId)) {
    newId <- nextId(rubrics[[row]], types[[row]])
    if (nzchar(ruleIds[[row]])) {
      ithMaybeMessage(verbose, "Renumbered ", rubrics[[row]], " ", ruleIds[[row]], " -> ", newId, " (type changed to ", types[[row]], ")")
    } else {
      ithMaybeMessage(verbose, "Assigned ", rubrics[[row]], " ", newId, " to new rule: ", editedConcise$criteria[[row]])
    }
    ruleIds[[row]] <- newId
  }
  editedConcise$ruleId <- ruleIds
  editedConcise
}

# Long-format list of what changed between two short rubrics: one row per changed field of a
# rule (text, score, type, concept sets, SQL), plus added/deleted rules and yes/no thresholds.
ithBuildRubricDelta <- function(oldRubric, newRubric, conditionName) {
  fields <- c("type", "score", "criteria", "rationale", "neededConceptSets", "sqlStatement")
  text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  }
  oldKey <- paste(text(oldRubric$rubric), toupper(text(oldRubric$ruleId)), sep = "||")
  newKey <- paste(text(newRubric$rubric), toupper(text(newRubric$ruleId)), sep = "||")
  rows <- list()
  push <- function(rubric, ruleId, previousRuleId, changeType, field, oldValue, newValue) {
    rows[[length(rows) + 1L]] <<- data.frame(
      condition = conditionName, rubric = rubric, ruleId = ruleId, previousRuleId = previousRuleId,
      changeType = changeType, field = field, oldValue = oldValue, newValue = newValue,
      stringsAsFactors = FALSE
    )
  }
  pushChangedFields <- function(j, i, changeType, previousRuleId) {
    for (field in fields) {
      oldValue <- text(oldRubric[[field]][[j]])
      newValue <- text(newRubric[[field]][[i]])
      if (!identical(oldValue, newValue)) {
        push(text(newRubric$rubric[[i]]), text(newRubric$ruleId[[i]]), previousRuleId, changeType, field, oldValue, newValue)
      }
    }
  }

  pairedOld <- integer()
  for (i in seq_len(nrow(newRubric))) {
    j <- match(newKey[[i]], oldKey)
    if (!is.na(j)) {
      pairedOld <- c(pairedOld, j)
      pushChangedFields(j, i, "modified", "")
      next
    }
    # A type change gives the rule a new ID; match it back to the old rule by identical criteria.
    candidates <- which(
      !(seq_len(nrow(oldRubric)) %in% pairedOld) & !(oldKey %in% newKey) &
        text(oldRubric$rubric) == text(newRubric$rubric[[i]]) &
        text(oldRubric$criteria) == text(newRubric$criteria[[i]])
    )
    if (length(candidates) > 0L) {
      j <- candidates[[1]]
      pairedOld <- c(pairedOld, j)
      previousRuleId <- text(oldRubric$ruleId[[j]])
      push(text(newRubric$rubric[[i]]), text(newRubric$ruleId[[i]]), previousRuleId, "retyped", "ruleId", previousRuleId, text(newRubric$ruleId[[i]]))
      pushChangedFields(j, i, "retyped", previousRuleId)
      next
    }
    for (field in fields) {
      newValue <- text(newRubric[[field]][[i]])
      if (nzchar(newValue)) {
        push(text(newRubric$rubric[[i]]), text(newRubric$ruleId[[i]]), "", "added", field, "", newValue)
      }
    }
  }
  for (j in setdiff(which(!(oldKey %in% newKey)), pairedOld)) {
    for (field in fields) {
      oldValue <- text(oldRubric[[field]][[j]])
      if (nzchar(oldValue)) {
        push(text(oldRubric$rubric[[j]]), text(oldRubric$ruleId[[j]]), "", "deleted", field, oldValue, "")
      }
    }
  }
  for (rubricId in unique(c(text(oldRubric$rubric), text(newRubric$rubric)))) {
    oldRow <- which(text(oldRubric$rubric) == rubricId)[1]
    newRow <- which(text(newRubric$rubric) == rubricId)[1]
    if (is.na(oldRow) || is.na(newRow)) {
      next
    }
    for (field in c("rubricYesScore", "rubricNoScore")) {
      oldValue <- text(oldRubric[[field]][[oldRow]])
      newValue <- text(newRubric[[field]][[newRow]])
      if (!identical(oldValue, newValue)) {
        push(rubricId, "(rubric)", "", "modified", field, oldValue, newValue)
      }
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      condition = character(), rubric = character(), ruleId = character(), previousRuleId = character(),
      changeType = character(), field = character(), oldValue = character(), newValue = character(),
      stringsAsFactors = FALSE
    ))
  }
  delta <- do.call(rbind, rows)
  delta[order(delta$rubric, ithRuleIdOrder(delta$ruleId), match(delta$field, c("ruleId", fields))), , drop = FALSE]
}

ithApplyConciseRubricEdits <- function(conditionName,
                                          outputDirectory,
                                          editedConciseRubricPath = NULL,
                                          llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                                          promptDir = NULL,
                                          updateSummary = TRUE,
                                          verbose = TRUE,
                                          refreshConceptSets = TRUE,
                                          adjustableRuleId = NULL,
                                          repairConceptSetMismatches = TRUE) {
  conditionDir <- file.path(outputDirectory, conditionName)
  if (!dir.exists(conditionDir)) {
    stop("Condition directory not found: ", conditionDir)
  }

  existingShortRubric <- ithLoadLatestShortRubric(conditionName, outputDirectory)

  if (is.null(editedConciseRubricPath)) {
    editedConcise <- ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
    if (is.null(editedConcise)) {
      stop("No concise rubric found for condition: ", conditionName)
    }
    ithMaybeMessage(verbose, "Loaded latest concise rubric for edits from condition directory.")
  } else {
    if (!file.exists(editedConciseRubricPath)) {
      stop("Edited concise rubric file not found: ", editedConciseRubricPath)
    }
    editedConcise <- utils::read.csv(editedConciseRubricPath, stringsAsFactors = FALSE)
    ithMaybeMessage(verbose, "Loaded edited concise rubric file: ", editedConciseRubricPath)
  }

  if (!("okToUse" %in% names(editedConcise))) {
    editedConcise$okToUse <- TRUE
  }

  # Rules in adjustableRuleId are re-scored at report time, so edits must keep them as evidence rules.
  for (protectedId in ithParseRuleIdList(adjustableRuleId, rowLabel = conditionName)) {
    protectedRows <- toupper(trimws(as.character(editedConcise$ruleId))) == protectedId
    if (!any(protectedRows)) {
      stop(
        "The edited concise rubric no longer has ", protectedId, ", which is listed in adjustableRuleId ",
        "for '", conditionName, "'. Keep the rule or update adjustableRuleId in conditionSpecs first.",
        call. = FALSE
      )
    }
    protectedTypes <- vapply(as.character(editedConcise$type[protectedRows]), ithNormalizeReviewRuleType, character(1))
    if (any(is.na(protectedTypes) | protectedTypes != "evidenceRules")) {
      stop(
        protectedId, " is listed in adjustableRuleId for '", conditionName, "' and must stay an evidence rule. ",
        "Keep its type or update adjustableRuleId in conditionSpecs first.",
        call. = FALSE
      )
    }
    for (row in which(protectedRows)) {
      oldRows <- which(
        trimws(existingShortRubric$rubric) == trimws(editedConcise$rubric[[row]]) &
          toupper(trimws(existingShortRubric$ruleId)) == protectedId
      )
      if (length(oldRows) > 0L && !identical(
        trimws(as.character(existingShortRubric$criteria[[oldRows[[1]]]])),
        trimws(as.character(editedConcise$criteria[[row]]))
      )) {
        warning(
          "Criteria changed for ", protectedId, ", which is listed in adjustableRuleId for '", conditionName,
          "'. Check that dxRuleScoreOverride/dxRuleScoreAdjustment should still apply to it.\n  Old: ",
          existingShortRubric$criteria[[oldRows[[1]]]], "\n  New: ", editedConcise$criteria[[row]],
          call. = FALSE, immediate. = TRUE
        )
      }
    }
  }

  editedConcise <- ithAssignConciseRuleIds(editedConcise, existingShortRubric, conditionDir, conditionName, verbose)

  ithValidateEditedConciseRubric(editedConcise, conditionName = conditionName)
  existingKey <- paste(existingShortRubric$rubric, existingShortRubric$ruleId, existingShortRubric$type, sep = "||")
  existingIndex <- setNames(seq_len(nrow(existingShortRubric)), existingKey)

  editedKey <- paste(editedConcise$rubric, editedConcise$ruleId, editedConcise$type, sep = "||")

  # Names already in the current rubric keep working even without a controlled suffix (older rubrics).
  existingConceptSetNames <- unique(unlist(lapply(existingShortRubric$neededConceptSets, ithParseConceptSets)))
  rubricConceptSetNames <- unique(c(
    existingConceptSetNames,
    unlist(lapply(editedConcise$neededConceptSets, ithParseConceptSets)),
    ithScoringSqlConceptSets(existingShortRubric$sqlStatement)
  ))
  checkConceptSets <- function(value, rowUp, source) {
    oldIdx <- match(editedKey[[rowUp]], existingKey)
    sqlNames <- if (is.na(oldIdx)) character() else ithScoringSqlConceptSets(existingShortRubric$sqlStatement[[oldIdx]])
    check <- ithCheckRuleConceptSets(
      value,
      knownNames = c(sqlNames, rubricConceptSetNames),
      legacyNames = existingConceptSetNames
    )
    label <- paste(editedConcise$rubric[[rowUp]], editedConcise$ruleId[[rowUp]])
    if (length(check$repairs) > 0L) {
      message("Repaired ", source, " concept-set name(s) for ", label, ": ", paste(check$repairs, collapse = "; "))
    }
    check$problems <- if (length(check$problems) > 0L) paste0(label, ": ", check$problems) else character()
    check
  }
  conceptSetProblems <- character()
  for (rowUp in seq_len(nrow(editedConcise))) {
    check <- checkConceptSets(editedConcise$neededConceptSets[[rowUp]], rowUp, "edited")
    editedConcise$neededConceptSets[[rowUp]] <- check$value
    conceptSetProblems <- c(conceptSetProblems, check$problems)
  }
  if (length(conceptSetProblems) > 0L) {
    stop(
      "Invalid concept-set names in the edited concise rubric for '", conditionName, "':\n  ",
      paste(conceptSetProblems, collapse = "\n  "),
      "\nList names separated by commas; each name uses letters, digits, and underscores and ends in one of: ",
      paste(ithControlledConceptSetSuffixes, collapse = ", "), ". Nothing was saved.",
      call. = FALSE
    )
  }
  checkInferred <- function(value, rowUp) {
    check <- checkConceptSets(value, rowUp, "inferred")
    if (length(check$problems) > 0L) {
      stop("Concept-set inference returned invalid names for ", paste(check$problems, collapse = "; "), call. = FALSE)
    }
    check$value
  }

  updatedRows <- list()
  nRegenerated <- 0L
  nReused <- 0L
  nNew <- 0L
  claimedOldRows <- integer(0)
  for (rowUp in seq_len(nrow(editedConcise))) {
    row <- editedConcise[rowUp, , drop = FALSE]
    key <- editedKey[[rowUp]]

    if (key %in% names(existingIndex)) {
      oldRow <- existingShortRubric[existingIndex[[key]], , drop = FALSE]

      criteriaChanged <- !identical(as.character(row$criteria[[1]]), as.character(oldRow$criteria[[1]]))
      conceptTypeMismatches <- ithFindConceptSetTypeMismatches(
        criteria = row$criteria,
        conceptSets = row$neededConceptSets
      )
      refreshConceptSetsNeeded <- criteriaChanged || (isTRUE(repairConceptSetMismatches) && length(conceptTypeMismatches) > 0L)
      if (refreshConceptSetsNeeded && isTRUE(refreshConceptSets)) {
        inferredConceptSets <- tryCatch(
          ithInferConceptSetsFromCriteria(
            criteria = row$criteria,
            currentConceptSets = oldRow$neededConceptSets,
            llmClient = llmClient,
            promptDir = promptDir
          ),
          error = function(e) {
            stop("Concept-set inference failed for ruleId=", row$ruleId[[1]], ": ", conditionMessage(e))
          }
        )
        inferredConceptSets <- checkInferred(inferredConceptSets, rowUp)
        inferredMismatches <- ithFindConceptSetTypeMismatches(
          criteria = row$criteria,
          conceptSets = inferredConceptSets
        )
        if (length(inferredMismatches) > 0L) {
          # The keyword check is a heuristic (e.g. "diagnosis ... after delivery" needs a procedure set).
          message(
            "Keeping concept set(s) for ruleId=", row$ruleId[[1]], " that the criteria wording suggests are the wrong type ",
            "(re-inference returned them again): ", paste(inferredMismatches, collapse = ", ")
          )
        }
        row$neededConceptSets[[1]] <- inferredConceptSets
        ithMaybeMessage(verbose, "Updated needed concept sets from edited criteria for ruleId=", row$ruleId[[1]], ": ", ifelse(nzchar(inferredConceptSets), inferredConceptSets, "(none)"))
      } else if (refreshConceptSetsNeeded && !isTRUE(refreshConceptSets)) {
        stop(
          "Concept-set types conflict with edited criteria for ruleId=", row$ruleId[[1]],
          ": ", paste(conceptTypeMismatches, collapse = ", "),
          ". Set refreshConceptSets = TRUE or update neededConceptSets manually."
        )
      }
      conceptChanged <- !identical(as.character(row$neededConceptSets[[1]]), as.character(oldRow$neededConceptSets[[1]]))

      sqlStatement <- oldRow$sqlStatement[[1]]
      if (criteriaChanged || conceptChanged) {
        ithMaybeMessage(verbose, "Regenerating SQL for edited row: ruleId=", row$ruleId[[1]], ", type=", row$type[[1]])
        sqlStatement <- ithCreateSqlStatement(
          criteria = row$criteria[[1]],
          conceptSets = row$neededConceptSets[[1]],
          llmClient = llmClient,
          promptDir = promptDir
        )
        nRegenerated <- nRegenerated + 1L
      } else {
        nReused <- nReused + 1L
      }

      updatedRows[[length(updatedRows) + 1L]] <- data.frame(
        condition = as.character(row$condition[[1]]),
        rubric = as.character(row$rubric[[1]]),
        rubricName = as.character(row$rubricName[[1]]),
        rubricYesScore = as.numeric(row$rubricYesScore[[1]]),
        rubricNoScore = as.numeric(row$rubricNoScore[[1]]),
        ruleId = as.character(row$ruleId[[1]]),
        type = as.character(row$type[[1]]),
        score = as.numeric(row$score[[1]]),
        criteria = as.character(row$criteria[[1]]),
        rationale = as.character(row$rationale[[1]]),
        neededConceptSets = as.character(row$neededConceptSets[[1]]),
        sqlStatement = sqlStatement,
        okToUse = isTRUE(as.logical(row$okToUse[[1]])),
        stringsAsFactors = FALSE
      )
    } else {
      # A retyped rule (new ID, same criteria and concept sets) keeps its SQL, which does not depend on the type.
      retypedFrom <- setdiff(which(
        !(existingKey %in% editedKey) &
          trimws(existingShortRubric$rubric) == trimws(row$rubric[[1]]) &
          trimws(as.character(existingShortRubric$criteria)) == trimws(as.character(row$criteria[[1]])) &
          trimws(as.character(existingShortRubric$neededConceptSets)) == trimws(as.character(row$neededConceptSets[[1]]))
      ), claimedOldRows)
      if (length(retypedFrom) > 0L) {
        claimedOldRows <- c(claimedOldRows, retypedFrom[[1]])
        nReused <- nReused + 1L
        updatedRows[[length(updatedRows) + 1L]] <- data.frame(
          condition = as.character(row$condition[[1]]),
          rubric = as.character(row$rubric[[1]]),
          rubricName = as.character(row$rubricName[[1]]),
          rubricYesScore = as.numeric(row$rubricYesScore[[1]]),
          rubricNoScore = as.numeric(row$rubricNoScore[[1]]),
          ruleId = as.character(row$ruleId[[1]]),
          type = as.character(row$type[[1]]),
          score = as.numeric(row$score[[1]]),
          criteria = as.character(row$criteria[[1]]),
          rationale = as.character(row$rationale[[1]]),
          neededConceptSets = as.character(row$neededConceptSets[[1]]),
          sqlStatement = existingShortRubric$sqlStatement[[retypedFrom[[1]]]],
          okToUse = isTRUE(as.logical(row$okToUse[[1]])),
          stringsAsFactors = FALSE
        )
        next
      }
      newMismatches <- ithFindConceptSetTypeMismatches(
        criteria = row$criteria,
        conceptSets = row$neededConceptSets
      )
      if (length(newMismatches) > 0L) {
        if (!isTRUE(refreshConceptSets)) {
          stop(
            "Concept-set types conflict with criteria for new ruleId=", row$ruleId[[1]],
            ": ", paste(newMismatches, collapse = ", "),
            ". Set refreshConceptSets = TRUE or update neededConceptSets manually."
          )
        }
        inferredConceptSets <- tryCatch(
          ithInferConceptSetsFromCriteria(
            criteria = row$criteria,
            currentConceptSets = row$neededConceptSets,
            llmClient = llmClient,
            promptDir = promptDir
          ),
          error = function(e) {
            stop("Concept-set inference failed for ruleId=", row$ruleId[[1]], ": ", conditionMessage(e))
          }
        )
        inferredConceptSets <- checkInferred(inferredConceptSets, rowUp)
        stillMismatched <- ithFindConceptSetTypeMismatches(
          criteria = row$criteria,
          conceptSets = inferredConceptSets
        )
        if (length(stillMismatched) > 0L) {
          message(
            "Keeping concept set(s) for new ruleId=", row$ruleId[[1]], " that the criteria wording suggests are the wrong type ",
            "(re-inference returned them again): ", paste(stillMismatched, collapse = ", ")
          )
        }
        row$neededConceptSets[[1]] <- inferredConceptSets
        ithMaybeMessage(verbose, "Updated needed concept sets for new ruleId=", row$ruleId[[1]], ": ", ifelse(nzchar(inferredConceptSets), inferredConceptSets, "(none)"))
      }
      ithMaybeMessage(verbose, "Generating SQL for new row: ruleId=", row$ruleId[[1]], ", type=", row$type[[1]])
      sqlStatement <- ithCreateSqlStatement(
        criteria = row$criteria[[1]],
        conceptSets = row$neededConceptSets[[1]],
        llmClient = llmClient,
        promptDir = promptDir
      )
      nRegenerated <- nRegenerated + 1L
      nNew <- nNew + 1L

      updatedRows[[length(updatedRows) + 1L]] <- data.frame(
        condition = as.character(row$condition[[1]]),
        rubric = as.character(row$rubric[[1]]),
        rubricName = as.character(row$rubricName[[1]]),
        rubricYesScore = as.numeric(row$rubricYesScore[[1]]),
        rubricNoScore = as.numeric(row$rubricNoScore[[1]]),
        ruleId = as.character(row$ruleId[[1]]),
        type = as.character(row$type[[1]]),
        score = as.numeric(row$score[[1]]),
        criteria = as.character(row$criteria[[1]]),
        rationale = as.character(row$rationale[[1]]),
        neededConceptSets = as.character(row$neededConceptSets[[1]]),
        sqlStatement = sqlStatement,
        okToUse = isTRUE(as.logical(row$okToUse[[1]])),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(updatedRows) == 0) {
    stop("Edited concise rubric has zero rows; cannot save an empty rubric.")
  }

  updatedShortRubric <- do.call(rbind, updatedRows)

  concisePath <- ithSaveShortRubric(updatedShortRubric, conditionName, outputDirectory, yesCombinationReport = FALSE)

  rubricDelta <- ithBuildRubricDelta(existingShortRubric, updatedShortRubric, conditionName)
  if (nrow(rubricDelta) > 0L) {
    # Same version number as the concise rubric it describes.
    deltaPath <- sub("_conciseRubric_V", "_rubricDelta_V", concisePath, fixed = TRUE)
    ithSaveByExt(rubricDelta, deltaPath, overwrite = TRUE)
    ithMaybeMessage(verbose, "Saved rubric delta: ", deltaPath)
  }
  rubricObject <- ithLoadLatest(conditionDir, "rubricList.rds")
  if (!is.null(rubricObject)) {
    rubricObject$neededConcepts <- ithBuildNeededConceptsFromShortRubric(updatedShortRubric)
    rubricObject <- ithUpdateRubricListThresholds(rubricObject, updatedShortRubric)
    ithSaveVersioned(
      saveDirectory = conditionDir,
      objectToSave = rubricObject,
      fileName = "rubricList.rds"
    )
    ithMaybeMessage(verbose, "Updated rubricList neededConcepts and yes/no thresholds for condition: ", conditionName)
  }

  # Run after rubricList is saved so the report reads the current rule categories.
  ithRunYesCombinationReport(conditionName, outputDirectory, verbose = verbose)

  if (isTRUE(updateSummary)) {
    summaryTemplate <- ithReadPrompt("summarizeConciseRubric.txt", promptDir = promptDir)
    exampleText <- ithReadPrompt("exampleSummaryReport.txt", promptDir = promptDir)

    summaryPrompt <- gsub("<CONCISE_RUBRIC>", jsonlite::toJSON(updatedShortRubric[, c(1:11)]), summaryTemplate, fixed = TRUE)
    summaryPrompt <- gsub("<EXAMPLE_TEXT_RULE>", "", summaryPrompt, fixed = TRUE)
    summaryPrompt <- gsub("<EXAMPLE_TEXT>", exampleText, summaryPrompt, fixed = TRUE)
    summaryText <- ithQueryLlm(llmClient, summaryPrompt, output = "text", silent = !isTRUE(verbose))

    ithSaveVersioned(
      saveDirectory = conditionDir,
      objectToSave = summaryText,
      fileName = paste0(conditionName, "_RuleSummary.txt")
    )
    ithMaybeMessage(verbose, "Updated concise rubric summary text for condition: ", conditionName)
  }

  deletedCount <- sum(!(existingKey %in% editedKey))
  ithMaybeMessage(
    verbose,
    "Applied concise rubric edits for condition '", conditionName,
    "': rows=", nrow(updatedShortRubric),
    ", newRows=", nNew,
    ", deletedRows=", deletedCount,
    ", regeneratedSql=", nRegenerated,
    ", reusedSql=", nReused, "."
  )

  invisible(updatedShortRubric)
}

# Scoring reads yes/no thresholds from rubricList, not from the short rubric.
ithUpdateRubricListThresholds <- function(rubricObject, shortRubric) {
  thresholds <- unique(shortRubric[, c("rubric", "rubricYesScore", "rubricNoScore")])

  for (itemUp in seq_along(rubricObject$rubricList)) {
    item <- rubricObject$rubricList[[itemUp]]
    rubricId <- as.character(if (!is.null(item$rubricId)) item$rubricId[[1]] else item$rubric_id[[1]])
    rows <- thresholds[as.character(thresholds$rubric) == rubricId, , drop = FALSE]

    if (nrow(rows) > 1) {
      stop("Edited concise rubric has conflicting yes/no scores for rubric ", rubricId)
    }
    if (nrow(rows) == 0) {
      next
    }

    field <- if (is.null(item$decisionThresholds) && !is.null(item$decision_thresholds)) {
      "decision_thresholds"
    } else {
      "decisionThresholds"
    }
    item[[field]]$yes <- rows$rubricYesScore[[1]]
    item[[field]]$no <- rows$rubricNoScore[[1]]
    rubricObject$rubricList[[itemUp]] <- item
  }

  rubricObject
}

ithUpdateRubricRule <- function(shortRubric,
                                   ruleId,
                                   updates,
                                   regenerateSql = TRUE,
                                   llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
                                   promptDir = NULL,
                                   verbose = TRUE) {
  ithMaybeMessage(verbose, "Updating rubric rule: ", ruleId)
  rowUp <- which(shortRubric$ruleId == ruleId)
  if (length(rowUp) != 1) {
    stop("ruleId must match exactly one row: ", ruleId)
  }

  for (nameUp in names(updates)) {
    shortRubric[rowUp, nameUp] <- updates[[nameUp]]
  }

  if (regenerateSql && ("criteria" %in% names(updates) || "neededConceptSets" %in% names(updates))) {
    ithMaybeMessage(verbose, "Regenerating SQL for rule: ", ruleId)
    shortRubric$sqlStatement[[rowUp]] <- ithCreateSqlStatement(
      criteria = shortRubric$criteria[[rowUp]],
      conceptSets = shortRubric$neededConceptSets[[rowUp]],
      llmClient = llmClient,
      promptDir = promptDir
    )
  }

  ithMaybeMessage(verbose, "Rule update completed: ", ruleId)
  shortRubric
}

# Rewrites concept-set names to ithNormalizeConceptSetName form (e.g. bile_duct_procedure_procedure_codes
# -> bile_duct_procedure_codes) in neededConceptSets, SQL @references, and rubricList conceptSets_needed.
ithCleanRubricConceptSetNames <- function(shortRubric, rubricList = NULL) {
  renames <- character()
  cleanString <- function(conceptSetString) {
    original <- ithParseConceptSets(as.character(conceptSetString))
    cleaned <- ithNormalizeConceptSetName(original)
    changed <- original != cleaned
    if (!any(changed)) {
      return(conceptSetString)
    }
    renames[original[changed]] <<- cleaned[changed]
    paste(unique(cleaned[nzchar(cleaned)]), collapse = ", ")
  }

  for (i in seq_len(nrow(shortRubric))) {
    shortRubric$neededConceptSets[[i]] <- cleanString(shortRubric$neededConceptSets[[i]])
    sql <- as.character(shortRubric$sqlStatement[[i]])
    for (token in ithScoringSqlConceptSets(sql)) {
      cleaned <- ithNormalizeConceptSetName(token)
      if (identical(token, cleaned)) next
      renames[token] <- cleaned
      sql <- gsub(paste0("@", token, "\\b"), paste0("@", cleaned), sql, perl = TRUE)
    }
    shortRubric$sqlStatement[[i]] <- sql
  }

  for (itemUp in seq_along(rubricList)) {
    item <- rubricList[[itemUp]]
    if (is.null(item)) next
    for (tableName in c("hardExclusions", "alternativeExplanations", "evidenceRules")) {
      ruleTable <- item[[tableName]]
      if (!is.data.frame(ruleTable) || !("conceptSets_needed" %in% names(ruleTable))) next
      for (rowUp in seq_len(nrow(ruleTable))) {
        ruleTable$conceptSets_needed[[rowUp]] <- cleanString(ruleTable$conceptSets_needed[[rowUp]])
      }
      item[[tableName]] <- ruleTable
    }
    rubricList[[itemUp]] <- item
  }

  list(
    shortRubric = shortRubric,
    rubricList = rubricList,
    renames = data.frame(from = names(renames), to = unname(renames), stringsAsFactors = FALSE)
  )
}

# Appends concept sets referenced in each rule's SQL but missing from its neededConceptSets to the
# short rubric and, when supplied, to the matching rule's conceptSets_needed in rubricList.
ithSyncSqlConceptSetsToNeeded <- function(shortRubric, rubricList = NULL) {
  appendNames <- function(conceptSetString, newNames) {
    paste(unique(c(ithParseConceptSets(as.character(conceptSetString)), newNames)), collapse = ", ")
  }

  mismatches <- data.frame(
    rubric = character(),
    ruleId = character(),
    missingConceptSets = character(),
    neededConceptSets = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(shortRubric))) {
    sqlNames <- ithScoringSqlConceptSets(shortRubric$sqlStatement[[i]])
    neededString <- as.character(shortRubric$neededConceptSets[[i]])
    listed <- ithNormalizeConceptSetName(ithParseConceptSets(neededString))
    missing <- sqlNames[!(ithNormalizeConceptSetName(sqlNames) %in% listed)]
    if (length(missing) == 0L) next
    # SQL may carry malformed names (e.g. repeated tokens); store the cleaned form.
    missing <- unique(ithNormalizeConceptSetName(missing))

    rubricId <- as.character(shortRubric$rubric[[i]])
    ruleId <- as.character(shortRubric$ruleId[[i]])
    mismatches[nrow(mismatches) + 1L, ] <- list(rubricId, ruleId, paste(missing, collapse = ", "), neededString)
    shortRubric$neededConceptSets[[i]] <- appendNames(neededString, missing)

    for (itemUp in seq_along(rubricList)) {
      item <- rubricList[[itemUp]]
      if (is.null(item)) next
      itemRubricId <- as.character(if (!is.null(item$rubricId)) item$rubricId[[1]] else item$rubric_id[[1]])
      if (!identical(itemRubricId, rubricId)) next
      for (tableName in c("hardExclusions", "alternativeExplanations", "evidenceRules")) {
        ruleTable <- item[[tableName]]
        if (!is.data.frame(ruleTable) || !all(c("ruleId", "conceptSets_needed") %in% names(ruleTable))) next
        for (rowUp in which(as.character(ruleTable$ruleId) == ruleId)) {
          ruleTable$conceptSets_needed[[rowUp]] <- appendNames(ruleTable$conceptSets_needed[[rowUp]], missing)
        }
        item[[tableName]] <- ruleTable
      }
      rubricList[[itemUp]] <- item
    }
  }

  list(shortRubric = shortRubric, rubricList = rubricList, mismatches = mismatches)
}

ithSaveShortRubric <- function(shortRubric, conditionName, outputDirectory, yesCombinationReport = TRUE) {
  conditionDir <- file.path(outputDirectory, conditionName)
  dir.create(conditionDir, recursive = TRUE, showWarnings = FALSE)

  ithSaveVersioned(
    saveDirectory = conditionDir,
    objectToSave = list(shortRubric = shortRubric),
    fileName = paste0(conditionName, "_shortRubric.rds")
  )

  concisePath <- ithSaveVersioned(
    saveDirectory = conditionDir,
    objectToSave = shortRubric[, c(1:12)],
    fileName = paste0(conditionName, "_conciseRubric.csv")
  )

  if (isTRUE(yesCombinationReport)) {
    ithRunYesCombinationReport(conditionName, outputDirectory)
  }
  concisePath
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
