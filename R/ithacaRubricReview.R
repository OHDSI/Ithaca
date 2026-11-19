ithRubricReviewOptionNumbers <- 2:3

ithRubricReviewOptionColumns <- function() {
  unlist(lapply(ithRubricReviewOptionNumbers, function(n) {
    paste0("option", n, c("Description", "SuggestedCriteria", "SuggestedScore", "SuggestedType"))
  }))
}

ithRubricReviewFindingFields <- function() {
  optionFields <- stats::setNames(
    replicate(length(ithRubricReviewOptionColumns()), ellmer::type_string(), simplify = FALSE),
    ithRubricReviewOptionColumns()
  )
  c(
    list(
      ruleId = ellmer::type_string(),
      category = ellmer::type_enum(
        values = c("clinicalValidity", "weighting", "additionalInformation", "ruleTypeClassification")
      ),
      severity = ellmer::type_enum(values = c("high", "medium", "low")),
      finding = ellmer::type_string(),
      recommendation = ellmer::type_string(),
      suggestedCriteria = ellmer::type_string(),
      suggestedScore = ellmer::type_string(),
      suggestedType = ellmer::type_string()
    ),
    optionFields,
    list(
      evidence = ellmer::type_string(),
      evidenceConfidence = ellmer::type_enum(values = c("high", "medium", "low"))
    )
  )
}

# Rows that belong together: every finding on the same single rule (the user may accept only one
# change per rule), or the options of one GLOBAL finding. "" for rows that stand alone.
ithReviewOptionGroupKey <- function(out) {
  ruleIdText <- toupper(trimws(as.character(out$ruleId)))
  singleRule <- nzchar(ruleIdText) & ruleIdText != "GLOBAL" & !grepl("[,;/&]|\\sAND\\s", ruleIdText)
  ifelse(
    singleRule, paste0("rule|", out$rubric, "|", ruleIdText),
    ifelse(nzchar(out$optionGroup), paste0("global|", out$rubric, "|", out$optionGroup), "")
  )
}

# Numbers the groups across all rubrics and labels each member "Option k of n"; single rows get blanks.
ithAssignReviewOptionGroups <- function(out) {
  groupKey <- ithReviewOptionGroupKey(out)
  out$optionGroup <- ""
  out$option <- ""
  groupNumber <- 0L
  for (key in unique(groupKey[nzchar(groupKey)])) {
    rows <- which(groupKey == key)
    if (length(rows) < 2L) {
      next
    }
    groupNumber <- groupNumber + 1L
    out$optionGroup[rows] <- paste0("G", groupNumber)
    out$option[rows] <- paste0("Option ", seq_along(rows), " of ", length(rows))
  }
  out
}

# Splits each finding that offers extra options into one fully structured row per option, tied
# together by optionGroup, so the user accepts exactly one row instead of editing newCriteria.
# An option left without type, score, and criteria cannot be applied; the apply step reports it.
ithExpandReviewOptions <- function(findings) {
  field <- function(i, name) {
    value <- if (name %in% names(findings)) as.character(findings[[name]][[i]]) else ""
    if (is.na(value)) "" else trimws(value)
  }
  keep <- setdiff(names(findings), ithRubricReviewOptionColumns())
  rows <- list()
  groupCount <- 0L
  for (i in seq_len(nrow(findings))) {
    base <- findings[i, keep, drop = FALSE]
    base$optionGroup <- ""
    base$option <- ""
    base$.findingSeq <- i
    base$.optionIndex <- 1L
    extras <- Filter(function(n) nzchar(field(i, paste0("option", n, "Description"))), ithRubricReviewOptionNumbers)
    if (length(extras) == 0L) {
      rows[[length(rows) + 1L]] <- base
      next
    }
    groupCount <- groupCount + 1L
    total <- length(extras) + 1L
    base$optionGroup <- paste0("G", groupCount)
    base$option <- paste0("Option 1 of ", total)
    rows[[length(rows) + 1L]] <- base
    for (k in seq_along(extras)) {
      n <- extras[[k]]
      row <- base
      row$recommendation <- field(i, paste0("option", n, "Description"))
      row$suggestedCriteria <- field(i, paste0("option", n, "SuggestedCriteria"))
      row$suggestedScore <- field(i, paste0("option", n, "SuggestedScore"))
      row$suggestedType <- field(i, paste0("option", n, "SuggestedType"))
      row$option <- paste0("Option ", k + 1L, " of ", total)
      row$.optionIndex <- k + 1L
      rows[[length(rows) + 1L]] <- row
    }
  }
  do.call(rbind, rows)
}

ithRubricReviewType <- function() {
  ellmer::type_object(
    overallAssessment = ellmer::type_string(),
    findings = ellmer::type_array(do.call(ellmer::type_object, ithRubricReviewFindingFields()))
  )
}

ithRubricReviewConsolidationType <- function() {
  ellmer::type_object(
    overallAssessment = ellmer::type_string(),
    findings = ellmer::type_array(do.call(
      ellmer::type_object,
      c(ithRubricReviewFindingFields(), list(sourceFindingIds = ellmer::type_string()))
    )),
    droppedFindings = ellmer::type_array(ellmer::type_object(
      findingId = ellmer::type_string(),
      reason = ellmer::type_string()
    ))
  )
}

ithBuildRubricReviewConsolidationPrompt <- function(conditionName,
                                                    reviewPrompt,
                                                    runOverall,
                                                    runFindings,
                                                    promptDir = NULL) {
  findingsJson <- as.character(jsonlite::toJSON(runFindings, auto_unbox = TRUE, pretty = TRUE, na = "null"))
  overallText <- paste0("Run ", seq_along(runOverall), ": ", runOverall, collapse = "\n")

  prompt <- ithReadPrompt("consolidateConciseRubricReview.txt", promptDir = promptDir)
  prompt <- gsub("<RUN_COUNT>", as.character(length(runOverall)), prompt, fixed = TRUE)
  prompt <- gsub("<PHENOTYPE_NAME>", conditionName, prompt, fixed = TRUE)
  prompt <- gsub("<RUN_OVERALL>", overallText, prompt, fixed = TRUE)
  prompt <- gsub("<REVIEW_INSTRUCTIONS>", reviewPrompt, prompt, fixed = TRUE)
  gsub("<RUN_FINDINGS>", findingsJson, prompt, fixed = TRUE)
}

# Counts the distinct runs cited by each "run.number" ID list (e.g. "1.2, 3.1" -> 2).
ithCountReviewRuns <- function(sourceFindingIds) {
  vapply(as.character(sourceFindingIds), function(ids) {
    if (is.na(ids)) {
      return(0L)
    }
    runs <- regmatches(ids, gregexpr("[0-9]+(?=\\.[0-9]+)", ids, perl = TRUE))[[1]]
    length(unique(runs))
  }, integer(1), USE.NAMES = FALSE)
}

ithReviewAlternativeVerbs <- c(
  "add", "adjust", "apply", "broaden", "cap", "change", "collapse", "combine", "convert", "create",
  "delete", "demote", "downgrade", "drop", "eliminate", "enforce", "exclude", "increase", "keep",
  "limit", "lower", "make", "merge", "modify", "move", "narrow", "promote", "raise", "reclassify",
  "redefine", "reduce", "replace", "require", "restrict", "retain", "reweight", "rewrite", "set",
  "split", "tighten", "treat", "upgrade", "use", "widen"
)

# Returns the phrase that makes a recommendation offer more than one action, or "" if none.
# "or" followed by an action verb ("collapse ... or enforce ...") counts; clinical "or" between
# nouns ("inpatient or emergency department visit") does not.
ithDetectReviewAlternatives <- function(recommendation) {
  verbs <- paste(ithReviewAlternativeVerbs, collapse = "|")
  pattern <- paste0(
    "\\b(alternatively|at (a )?minimum|or else|option 2)\\b|",
    "\\beither\\s+(\\(?[a-z0-9]\\)|[0-9][.)]|to\\s|(", verbs, ")\\b)|",
    "\\bor\\s+(to\\s+|simply\\s+|instead\\s+)?(", verbs, ")\\b"
  )
  text <- as.character(recommendation)
  text[is.na(text)] <- ""
  vapply(text, function(x) {
    hit <- regmatches(x, regexpr(pattern, x, ignore.case = TRUE, perl = TRUE))
    if (length(hit) == 0L) "" else hit
  }, character(1), USE.NAMES = FALSE)
}

# Runs the review `reviewRuns` times for one rubric; with more than one run, an extra LLM call
# consolidates the runs. Returns the final findings, overall assessment, raw run findings, and
# the run findings the consolidation dropped.
ithRunRubricReview <- function(conditionName, rubricId, reviewPrompt, reviewRuns, llmClient, promptDir, verbose) {
  runOverall <- character()
  runFindings <- list()
  for (runNumber in seq_len(reviewRuns)) {
    ithMaybeMessage(verbose && reviewRuns > 1L, "  Review run ", runNumber, "/", reviewRuns, " for rubric ", rubricId)
    result <- ithQueryLlm(
      llmClient = llmClient,
      prompt = reviewPrompt,
      output = "list",
      ellmerTypeObject = ithRubricReviewType(),
      silent = !isTRUE(verbose)
    )
    runOverall[[runNumber]] <- paste(as.character(result$overallAssessment), collapse = " ")
    findings <- result$findings
    if (is.data.frame(findings) && nrow(findings) > 0L) {
      findings <- cbind(
        findingId = paste0(runNumber, ".", seq_len(nrow(findings))),
        findings,
        stringsAsFactors = FALSE
      )
      runFindings[[length(runFindings) + 1L]] <- findings
    }
  }
  allRunFindings <- if (length(runFindings) > 0L) ithBindRows(runFindings) else NULL

  if (reviewRuns == 1L || is.null(allRunFindings)) {
    final <- allRunFindings
    if (!is.null(final)) {
      final$sourceFindingIds <- final$findingId
      final$findingId <- NULL
    }
    return(list(
      findings = final,
      overall = paste(runOverall, collapse = " "),
      runFindings = allRunFindings,
      dropped = NULL
    ))
  }

  ithMaybeMessage(verbose, "  Consolidating ", nrow(allRunFindings), " findings from ", reviewRuns, " runs for rubric ", rubricId)
  consolidated <- ithQueryLlm(
    llmClient = llmClient,
    prompt = ithBuildRubricReviewConsolidationPrompt(conditionName, reviewPrompt, runOverall, allRunFindings, promptDir = promptDir),
    output = "list",
    ellmerTypeObject = ithRubricReviewConsolidationType(),
    silent = !isTRUE(verbose)
  )
  final <- consolidated$findings
  if (!is.data.frame(final) || nrow(final) == 0L) {
    final <- NULL
  }
  dropped <- consolidated$droppedFindings
  if (!is.data.frame(dropped) || nrow(dropped) == 0L) {
    dropped <- NULL
  }

  citedText <- c(
    if (!is.null(final)) as.character(final$sourceFindingIds),
    if (!is.null(dropped)) as.character(dropped$findingId)
  )
  citedIds <- unlist(regmatches(citedText, gregexpr("[0-9]+\\.[0-9]+", citedText)))
  unaccounted <- setdiff(allRunFindings$findingId, citedIds)
  if (length(unaccounted) > 0L) {
    message(
      "Consolidation for rubric ", rubricId, " did not account for ", length(unaccounted),
      " run finding(s): ", paste(unaccounted, collapse = ", "),
      ". See droppedReason in the saved conciseRubricReviewRuns file (or the review's runFindings attribute)."
    )
  }

  list(
    findings = final,
    overall = paste(as.character(consolidated$overallAssessment), collapse = " "),
    runFindings = allRunFindings,
    dropped = rbind(
      if (!is.null(dropped)) data.frame(findingId = as.character(dropped$findingId), reason = as.character(dropped$reason), stringsAsFactors = FALSE),
      if (length(unaccounted) > 0L) data.frame(findingId = unaccounted, reason = "not accounted for by the consolidation (not merged or dropped)", stringsAsFactors = FALSE)
    )
  )
}

ithBindRows <- function(frames) {
  columns <- unique(unlist(lapply(frames, names)))
  do.call(rbind, lapply(frames, function(x) {
    for (column in setdiff(columns, names(x))) {
      x[[column]] <- NA
    }
    x[, columns, drop = FALSE]
  }))
}

ithBuildRubricReviewPrompt <- function(conditionName,
                                       rubricRows,
                                       dxCriteria,
                                       additionalInformation,
                                       promptDir = NULL) {
  orNone <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x[[1]]) || !nzchar(trimws(x[[1]]))) "<none supplied>" else as.character(x[[1]])
  }
  rubricJson <- as.character(jsonlite::toJSON(rubricRows, auto_unbox = TRUE, pretty = TRUE, na = "null"))

  # Rubric JSON is substituted last so placeholder-like text inside rule criteria is never re-expanded.
  prompt <- ithReadPrompt("reviewConciseRubric.txt", promptDir = promptDir)
  prompt <- gsub("<PHENOTYPE_NAME>", conditionName, prompt, fixed = TRUE)
  prompt <- gsub("<DX_CRITERA>", orNone(dxCriteria), prompt, fixed = TRUE)
  prompt <- gsub("<ADDITIONAL_INFORMATION>", orNone(additionalInformation), prompt, fixed = TRUE)
  gsub("<CONCISE_RUBRIC>", rubricJson, prompt, fixed = TRUE)
}

# A finding may cite several rules ("ER9, ER10"); their values are joined one per line.
# GLOBAL or unknown rule IDs yield "".
ithLookupReviewCriteria <- function(rubric, ruleId, conciseRubric, column = "criteria") {
  key <- paste(trimws(as.character(conciseRubric$rubric)), toupper(trimws(as.character(conciseRubric$ruleId))), sep = "||")
  criteria <- as.character(conciseRubric[[column]])
  vapply(seq_along(ruleId), function(i) {
    ids <- toupper(trimws(unlist(strsplit(as.character(ruleId[[i]]), "[,;/&]|\\s+and\\s+", perl = TRUE))))
    ids <- ids[nzchar(ids)]
    found <- criteria[match(paste(trimws(rubric[[i]]), ids, sep = "||"), key)]
    hit <- !is.na(found)
    if (!any(hit)) {
      return("")
    }
    if (length(ids) == 1L) {
      return(found[[1]])
    }
    paste(paste0(ids[hit], ": ", found[hit]), collapse = "\n")
  }, character(1))
}

#' Critically review a concise rubric
#'
#' Reads a concise rubric and asks an LLM to assess (1) clinical validity of each rule,
#' (2) rule weighting and yes/no thresholds, (3) consistency with the diagnostic criteria and
#' additional information supplied at rubric creation, and (4) whether alternative
#' explanations and hard exclusions are classified appropriately. Every finding includes
#' clinical evidence recalled by the LLM from its training data.
#'
#' The cited evidence comes from the LLM's training data, not a literature search. Verify
#' citations before relying on them.
#'
#' @param conditionName Condition/rubric name (`rubricName` or `testCondition` in the config).
#' @param outputDirectory Optional root output directory for rubric artifacts. When `NULL`,
#'   the config's `outputDirectory` is used.
#' @param configPath Path to the YAML config. The condition's `dxCriteria` and
#'   `additionalInformation` are resolved from it exactly as during rubric creation
#'   (from `conditionSpecsCsvPath`/`conditionSpecs`, falling back to `clinicalDefinitionFile`).
#' @param conciseRubricPath Optional path to a concise rubric CSV. When `NULL`, the latest
#'   concise rubric CSV in the condition directory is used.
#' @param dxCriteria Optional override for the resolved diagnostic criteria text.
#' @param additionalInformation Optional override for the resolved additional information text.
#' @param llmClient LLM client used for the review. Defaults to the config's
#'   `llmReasoning` settings (or the built-in o3 defaults).
#' @param promptDir Optional prompt folder override for `reviewConciseRubric.txt` and
#'   `consolidateConciseRubricReview.txt`. When `NULL`, the config's `promptDir` is used,
#'   otherwise the package prompts.
#' @param reviewRuns Number of independent LLM review runs per rubric. When `NULL`, the
#'   config's `reviewRuns` is used (default 3). With more than one run, a further LLM call
#'   merges duplicate findings, drops invalid ones, and turns runs that disagree on the fix into
#'   one finding with options (one row per option, sharing an `optionGroup`); `timesFlagged` reports how many runs
#'   raised each finding.
#' @param saveReview Logical; when `TRUE`, save the findings as a versioned
#'   `<condition>_conciseRubricReview_V#.csv` (first column `condition`) in the condition directory (plus the individual run findings as
#'   `<condition>_conciseRubricReviewRuns_V#.csv` with the same version when `reviewRuns > 1`).
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return A data frame of findings with columns `Accept` (blank; mark findings to apply),
#'   `newCriteria` (blank; optional user amendment that replaces `suggestedCriteria` when filled),
#'   `timesFlagged` (runs that raised the finding, e.g. `"3/3"`),
#'   `rubric`, `ruleId`, `originalCriteria` (current criteria of the cited rule(s), blank for
#'   `GLOBAL` findings), `originalScore` (current score of the cited rule, blank for `GLOBAL`),
#'   `category`, `severity`,
#'   `finding`, `recommendation`, `option` (e.g. `"Option 2 of 3"`, blank for single-action findings),
#'   `optionGroup` (rows sharing it are mutually exclusive options; accept only one),
#'   `suggestedCriteria`, `suggestedScore`, `suggestedType`, `evidence`,
#'   `evidenceConfidence`, `sourceFindingIds` (run findings merged into this row). Rows are sorted
#'   by rubric, rule group (GLOBAL, HE, AE, ER), rule number, then `timesFlagged` (most first),
#'   then severity. The per-rubric overall
#'   assessments are attached as the `overallAssessment` attribute and the individual run
#'   findings (with `droppedReason` for findings the consolidation dropped) as `runFindings`.
#' @examples
#' \dontrun{
#' review <- reviewConciseRubric("Major Depressive Disorder")
#' attr(review, "overallAssessment")
#' review1 <- reviewConciseRubric("Major Depressive Disorder", reviewRuns = 1)
#' }
#' @export
reviewConciseRubric <- function(conditionName,
                                outputDirectory = NULL,
                                configPath = "config.yaml",
                                conciseRubricPath = NULL,
                                dxCriteria = NULL,
                                additionalInformation = NULL,
                                llmClient = NULL,
                                promptDir = NULL,
                                reviewRuns = NULL,
                                saveReview = TRUE,
                                verbose = TRUE) {
  if (is.null(outputDirectory) || is.null(dxCriteria) || is.null(additionalInformation) || is.null(llmClient) ||
      is.null(reviewRuns)) {
    config <- ithReadIthacaConditionConfig(configPath)
    spec <- ithFindConfigConditionSpec(config, conditionName)
    if (is.null(reviewRuns)) {
      reviewRuns <- ithCoalesce(config$reviewRuns, 3L)
    }
    if (is.null(promptDir) && !is.null(config$promptDir)) {
      promptDir <- config$promptDir
    }
    if (is.null(outputDirectory)) {
      if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
        stop("outputDirectory was not supplied and is not defined in config: ", configPath)
      }
      outputDirectory <- as.character(config$outputDirectory[[1]])
    }
    if (is.null(dxCriteria)) {
      dxCriteria <- as.character(ithCoalesce(spec$dxCriteria, "")[[1]])
    }
    if (is.null(additionalInformation)) {
      additionalInformation <- as.character(ithCoalesce(spec$additionalInformation, "")[[1]])
    }
    if (is.null(llmClient)) {
      llmClient <- ithGetLlmClientFromConfig(config, "reasoning")
    }
  }

  conditionDir <- file.path(outputDirectory, conditionName)
  if (!dir.exists(conditionDir)) {
    stop("Condition directory not found: ", conditionDir)
  }
  reviewRuns <- suppressWarnings(as.integer(reviewRuns[[1]]))
  if (is.na(reviewRuns) || reviewRuns < 1L) {
    stop("reviewRuns must be a whole number >= 1", call. = FALSE)
  }
  ithMaybeMessage(verbose, "Review runs per rubric: ", reviewRuns, if (reviewRuns > 1L) " (+1 consolidation call)" else "")
  ithMaybeMessage(verbose, "Diagnostic criteria used: ", ifelse(nzchar(dxCriteria), dxCriteria, "<none>"))
  ithMaybeMessage(verbose, "Additional information used: ", ifelse(nzchar(additionalInformation), additionalInformation, "<none>"))

  if (is.null(conciseRubricPath)) {
    conciseRubric <- ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
    if (is.null(conciseRubric)) {
      stop("No concise rubric found for condition: ", conditionName)
    }
  } else {
    if (!file.exists(conciseRubricPath)) {
      stop("Concise rubric file not found: ", conciseRubricPath)
    }
    conciseRubric <- utils::read.csv(conciseRubricPath, stringsAsFactors = FALSE)
  }
  ithValidateEditedConciseRubric(conciseRubric, conditionName = conditionName)

  keepColumns <- intersect(
    c("rubric", "rubricName", "rubricYesScore", "rubricNoScore", "ruleId", "type", "score", "criteria", "rationale", "neededConceptSets"),
    names(conciseRubric)
  )
  rubricIds <- unique(as.character(conciseRubric$rubric))
  findingsList <- list()
  runFindingsList <- list()
  overall <- character()

  for (rubricId in rubricIds) {
    rubricRows <- conciseRubric[as.character(conciseRubric$rubric) == rubricId, keepColumns, drop = FALSE]
    ruleIds <- toupper(trimws(as.character(rubricRows$ruleId)))
    isFixedRule <- ruleIds %in% c("ER1", "ER2") |
      (ruleIds == "AE1" & grepl("_relevant_diagnosis_codes(,|$)", tolower(gsub("\\s+", "", as.character(rubricRows$neededConceptSets)))))
    rubricRows$fixed <- isFixedRule
    reviewableCount <- sum(!isFixedRule)
    if (any(isFixedRule)) {
      ithMaybeMessage(verbose, "  ER1/ER2/AE1 are fixed rules, shown to the reviewer for context only")
    }
    if (reviewableCount == 0L) {
      overall[[rubricId]] <- "No reviewable rules."
      next
    }
    ithMaybeMessage(verbose, "Reviewing rubric ", rubricId, " (", reviewableCount, " rules) for condition: ", conditionName)

    reviewed <- ithRunRubricReview(
      conditionName = conditionName,
      rubricId = rubricId,
      reviewPrompt = ithBuildRubricReviewPrompt(conditionName, rubricRows, dxCriteria, additionalInformation, promptDir = promptDir),
      reviewRuns = reviewRuns,
      llmClient = llmClient,
      promptDir = promptDir,
      verbose = verbose
    )

    # Drop any finding that targets or mentions ER1/ER2 despite the prompt telling the LLM not to.
    excludedIds <- unique(c("ER1", "ER2", ruleIds[isFixedRule]))
    isExcludedFinding <- function(ruleId) {
      ids <- unlist(strsplit(toupper(trimws(as.character(ruleId))), "[,;/&]|\\s+and\\s+", perl = TRUE))
      any(trimws(ids) %in% excludedIds)
    }
    mentionsFixedRule <- function(x) {
      text <- paste(
        as.character(x$recommendation), as.character(x$suggestedCriteria),
        if ("option2Description" %in% names(x)) as.character(x$option2Description) else "",
        if ("option3Description" %in% names(x)) as.character(x$option3Description) else ""
      )
      grepl(paste0("\\b(", paste(excludedIds, collapse = "|"), ")\\b"), text, ignore.case = TRUE, perl = TRUE)
    }
    dropExcluded <- function(x) {
      drop <- vapply(x$ruleId, isExcludedFinding, logical(1)) | mentionsFixedRule(x)
      if (any(drop)) {
        ithMaybeMessage(verbose, "  Dropped ", sum(drop), " finding(s) that target or mention ER1/ER2")
      }
      x <- x[!drop, , drop = FALSE]
      if (nrow(x) == 0L) NULL else x
    }
    if (!is.null(reviewed$findings) && nrow(reviewed$findings) > 0L) {
      reviewed$findings <- dropExcluded(reviewed$findings)
    }
    if (!is.null(reviewed$runFindings) && nrow(reviewed$runFindings) > 0L) {
      reviewed$runFindings <- dropExcluded(reviewed$runFindings)
    }

    overall[[rubricId]] <- reviewed$overall
    if (!is.null(reviewed$findings)) {
      reviewed$findings$rubric <- rubricId
      findingsList[[length(findingsList) + 1L]] <- reviewed$findings
    }
    if (!is.null(reviewed$runFindings)) {
      runs <- reviewed$runFindings
      runs$rubric <- rubricId
      runs$droppedReason <- ""
      if (!is.null(reviewed$dropped)) {
        hit <- match(runs$findingId, trimws(as.character(reviewed$dropped$findingId)))
        runs$droppedReason[!is.na(hit)] <- as.character(reviewed$dropped$reason)[hit[!is.na(hit)]]
      }
      runFindingsList[[length(runFindingsList) + 1L]] <- runs
    }
  }

  columns <- c(
    "condition", "Accept", "newCriteria", "timesFlagged", "rubric", "ruleId", "originalCriteria", "originalScore", "category", "severity", "finding",
    "recommendation", "option", "optionGroup", "suggestedCriteria", "suggestedScore", "suggestedType", "evidence",
    "evidenceConfidence", "sourceFindingIds"
  )
  if (length(findingsList) == 0L) {
    out <- as.data.frame(
      stats::setNames(replicate(length(columns), character(), simplify = FALSE), columns),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, lapply(findingsList, function(x) {
      x <- ithExpandReviewOptions(x)
      x$condition <- conditionName
      for (column in setdiff(columns, names(x))) {
        x[[column]] <- ""
      }
      x[, c(columns, ".findingSeq", ".optionIndex"), drop = FALSE]
    }))
    runsFlagged <- ithCountReviewRuns(out$sourceFindingIds)
    out$timesFlagged <- paste0(runsFlagged, "/", reviewRuns)
    out$originalCriteria <- ithLookupReviewCriteria(out$rubric, out$ruleId, conciseRubric)
    out$originalScore <- ithLookupReviewCriteria(out$rubric, out$ruleId, conciseRubric, column = "score")
    out$Accept <- ""
    out$newCriteria <- ""
    # The LLM sometimes writes "X or Y" in a recommendation without giving structured options.
    alternativePhrase <- ithDetectReviewAlternatives(out$recommendation)
    alternativePhrase[nzchar(out$optionGroup)] <- ""
    if (any(nzchar(alternativePhrase))) {
      message(
        sum(nzchar(alternativePhrase)),
        " finding(s) offer more than one option without structured options; apply will require newCriteria for: ",
        paste0(out$ruleId[nzchar(alternativePhrase)], " ('", alternativePhrase[nzchar(alternativePhrase)], "')", collapse = ", ")
      )
    }
    severityOrder <- match(out$severity, c("high", "medium", "low"))
    ruleIdText <- toupper(trimws(as.character(out$ruleId)))
    groupKey <- ithReviewOptionGroupKey(out)
    keyed <- nzchar(groupKey)
    runsKey <- runsFlagged
    severityKey <- severityOrder
    if (any(keyed)) {
      runsKey[keyed] <- stats::ave(runsFlagged[keyed], groupKey[keyed], FUN = max)
      severityKey[keyed] <- suppressWarnings(stats::ave(severityOrder[keyed], groupKey[keyed], FUN = min))
    }
    ruleGroupOrder <- ifelse(
      ruleIdText == "GLOBAL", 1L,
      ifelse(grepl("^HE", ruleIdText), 2L,
        ifelse(grepl("^AE", ruleIdText), 3L,
          ifelse(grepl("^ER", ruleIdText), 4L, 5L)
        )
      )
    )
    ruleNumber <- suppressWarnings(as.integer(sub("^[A-Z]+([0-9]+).*$", "\\1", ruleIdText)))
    out <- out[order(out$rubric, ruleGroupOrder, ruleNumber, -runsKey, severityKey, ruleIdText, out$.findingSeq, out$.optionIndex, na.last = TRUE), , drop = FALSE]
    out <- ithAssignReviewOptionGroups(out)
    out$.findingSeq <- NULL
    out$.optionIndex <- NULL
    rownames(out) <- NULL
  }
  runFindings <- if (length(runFindingsList) > 0L) ithBindRows(runFindingsList) else NULL

  if (isTRUE(saveReview)) {
    path <- ithSaveVersioned(
      saveDirectory = conditionDir,
      objectToSave = out,
      fileName = paste0(conditionName, "_conciseRubricReview.csv")
    )
    ithMaybeMessage(verbose, "Saved concise rubric review: ", path)
    if (reviewRuns > 1L && !is.null(runFindings)) {
      # Same version number as the review so sourceFindingIds can be traced to the raw runs.
      runsPath <- sub("_conciseRubricReview_V", "_conciseRubricReviewRuns_V", path, fixed = TRUE)
      runFindings <- cbind(condition = conditionName, runFindings, stringsAsFactors = FALSE)
      ithSaveByExt(runFindings, runsPath, overwrite = TRUE)
      ithMaybeMessage(verbose, "Saved individual review runs: ", runsPath)
    }
  }

  attr(out, "overallAssessment") <- overall
  attr(out, "runFindings") <- runFindings
  out
}

#' Review the concise rubric for every condition in a config
#'
#' Runs [reviewConciseRubric()] for each condition in the config (including
#' `conditionSpecsCsvPath`). Conditions without a concise rubric are skipped, and an error in
#' one condition does not stop the others. Each condition's findings are saved as
#' `<condition>_conciseRubricReview_V*.csv` in its condition directory.
#'
#' @param conditionNames Optional character vector of condition names (`rubricName` or
#'   `testCondition`, case-insensitive) to review. When `NULL`, all configured conditions are reviewed.
#' @param configPath Path to YAML configuration file.
#'
#' @return Data frame with one row per condition: `conditionName`, `status` (`reviewed`,
#'   `skipped: ...`, or `error: ...`), and `findings` (number of findings). The combined
#'   findings for all reviewed conditions are attached as the `findings` attribute.
#' @examples
#' \dontrun{
#' results <- reviewConciseRubricsFromConfig()
#' results <- reviewConciseRubricsFromConfig("Major Depressive Disorder")
#' allFindings <- attr(results, "findings")
#' }
#' @export
reviewConciseRubricsFromConfig <- function(conditionNames = NULL,
                                           configPath = "config.yaml") {
  ithCheckConditionNamesNotConfig(conditionNames)
  config <- ithReadIthacaConditionConfig(configPath)
  if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
    stop("outputDirectory is not defined in config: ", configPath)
  }
  outputDirectory <- as.character(config$outputDirectory[[1]])
  verbose <- isTRUE(ithCoalesce(config$verbose, TRUE))
  llmClient <- ithGetLlmClientFromConfig(config, "reasoning")

  specs <- config$conditionSpecs
  specNames <- vapply(
    specs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    if (!is.character(conditionNames)) {
      stop("conditionNames must be NULL or a character vector")
    }
    missingNames <- conditionNames[!tolower(trimws(conditionNames)) %in% tolower(specNames)]
    if (length(missingNames) > 0) {
      stop("Requested conditionNames are not configured: ", paste(missingNames, collapse = ", "),
           "\n  Configured conditions: ", paste(specNames, collapse = ", "), call. = FALSE)
    }
    keep <- tolower(specNames) %in% tolower(trimws(conditionNames))
    specs <- specs[keep]
    specNames <- specNames[keep]
  }

  statusRow <- function(conditionName, status, findings = 0L) {
    data.frame(conditionName = conditionName, status = status, findings = findings, stringsAsFactors = FALSE)
  }

  allFindings <- list()
  results <- lapply(seq_along(specs), function(i) {
    conditionName <- specNames[[i]]
    spec <- specs[[i]]
    conditionDir <- file.path(outputDirectory, conditionName)
    if (is.null(ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv")))) {
      return(statusRow(conditionName, "skipped: no concise rubric found"))
    }

    tryCatch({
      ithMaybeMessage(verbose, "Reviewing condition ", i, "/", length(specs), ": ", conditionName)
      review <- reviewConciseRubric(
        conditionName = conditionName,
        outputDirectory = outputDirectory,
        configPath = configPath,
        dxCriteria = as.character(ithCoalesce(spec$dxCriteria, "")[[1]]),
        additionalInformation = as.character(ithCoalesce(spec$additionalInformation, "")[[1]]),
        llmClient = llmClient,
        promptDir = config$promptDir,
        reviewRuns = ithCoalesce(config$reviewRuns, 3L),
        saveReview = TRUE,
        verbose = verbose
      )
      if (nrow(review) > 0L) {
        allFindings[[length(allFindings) + 1L]] <<- review
      }
      statusRow(conditionName, "reviewed", nrow(review))
    }, error = function(e) {
      statusRow(conditionName, paste("error:", conditionMessage(e)))
    })
  })

  results <- do.call(rbind, results)
  failed <- startsWith(results$status, "error")
  if (any(failed)) {
    warning(sum(failed), " condition(s) failed; see the status column.")
  }

  attr(results, "findings") <- if (length(allFindings) > 0L) do.call(rbind, allFindings) else NULL
  results
}
