ithReviewTypePrefixes <- c(hard_exclusion = "HE", alternativeExplanations = "AE", evidenceRules = "ER")

# Non-type actions accepted in suggestedType. Add future actions here (accepted spellings ->
# canonical name) and handle the canonical name in ithPlanConciseRubricReview() and
# ithBuildReviewedConciseRubric().
ithReviewRuleActions <- list(
  remove = c("remove", "removerule", "delete", "deleterule")
)

ithNormalizeReviewRuleType <- function(x) {
  key <- tolower(gsub("[^A-Za-z]", "", as.character(x)))
  if (!nzchar(key)) {
    return("")
  }
  for (action in names(ithReviewRuleActions)) {
    if (key %in% ithReviewRuleActions[[action]]) {
      return(action)
    }
  }
  if (key %in% c("hardexclusion", "hardexclusions")) {
    return("hard_exclusion")
  }
  if (key %in% c("alternativeexplanation", "alternativeexplanations")) {
    return("alternativeExplanations")
  }
  if (key %in% c("evidencerule", "evidencerules")) {
    return("evidenceRules")
  }
  NA_character_
}

ithParseReviewAccept <- function(x) {
  value <- toupper(trimws(as.character(x)))
  if (value %in% c("", "NA", "N", "NO", "FALSE", "0")) {
    return(FALSE)
  }
  if (value %in% c("Y", "YES", "TRUE", "X", "1")) {
    return(TRUE)
  }
  NA
}

# Returns the resolved score for a rule of `type`, or a character issue message.
ithResolveReviewScore <- function(type, suggestedScore) {
  if (identical(type, "hard_exclusion")) {
    return(-100)
  }
  if (!nzchar(suggestedScore)) {
    return(paste0("suggestedScore is required for a ", type, " rule but is blank"))
  }
  score <- suppressWarnings(as.numeric(suggestedScore))
  if (is.na(score)) {
    return(paste0("suggestedScore '", suggestedScore, "' is not a single number"))
  }
  if (identical(type, "alternativeExplanations") && score >= 0) {
    return(paste0("alternativeExplanations score must be negative (got ", suggestedScore, ")"))
  }
  if (identical(type, "evidenceRules") && score <= 0) {
    return(paste0("evidenceRules score must be positive (got ", suggestedScore, ")"))
  }
  score
}

# Validates accepted review rows against the concise rubric and returns per-row issues plus
# the planned edits. No LLM calls are made here.
ithPlanConciseRubricReview <- function(review, conciseRubric, protectedRuleIds = character(0)) {
  normalize <- function(x) trimws(gsub("\\s+", " ", gsub("\r\n?", "\n", as.character(x))))
  field <- function(i, name) {
    if (!(name %in% names(review))) {
      return("")
    }
    value <- as.character(review[[name]][[i]])
    if (is.na(value)) "" else trimws(value)
  }

  conciseKey <- paste(trimws(conciseRubric$rubric), toupper(trimws(conciseRubric$ruleId)), sep = "||")
  issues <- rep("", nrow(review))
  accepted <- rep(FALSE, nrow(review))
  plans <- vector("list", nrow(review))
  # Review files made before structured options cannot show which rows are options of one finding.
  legacyFormat <- !("optionGroup" %in% names(review))

  for (i in seq_len(nrow(review))) {
    if (nzchar(field(i, "applied"))) {
      next
    }
    acceptValue <- ithParseReviewAccept(field(i, "Accept"))
    if (is.na(acceptValue)) {
      issues[[i]] <- paste0("Unrecognized Accept value '", field(i, "Accept"), "' (use Y/YES/TRUE/X/1 or leave blank)")
      next
    }
    if (!acceptValue) {
      next
    }
    accepted[[i]] <- TRUE
    if (legacyFormat) {
      issues[[i]] <- paste(
        "This review file predates structured options (no optionGroup column);",
        "re-run reviewConciseRubric() to regenerate it, then mark Accept again"
      )
      next
    }

    rowIssues <- character()
    rubricId <- field(i, "rubric")
    ruleIdText <- field(i, "ruleId")
    ruleIds <- trimws(unlist(strsplit(ruleIdText, "[,;/&]|\\s+and\\s+", perl = TRUE)))
    ruleIds <- ruleIds[nzchar(ruleIds)]
    newCriteria <- field(i, "newCriteria")
    # Rows in an optionGroup are structured options; ungrouped rows are checked for options in the text.
    inOptionGroup <- nzchar(field(i, "optionGroup"))
    alternativePhrase <- if (inOptionGroup) "" else ithDetectReviewAlternatives(field(i, "recommendation"))
    emptyOption <- inOptionGroup &&
      !any(nzchar(c(field(i, "suggestedCriteria"), field(i, "suggestedScore"), field(i, "suggestedType"))))
    if ((nzchar(alternativePhrase) || emptyOption) && !nzchar(newCriteria)) {
      issues[[i]] <- paste0(
        if (emptyOption) "This option has no suggestedType, suggestedScore, or suggestedCriteria" else paste0("The recommendation offers more than one option ('", alternativePhrase, "') without structured fields"),
        "; write the action in newCriteria",
        " (the complete replacement criteria, or 'remove' to delete the rule) and set suggestedType/suggestedScore to match"
      )
      next
    }
    # newCriteria = "remove" selects the remove option.
    newCriteriaIsRemove <- nzchar(newCriteria) && identical(ithNormalizeReviewRuleType(newCriteria), "remove")
    criteria <- if (newCriteriaIsRemove) "" else if (nzchar(newCriteria)) newCriteria else field(i, "suggestedCriteria")
    suggestedScore <- field(i, "suggestedScore")
    suggestedType <- if (newCriteriaIsRemove) "remove" else ithNormalizeReviewRuleType(field(i, "suggestedType"))

    if (!(rubricId %in% trimws(conciseRubric$rubric))) {
      rowIssues <- c(rowIssues, paste0("rubric '", rubricId, "' not found in the latest concise rubric"))
    }
    if (length(ruleIds) != 1L) {
      rowIssues <- c(rowIssues, paste0("ruleId '", ruleIdText, "' must cite exactly one rule; split into one row per rule"))
    }
    if (is.na(suggestedType)) {
      rowIssues <- c(rowIssues, paste0(
        "suggestedType '", field(i, "suggestedType"), "' is not hard_exclusion, alternativeExplanations, evidenceRules, or ",
        paste(names(ithReviewRuleActions), collapse = ", ")
      ))
    }
    if (length(rowIssues) > 0L) {
      issues[[i]] <- paste(rowIssues, collapse = "; ")
      next
    }

    ruleId <- toupper(ruleIds[[1]])
    if (ruleId == "GLOBAL") {
      if (identical(suggestedType, "remove")) {
        issues[[i]] <- "suggestedType 'remove' needs a specific ruleId, not GLOBAL"
        next
      }
      if (!nzchar(suggestedType) || !nzchar(criteria)) {
        existingIds <- toupper(trimws(conciseRubric$ruleId[trimws(conciseRubric$rubric) == rubricId]))
        mentioned <- regmatches(
          paste(field(i, "finding"), field(i, "recommendation")),
          gregexpr("\\b(HE|AE|ER)[0-9]+\\b", paste(field(i, "finding"), field(i, "recommendation")), ignore.case = TRUE)
        )[[1]]
        mentioned <- intersect(unique(toupper(mentioned)), existingIds)
        issues[[i]] <- if (length(mentioned) > 0L) {
          paste0(
            "This GLOBAL row is about existing rule(s) ", paste(mentioned, collapse = ", "),
            ", but GLOBAL rows can only add a new rule. Set ruleId to the one rule to change and write its replacement",
            " criteria (or 'remove') in newCriteria; use a separate row for each rule"
          )
        } else {
          "GLOBAL row can only be applied as a new rule: suggestedType and suggestedCriteria (or newCriteria) are required"
        }
        next
      }
      score <- ithResolveReviewScore(suggestedType, suggestedScore)
      if (is.character(score)) {
        issues[[i]] <- score
        next
      }
      plans[[i]] <- list(action = "add", rubric = rubricId, type = suggestedType, score = score, criteria = criteria, rationale = field(i, "finding"))
      next
    }

    rowIndex <- match(paste(rubricId, ruleId, sep = "||"), conciseKey)
    if (is.na(rowIndex)) {
      issues[[i]] <- paste0("ruleId '", ruleId, "' not found in rubric ", rubricId, " of the latest concise rubric")
      next
    }
    current <- conciseRubric[rowIndex, , drop = FALSE]
    # Blank originalCriteria means the user re-targeted a GLOBAL row to this rule; there is nothing to compare.
    if (nzchar(field(i, "originalCriteria")) && normalize(field(i, "originalCriteria")) != normalize(current$criteria[[1]])) {
      issues[[i]] <- "originalCriteria no longer matches the latest concise rubric (review is stale); rerun the review"
      next
    }

    if (identical(suggestedType, "remove")) {
      if (nzchar(newCriteria) && !newCriteriaIsRemove) {
        issues[[i]] <- "suggestedType is 'remove' but newCriteria contains criteria text; to remove the rule write 'remove' in newCriteria (or clear it), or to keep the rule change suggestedType"
        next
      }
      plans[[i]] <- list(action = "remove", rubric = rubricId, ruleId = ruleId, rowIndex = rowIndex)
      next
    }

    currentType <- ithNormalizeReviewRuleType(current$type[[1]])
    newType <- if (nzchar(suggestedType) && !identical(suggestedType, currentType)) suggestedType else currentType
    typeChanged <- !identical(newType, currentType)
    score <- NULL
    if (typeChanged) {
      score <- ithResolveReviewScore(newType, suggestedScore)
    } else if (nzchar(suggestedScore)) {
      score <- ithResolveReviewScore(newType, suggestedScore)
    }
    if (is.character(score)) {
      issues[[i]] <- score
      next
    }
    if (!typeChanged && is.null(score) && !nzchar(criteria)) {
      issues[[i]] <- "Accepted, but there is nothing to apply: newCriteria, suggestedCriteria, suggestedScore, and suggestedType are all blank or unchanged"
      next
    }

    plans[[i]] <- list(
      action = "modify", rubric = rubricId, ruleId = ruleId, rowIndex = rowIndex,
      type = if (typeChanged) newType else NULL, score = score,
      criteria = if (nzchar(criteria)) criteria else NULL
    )
  }

  addIssue <- function(rows, message) {
    issues[rows] <<- ifelse(nzchar(issues[rows]), paste(issues[rows], message, sep = "; "), message)
  }

  # Rows of one optionGroup are mutually exclusive options for the same finding.
  groupIds <- vapply(seq_len(nrow(review)), function(i) field(i, "optionGroup"), character(1))
  for (group in setdiff(unique(groupIds), "")) {
    groupRows <- which(groupIds == group & accepted)
    if (length(groupRows) > 1L) {
      addIssue(groupRows, paste0("More than one change accepted for the same rule (", field(groupRows[[1]], "ruleId"), ", optionGroup ", group, "); accept only one row per rule and rerun the review for further changes"))
    }
  }

  # Several accepted rows may target the same rule only when they agree on every field they set;
  # removing a rule conflicts with any other change to it.
  targetRows <- which(vapply(plans, function(p) !is.null(p$action) && p$action %in% c("modify", "remove"), logical(1)))
  targets <- vapply(plans[targetRows], function(p) paste(p$rubric, p$ruleId, sep = "||"), character(1))
  for (target in unique(targets[duplicated(targets)])) {
    rows <- targetRows[targets == target]
    ruleLabel <- sub(".*\\|\\|", "", target)
    actions <- unique(vapply(plans[rows], function(p) p$action, character(1)))
    if (length(actions) > 1L) {
      addIssue(rows, paste0("Conflicts with other accepted row(s) for ", ruleLabel, ": one removes the rule and another changes it"))
      next
    }
    if (identical(actions, "remove")) {
      next
    }
    conflicts <- vapply(c("type", "score", "criteria"), function(name) {
      values <- unique(unlist(lapply(plans[rows], function(p) if (is.null(p[[name]])) NULL else as.character(p[[name]]))))
      length(values) > 1L
    }, logical(1))
    if (any(conflicts)) {
      addIssue(rows, paste0(
        "Conflicts with other accepted row(s) for ", ruleLabel, " on: ",
        paste(names(conflicts)[conflicts], collapse = ", ")
      ))
    }
  }

  # Rules listed in the condition's adjustableRuleId are re-scored at report time, so they must
  # stay evidence rules with the same ID.
  protectedRuleIds <- toupper(trimws(as.character(protectedRuleIds)))
  for (i in seq_along(plans)) {
    p <- plans[[i]]
    if (is.null(p$action) || !(p$action %in% c("modify", "remove")) || !(toupper(p$ruleId) %in% protectedRuleIds)) {
      next
    }
    if (identical(p$action, "remove")) {
      addIssue(i, paste0(
        p$ruleId, " is listed in adjustableRuleId in conditionSpecs and cannot be removed; ",
        "change adjustableRuleId first or leave this row unaccepted"
      ))
    } else if (!is.null(p$type)) {
      addIssue(i, paste0(
        p$ruleId, " is listed in adjustableRuleId in conditionSpecs and must stay an evidence rule; ",
        "change adjustableRuleId first or leave this row unaccepted"
      ))
    }
  }

  # The fixed AE1 zero-occurrence penalty (its SQL and concept set are generated) is not editable here.
  isFixedAe1Row <- toupper(trimws(as.character(conciseRubric$ruleId))) == "AE1" &
    grepl("_relevant_diagnosis_codes(,|$)", tolower(gsub("\\s+", "", as.character(conciseRubric$neededConceptSets))))
  for (i in seq_along(plans)) {
    p <- plans[[i]]
    if (is.null(p$action) || !(p$action %in% c("modify", "remove")) || is.null(p$rowIndex) || is.na(p$rowIndex)) {
      next
    }
    if (isTRUE(isFixedAe1Row[[p$rowIndex]])) {
      addIssue(i, "AE1 is the fixed zero-occurrence penalty and cannot be modified or removed; change zeroOccurrencePenalty and run addZeroOccurrencePenaltyRule instead")
    }
  }

  # A rubric must keep at least one evidence rule after removals, or it can never reach YES.
  removeRows <- which(vapply(plans, function(p) identical(p$action, "remove"), logical(1)))
  for (rubricId in unique(vapply(plans[removeRows], function(p) p$rubric, character(1)))) {
    removedIndex <- vapply(Filter(function(p) identical(p$action, "remove") && p$rubric == rubricId, plans), function(p) p$rowIndex, numeric(1))
    finalTypes <- vapply(seq_len(nrow(conciseRubric)), function(r) ithNormalizeReviewRuleType(conciseRubric$type[[r]]), character(1))
    for (p in Filter(function(p) identical(p$action, "modify") && !is.null(p$type), plans)) {
      finalTypes[[p$rowIndex]] <- p$type
    }
    keep <- trimws(conciseRubric$rubric) == rubricId & !(seq_len(nrow(conciseRubric)) %in% removedIndex)
    addedEvidence <- any(vapply(plans, function(p) identical(p$action, "add") && p$rubric == rubricId && identical(p$type, "evidenceRules"), logical(1)))
    if (!any(finalTypes[keep] == "evidenceRules") && !addedEvidence) {
      rows <- removeRows[vapply(plans[removeRows], function(p) p$rubric == rubricId, logical(1))]
      addIssue(rows, paste0("Removing these rule(s) would leave rubric ", rubricId, " with no evidence rules"))
    }
  }

  list(issues = issues, accepted = accepted, plans = plans)
}

ithBuildReviewedConciseRubric <- function(conciseRubric, plans, conditionName, llmClient) {
  edited <- conciseRubric
  plans <- Filter(Negate(is.null), plans)

  for (plan in Filter(function(p) identical(p$action, "modify"), plans)) {
    rowIndex <- plan$rowIndex
    if (!is.null(plan$criteria)) {
      edited$criteria[[rowIndex]] <- plan$criteria
    }
    if (!is.null(plan$score)) {
      edited$score[[rowIndex]] <- plan$score
    }
    # ithApplyConciseRubricEdits() gives a retyped rule its new ID.
    if (!is.null(plan$type) && !identical(plan$type, ithNormalizeReviewRuleType(edited$type[[rowIndex]]))) {
      edited$type[[rowIndex]] <- plan$type
    }
  }

  for (plan in Filter(function(p) identical(p$action, "add"), plans)) {
    template <- edited[trimws(edited$rubric) == plan$rubric, , drop = FALSE][1, , drop = FALSE]
    newRow <- template
    for (column in names(newRow)) {
      if (is.character(newRow[[column]])) {
        newRow[[column]] <- ""
      }
    }
    newRow$condition <- template$condition
    newRow$rubric <- template$rubric
    newRow$rubricName <- template$rubricName
    newRow$rubricYesScore <- template$rubricYesScore
    newRow$rubricNoScore <- template$rubricNoScore
    # Left blank so ithApplyConciseRubricEdits() assigns an ID that was never used before.
    newRow$ruleId <- ""
    newRow$type <- plan$type
    newRow$score <- plan$score
    newRow$criteria <- plan$criteria
    newRow$rationale <- plan$rationale
    if ("okToUse" %in% names(newRow)) {
      newRow$okToUse <- TRUE
    }
    newRow$neededConceptSets <- ithInferConceptSetsFromCriteria(
      criteria = plan$criteria,
      currentConceptSets = "",
      llmClient = llmClient
    )
    if (!nzchar(newRow$neededConceptSets)) {
      stop("LLM found no concept sets for new ", plan$rubric, " rule (", plan$criteria, ")")
    }
    edited <- rbind(edited, newRow)
  }

  removeIndex <- vapply(Filter(function(p) identical(p$action, "remove"), plans), function(p) p$rowIndex, numeric(1))
  if (length(removeIndex) > 0L) {
    edited <- edited[-unique(removeIndex), , drop = FALSE]
  }

  edited
}

# Latest `<condition>_conciseRubricReview_V*.csv`; falls back to the older unprefixed name.
ithFindLatestReviewFile <- function(conditionDir, conditionName) {
  latest <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubricReview.csv"))
  if (is.null(latest)) {
    latest <- ithFindLatestVersion(conditionDir, "conciseRubricReview.csv")
  }
  latest
}

#' Apply accepted findings from a concise rubric review
#'
#' Reads the latest `<condition>_conciseRubricReview_V*.csv` for a condition and applies every row
#' marked in `Accept` (`Y`, `YES`, `TRUE`, `X`, or `1`; blank/`N`/`NO`/`FALSE` rows are
#' skipped). For each accepted row, `newCriteria` replaces the rule criteria when filled,
#' otherwise `suggestedCriteria` is used; `suggestedScore` and `suggestedType` are applied
#' when present. A type change gives the rule a new ID (e.g. `AE4` to the next `HE#`); hard
#' exclusions always score -100, while alternative explanations (negative) and evidence rules
#' (positive) require `suggestedScore`. Accepted `GLOBAL` rows add a new rule when
#' `suggestedType` and criteria are given. `suggestedType = "remove"` (or `newCriteria = "remove"`)
#' deletes the cited rule (each rubric must keep at least one evidence rule). Findings that
#' offer several options appear as one fully structured row per option (same `optionGroup`);
#' accept exactly one of them. Ungrouped rows whose recommendation text offers options, and
#' options with no suggested type, score, or criteria, are only applied once the chosen action
#' is written in `newCriteria` (replacement criteria or `remove`); review files without an
#' `optionGroup` column must be regenerated with [reviewConciseRubric()].
#'
#' Changes are all-or-nothing. When every accepted row is implementable, the edits are applied
#' with [applyConciseRubricEdits()], which writes new versions of the concise rubric, short
#' rubric, rubric list, and (optionally) summary. Otherwise nothing is changed, the message
#' "no changes were made, there were issues with the reviewed changes" is printed, and an
#' `issues` column describing each problem is written into the review file. The `issues`
#' column is ignored (and rewritten) on the next pass.
#'
#' @param conditionName Condition/rubric name (`rubricName` or `testCondition` in the config),
#'   or a character vector of names to apply reviews for several conditions in one call (the
#'   config is read once; an error in one condition does not stop the others).
#' @param outputDirectory Optional root output directory. When `NULL`, the config's
#'   `outputDirectory` is used.
#' @param configPath Path to the YAML config.
#' @param reviewPath Optional path to a review CSV (single condition only). When `NULL`, the latest
#'   `<condition>_conciseRubricReview_V*.csv` in the condition directory is used.
#' @param llmClient LLM client used for concept-set inference and SQL regeneration. Defaults
#'   to the config's `llmReasoning` settings (or the built-in o3 defaults).
#' @param promptDir Optional prompt folder override. When `NULL`, the config's `promptDir` is used.
#' @param updateSummary Logical; regenerate the concise rubric summary text.
#' @param verbose Logical; when `TRUE`, print progress messages.
#'
#' @return For a single condition, invisibly, a list with `status` (`"applied"`, `"issues"`, or
#'   `"nothingAccepted"`), `reviewPath`, and `issues` (data frame of rows with problems). For
#'   several conditions, a data frame with `conditionName` and `status` (`error: ...` when a
#'   condition failed).
#' @examples
#' \dontrun{
#' applyConciseRubricReview("Major Depressive Disorder")
#' applyConciseRubricReview(c("Major Depressive Disorder", "Seizure"))
#' }
#' @export
applyConciseRubricReview <- function(conditionName,
                                     outputDirectory = NULL,
                                     configPath = "config.yaml",
                                     reviewPath = NULL,
                                     llmClient = NULL,
                                     promptDir = NULL,
                                     updateSummary = TRUE,
                                     verbose = TRUE) {
  if (is.null(outputDirectory) || is.null(llmClient) || is.null(promptDir)) {
    config <- ithReadIthacaConditionConfig(configPath)
    if (is.null(outputDirectory)) {
      if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
        stop("outputDirectory was not supplied and is not defined in config: ", configPath)
      }
      outputDirectory <- as.character(config$outputDirectory[[1]])
    }
    if (is.null(promptDir)) {
      promptDir <- ithCoalesce(config$promptDir, ithPromptDir())
    }
    if (is.null(llmClient)) {
      llmClient <- ithGetLlmClientFromConfig(config, "reasoning")
    }
  }

  if (!is.character(conditionName) || length(conditionName) == 0L || any(!nzchar(trimws(conditionName)))) {
    stop("conditionName must be one or more non-empty condition names", call. = FALSE)
  }
  if (length(conditionName) > 1L) {
    if (!is.null(reviewPath)) {
      stop("reviewPath can only be supplied with a single conditionName", call. = FALSE)
    }
    results <- lapply(conditionName, function(name) {
      status <- tryCatch(
        applyConciseRubricReview(
          conditionName = name,
          outputDirectory = outputDirectory,
          configPath = configPath,
          llmClient = llmClient,
          promptDir = promptDir,
          updateSummary = updateSummary,
          verbose = verbose
        )$status,
        error = function(e) paste("error:", conditionMessage(e))
      )
      data.frame(conditionName = name, status = status, stringsAsFactors = FALSE)
    })
    results <- do.call(rbind, results)
    if (any(results$status == "issues")) {
      message("Some conditions had issues; see the 'issues' column in their latest <condition>_conciseRubricReview_V*.csv.")
    }
    return(results)
  }

  conditionDir <- file.path(outputDirectory, conditionName)
  if (is.null(reviewPath)) {
    latestReview <- ithFindLatestReviewFile(conditionDir, conditionName)
    if (is.null(latestReview)) {
      stop("No ", conditionName, "_conciseRubricReview_V*.csv found for condition: ", conditionName)
    }
    reviewPath <- file.path(conditionDir, latestReview)
  }
  if (!file.exists(reviewPath)) {
    stop("Review file not found: ", reviewPath)
  }
  ithMaybeMessage(verbose, "Reading review file: ", reviewPath)

  review <- utils::read.csv(reviewPath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  review$issues <- NULL
  conciseRubric <- ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
  if (is.null(conciseRubric)) {
    stop("No concise rubric found for condition: ", conditionName)
  }

  protectedRuleIds <- ithConfiguredAdjustableRuleIds(configPath, conditionName)
  plan <- ithPlanConciseRubricReview(review, conciseRubric, protectedRuleIds = protectedRuleIds)
  hasIssues <- nzchar(plan$issues)

  edited <- NULL
  if (!any(hasIssues) && any(plan$accepted)) {
    # New rules need LLM concept-set inference; a failure here is reported like any other issue.
    edited <- tryCatch(
      ithBuildReviewedConciseRubric(conciseRubric, plan$plans, conditionName, llmClient),
      error = function(e) {
        plan$issues[plan$accepted] <<- paste("Could not build the edited rubric:", conditionMessage(e))
        NULL
      }
    )
    hasIssues <- nzchar(plan$issues)
  }

  writeIssues <- function() {
    review$issues <- plan$issues
    utils::write.csv(review, reviewPath, row.names = FALSE, na = "")
    message("no changes were made, there were issues with the reviewed changes")
    message("See the 'issues' column in: ", reviewPath)
    invisible(list(status = "issues", reviewPath = reviewPath, issues = review[hasIssues, , drop = FALSE]))
  }

  if (any(hasIssues)) {
    return(writeIssues())
  }
  if (!any(plan$accepted)) {
    message("No accepted rows in ", reviewPath, "; nothing to apply.")
    return(invisible(list(status = "nothingAccepted", reviewPath = reviewPath, issues = review[0, , drop = FALSE])))
  }

  ithMaybeMessage(verbose, "Applying ", sum(plan$accepted), " accepted review row(s) for condition: ", conditionName)
  editedPath <- tempfile(fileext = ".csv")
  on.exit(unlink(editedPath), add = TRUE)
  utils::write.csv(edited, editedPath, row.names = FALSE)

  shortVersionBefore <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_shortRubric.rds"))
  applyError <- NULL
  tryCatch(
    ithApplyConciseRubricEdits(
      conditionName = conditionName,
      outputDirectory = outputDirectory,
      editedConciseRubricPath = editedPath,
      llmClient = llmClient,
      promptDir = promptDir,
      updateSummary = updateSummary,
      verbose = verbose,
      refreshConceptSets = TRUE,
      adjustableRuleId = protectedRuleIds
    ),
    error = function(e) applyError <<- conditionMessage(e)
  )

  if (!is.null(applyError)) {
    shortVersionAfter <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_shortRubric.rds"))
    if (!identical(shortVersionBefore, shortVersionAfter)) {
      stop("Rubric files were saved (", shortVersionAfter, ") but a later step failed: ", applyError)
    }
    plan$issues[plan$accepted] <- paste("Apply failed:", applyError)
    hasIssues <- nzchar(plan$issues)
    return(writeIssues())
  }

  ithMaybeMessage(verbose, "Applied review ", basename(reviewPath), " for condition: ", conditionName)
  # Mark applied rows so a later run does not apply them again.
  appliedTo <- ithCoalesce(ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv")), "applied")
  if (!("applied" %in% names(review))) {
    review$applied <- ""
  }
  review$applied[plan$accepted] <- appliedTo
  utils::write.csv(review, reviewPath, row.names = FALSE, na = "")
  invisible(list(status = "applied", reviewPath = reviewPath, issues = review[0, , drop = FALSE]))
}

#' Apply accepted review findings for every condition in a config
#'
#' Runs [applyConciseRubricReview()] on the latest `<condition>_conciseRubricReview_V*.csv` of each
#' condition in the config. Conditions without a review file are skipped, and an error in one
#' condition does not stop the others.
#'
#' @param conditionNames Optional character vector of condition names (`rubricName` or
#'   `testCondition`, case-insensitive). When `NULL`, all configured conditions are processed.
#' @param configPath Path to YAML configuration file.
#' @param updateSummary Logical; regenerate the concise rubric summary text for applied conditions.
#'
#' @return Data frame with one row per condition: `conditionName` and `status` (`applied`,
#'   `issues`, `nothingAccepted`, `skipped: ...`, or `error: ...`).
#' @examples
#' \dontrun{
#' results <- applyConciseRubricReviewsFromConfig()
#' results <- applyConciseRubricReviewsFromConfig("Major Depressive Disorder")
#' }
#' @export
applyConciseRubricReviewsFromConfig <- function(conditionNames = NULL,
                                                configPath = "config.yaml",
                                                updateSummary = TRUE) {
  ithCheckConditionNamesNotConfig(conditionNames)
  config <- ithReadIthacaConditionConfig(configPath)
  if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
    stop("outputDirectory is not defined in config: ", configPath)
  }
  outputDirectory <- as.character(config$outputDirectory[[1]])
  verbose <- isTRUE(ithCoalesce(config$verbose, TRUE))
  llmClient <- ithGetLlmClientFromConfig(config, "reasoning")

  selectedNames <- vapply(
    config$conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    missingNames <- conditionNames[!tolower(trimws(conditionNames)) %in% tolower(selectedNames)]
    if (length(missingNames) > 0) {
      stop("Requested conditionNames are not configured: ", paste(missingNames, collapse = ", "),
           "\n  Configured conditions: ", paste(selectedNames, collapse = ", "), call. = FALSE)
    }
    selectedNames <- selectedNames[tolower(selectedNames) %in% tolower(trimws(conditionNames))]
  }

  results <- lapply(selectedNames, function(conditionName) {
    statusRow <- function(status) data.frame(conditionName = conditionName, status = status, stringsAsFactors = FALSE)
    if (is.null(ithFindLatestReviewFile(file.path(outputDirectory, conditionName), conditionName))) {
      return(statusRow("skipped: no review file found"))
    }
    tryCatch({
      out <- applyConciseRubricReview(
        conditionName = conditionName,
        outputDirectory = outputDirectory,
        configPath = configPath,
        llmClient = llmClient,
        promptDir = ithCoalesce(config$promptDir, ithPromptDir()),
        updateSummary = updateSummary,
        verbose = verbose
      )
      statusRow(out$status)
    }, error = function(e) statusRow(paste("error:", conditionMessage(e))))
  })

  results <- do.call(rbind, results)
  if (any(results$status == "issues")) {
    message("Some conditions had issues; see the 'issues' column in their latest <condition>_conciseRubricReview_V*.csv.")
  }
  results
}

#' Apply pending review findings and concise rubric edits for every condition in a config
#'
#' For each condition in the config (including `conditionSpecsCsvPath`):
#' 1. Applies accepted, not-yet-applied rows of the latest `<condition>_conciseRubricReview_V*.csv`
#'    with [applyConciseRubricReview()]. That apply starts from the latest concise rubric CSV, so
#'    hand edits in the CSV are applied with it.
#' 2. Compares the latest concise rubric CSV with the short rubric and runs
#'    [applyConciseRubricEdits()] when they still differ.
#' Step 2 is skipped when step 1 reports issues so the review can be fixed first. An error in one
#' condition does not stop the others.
#'
#' @param conditionNames Optional character vector of condition names (`rubricName` or
#'   `testCondition`, case-insensitive). When `NULL`, all configured conditions are processed.
#' @param configPath Path to YAML configuration file.
#' @param updateSummary Logical; regenerate the concise rubric summary text for applied conditions.
#'
#' @return Data frame with one row per condition: `conditionName`, `reviewStatus` (`applied`,
#'   `issues`, `nothingAccepted`, `no review file`, or `error: ...`), and `conciseStatus`
#'   (`applied (<changes>)`, `unchanged`, `no concise rubric`, `skipped: ...`, or `error: ...`).
#' @examples
#' \dontrun{
#' results <- applyPendingRubricChangesFromConfig()
#' results <- applyPendingRubricChangesFromConfig("Major Depressive Disorder")
#' }
#' @export
applyPendingRubricChangesFromConfig <- function(conditionNames = NULL,
                                                configPath = "config.yaml",
                                                updateSummary = TRUE) {
  ithCheckConditionNamesNotConfig(conditionNames)
  config <- ithReadIthacaConditionConfig(configPath)
  if (is.null(config$outputDirectory) || !nzchar(as.character(config$outputDirectory[[1]]))) {
    stop("outputDirectory is not defined in config: ", configPath)
  }
  outputDirectory <- as.character(config$outputDirectory[[1]])
  verbose <- isTRUE(ithCoalesce(config$verbose, TRUE))
  llmClient <- ithGetLlmClientFromConfig(config, "reasoning")
  promptDir <- ithCoalesce(config$promptDir, ithPromptDir())

  selectedNames <- vapply(
    config$conditionSpecs,
    function(spec) as.character(ithCoalesce(spec$rubricName, spec$testCondition)[[1]]),
    character(1)
  )
  if (!is.null(conditionNames)) {
    missingNames <- conditionNames[!tolower(trimws(conditionNames)) %in% tolower(selectedNames)]
    if (length(missingNames) > 0) {
      stop("Requested conditionNames are not configured: ", paste(missingNames, collapse = ", "),
           "\n  Configured conditions: ", paste(selectedNames, collapse = ", "), call. = FALSE)
    }
    selectedNames <- selectedNames[tolower(selectedNames) %in% tolower(trimws(conditionNames))]
  }

  results <- lapply(selectedNames, function(conditionName) {
    conditionDir <- file.path(outputDirectory, conditionName)
    statusRow <- function(reviewStatus, conciseStatus) {
      data.frame(conditionName = conditionName, reviewStatus = reviewStatus, conciseStatus = conciseStatus, stringsAsFactors = FALSE)
    }

    reviewStatus <- if (is.null(ithFindLatestReviewFile(conditionDir, conditionName))) {
      "no review file"
    } else {
      tryCatch(
        applyConciseRubricReview(
          conditionName = conditionName,
          outputDirectory = outputDirectory,
          configPath = configPath,
          llmClient = llmClient,
          promptDir = promptDir,
          updateSummary = updateSummary,
          verbose = verbose
        )$status,
        error = function(e) paste("error:", conditionMessage(e))
      )
    }
    if (identical(reviewStatus, "issues") || startsWith(reviewStatus, "error")) {
      return(statusRow(reviewStatus, "skipped: fix the review first"))
    }

    latestConcise <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
    if (is.null(latestConcise)) {
      return(statusRow(reviewStatus, "no concise rubric"))
    }
    conciseStatus <- tryCatch({
      concisePath <- file.path(conditionDir, latestConcise)
      changes <- ithDiffConciseRubric(
        utils::read.csv(concisePath, stringsAsFactors = FALSE),
        ithLoadLatestShortRubric(conditionName, outputDirectory)
      )
      if (!isTRUE(changes$differs)) {
        "unchanged"
      } else {
        changed <- setdiff(names(changes)[vapply(changes, isTRUE, logical(1))], "differs")
        applyConciseRubricEdits(
          conditionName = conditionName,
          outputDirectory = outputDirectory,
          editedConciseRubricPath = concisePath,
          llmClient = llmClient,
          promptDir = promptDir,
          updateSummary = updateSummary && (changes$rulesChanged || changes$criteriaChanged || changes$conceptSetsChanged),
          verbose = verbose,
          adjustableRuleId = ithFindConfigConditionSpec(config, conditionName)$adjustableRuleId
        )
        paste0("applied (", paste(changed, collapse = ", "), ")")
      }
    }, error = function(e) paste("error:", conditionMessage(e)))

    statusRow(reviewStatus, conciseStatus)
  })

  results <- do.call(rbind, results)
  if (any(results$reviewStatus == "issues")) {
    message("Some conditions had review issues; see the 'issues' column in their latest <condition>_conciseRubricReview_V*.csv.")
  }
  results
}
