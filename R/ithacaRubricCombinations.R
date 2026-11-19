#' List the smallest rule combinations that reach a rubric's YES score
#'
#' Reads the latest `<condition>_conciseRubric_V#.csv` and lists every combination of positive
#' evidence rules whose points reach the rubric's YES threshold without any extra rule: removing
#' any one rule from a listed combination drops the total below YES. For example, with YES = 10,
#' ER9 + ER10 + ER11 (5 + 3 + 4 = 12) is listed, but adding a fourth rule to it is not.
#' Alternative explanations, hard exclusions, and other negative or zero-point rules are not used.
#' Each combination also shows its evidence categories (from the latest `rubricList_V#.rds`;
#' rules added by later edits show `unknown`) and how many distinct known categories it spans.
#'
#' The report is saved as `<condition>_yesRuleCombinations_V#.csv` in the condition folder. It is
#' also created automatically after a rubric is created or changed (rubric creation, concise
#' rubric edits, review apply, `adjustRubric()`); set `options(ithaca.yesCombinationReport = FALSE)`
#' to turn that off.
#'
#' @param conditionName Condition folder name under `outputDirectory`.
#' @param outputDirectory Rubric output root folder.
#' @param maxCombinations Stop with an error when a rubric has more combinations than this.
#' @param verbose Print progress messages.
#'
#' @return Invisibly, a data frame with one row per combination; the saved file path is in the
#'   `path` attribute.
#'
#' @examples
#' \dontrun{
#' createRubricYesCombinationReport(
#'   conditionName = "Multiple Myeloma",
#'   outputDirectory = "P:/llm/rubricsPears2"
#' )
#' }
#' @export
createRubricYesCombinationReport <- function(conditionName,
                                             outputDirectory,
                                             maxCombinations = 100000,
                                             verbose = TRUE) {
  conditionDir <- file.path(outputDirectory, conditionName)
  conciseName <- ithFindLatestVersion(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
  if (is.null(conciseName)) {
    stop("No concise rubric found for '", conditionName, "' in ", conditionDir, call. = FALSE)
  }
  concise <- utils::read.csv(file.path(conditionDir, conciseName), stringsAsFactors = FALSE)
  ithMaybeMessage(verbose, "Finding YES rule combinations in ", conciseName)

  categories <- ithRubricRuleCategories(ithLoadLatest(conditionDir, "rubricList.rds"))
  types <- vapply(as.character(concise$type), ithNormalizeReviewRuleType, character(1), USE.NAMES = FALSE)
  scores <- suppressWarnings(as.numeric(concise$score))

  report <- NULL
  for (rubricId in unique(as.character(concise$rubric))) {
    rubricRows <- concise[as.character(concise$rubric) == rubricId, , drop = FALSE]
    yesScore <- suppressWarnings(as.numeric(unique(rubricRows$rubricYesScore)))
    if (length(yesScore) != 1L || is.na(yesScore)) {
      stop("Rubric ", rubricId, " for '", conditionName, "' does not have one YES score.", call. = FALSE)
    }
    keep <- as.character(concise$rubric) == rubricId & types %in% "evidenceRules" & !is.na(scores) & scores > 0
    rules <- data.frame(
      ruleId = toupper(trimws(as.character(concise$ruleId[keep]))),
      score = scores[keep],
      criteria = as.character(concise$criteria[keep]),
      stringsAsFactors = FALSE
    )
    rules$category <- unname(categories[paste(rubricId, rules$ruleId)])
    rules$category[is.na(rules$category) | !nzchar(rules$category)] <- "unknown"

    combos <- ithMinimalYesCombinations(rules$score, yesScore, maxCombinations, label = paste(conditionName, rubricId))
    ithMaybeMessage(
      verbose, "Rubric ", rubricId, ": ", length(combos), " combination(s) of ", nrow(rules),
      " positive evidence rule(s) reach YES = ", yesScore, "."
    )
    if (length(combos) == 0L) {
      next
    }

    ruleOrder <- ithRuleIdOrder(rules$ruleId)
    rows <- lapply(combos, function(index) {
      index <- index[order(ruleOrder[index])]
      comboCategories <- unique(rules$category[index])
      knownCategories <- setdiff(comboCategories, "unknown")
      data.frame(
        condition = conditionName,
        rubric = rubricId,
        ruleIds = paste(rules$ruleId[index], collapse = "; "),
        criteria = paste0(rules$ruleId[index], ": ", rules$criteria[index], collapse = " | "),
        ruleScores = paste(rules$score[index], collapse = " + "),
        categories = paste(sort(comboCategories), collapse = "; "),
        distinctCategories = length(knownCategories),
        includesDiagnosisRules = any(rules$ruleId[index] %in% c("ER1", "ER2")),
        rubricYesScore = yesScore,
        ruleCount = length(index),
        totalScore = sum(rules$score[index]),
        pointsOverYes = sum(rules$score[index]) - yesScore,
        stringsAsFactors = FALSE
      )
    })
    rubricReport <- do.call(rbind, rows)
    rubricReport <- rubricReport[order(!rubricReport$includesDiagnosisRules, rubricReport$ruleCount, rubricReport$totalScore, rubricReport$ruleIds), , drop = FALSE]
    report <- rbind(report, rubricReport)
  }

  if (is.null(report)) {
    message("No rule combinations reach the YES score for '", conditionName, "'; nothing saved.")
    return(invisible(report))
  }
  rownames(report) <- NULL
  path <- ithSaveVersioned(conditionDir, report, paste0(conditionName, "_yesRuleCombinations.csv"))
  ithMaybeMessage(verbose, "Saved YES rule combinations: ", path)
  attr(report, "path") <- path
  invisible(report)
}

# Runs createRubricYesCombinationReport() after a rubric is created or changed. Turn off with
# options(ithaca.yesCombinationReport = FALSE). Failures are warnings so they never stop the
# rubric save that triggered the report.
ithRunYesCombinationReport <- function(conditionName, outputDirectory, verbose = TRUE) {
  if (!isTRUE(getOption("ithaca.yesCombinationReport", TRUE))) {
    return(invisible(NULL))
  }
  tryCatch(
    createRubricYesCombinationReport(conditionName, outputDirectory, verbose = verbose),
    error = function(e) {
      warning(
        "YES rule combinations report not created for '", conditionName, "': ", conditionMessage(e),
        call. = FALSE
      )
      invisible(NULL)
    }
  )
}

# Indexes (into scores) of every combination with sum >= yesScore where dropping any one rule
# falls below yesScore. Scores must be positive.
ithMinimalYesCombinations <- function(scores, yesScore, maxCombinations = 100000, label = "rubric") {
  order <- order(-scores)
  sorted <- scores[order]
  remaining <- rev(cumsum(rev(sorted)))
  found <- list()

  search <- function(i, chosen, total) {
    if (total >= yesScore) {
      # Rules are added largest first, so the last one added is the smallest.
      if (total - sorted[[chosen[[length(chosen)]]]] < yesScore) {
        if (length(found) >= maxCombinations) {
          stop(
            "More than ", maxCombinations, " YES rule combinations for ", label,
            "; raise maxCombinations to list them all.",
            call. = FALSE
          )
        }
        found[[length(found) + 1L]] <<- order[chosen]
      }
      return(invisible(NULL))
    }
    if (i > length(sorted) || total + remaining[[i]] < yesScore) {
      return(invisible(NULL))
    }
    search(i + 1L, c(chosen, i), total + sorted[[i]])
    search(i + 1L, chosen, total)
  }
  search(1L, integer(0), 0)
  found
}

# Named vector "R1 ER3" -> category from a rubricList_V#.rds object; empty when unavailable.
ithRubricRuleCategories <- function(rubricObject) {
  items <- if (is.list(rubricObject) && !is.null(rubricObject$rubricList)) rubricObject$rubricList else rubricObject
  out <- character()
  for (item in items) {
    rules <- item$evidenceRules
    if (!is.data.frame(rules) || !all(c("ruleId", "category") %in% names(rules))) {
      next
    }
    rubricId <- as.character(item$rubricId[[1]])
    out[paste(rubricId, toupper(trimws(as.character(rules$ruleId))))] <- as.character(rules$category)
  }
  out
}
