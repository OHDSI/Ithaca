ithIsNumericCsv <- function(x) {
  grepl("^\\s*\\d+\\s*(,\\s*\\d+\\s*)*$", x)
}

ithComputeSingleProportion <- function(i, x, n) {
  prop.test(x[i], n[i])$conf.int
}

ithComputeProportions <- function(x, n, name) {
  cis <- lapply(seq_along(x), ithComputeSingleProportion, x = x, n = n)
  data.frame(
    metric = name,
    x = x,
    n = n,
    proportion = x / n,
    lower = vapply(cis, function(ci) ci[[1]], numeric(1)),
    upper = vapply(cis, function(ci) ci[[2]], numeric(1)),
    stringsAsFactors = FALSE
  )
}

ithFlattenRubric <- function(rubric) {
  flatRubric <- NULL

  for (rubricUp in seq_along(rubric)) {
    item <- rubric[[rubricUp]]

    hardExclusions <- item$hardExclusions[, c("ruleId", "rule_type", "criteria")]
    hardExclusions$points <- -100

    alt <- item$alternativeExplanations[, c("ruleId", "rule_type", "criteria", "points")]
    evidence <- item$evidenceRules[, c("ruleId", "rule_type", "criteria", "points")]

    rules <- rbind(hardExclusions, alt, evidence)
    rules$phenotype <- item$phenotype[[1]]
    rules$rubricId <- item$rubricId[[1]]
    rules$yesScore <- item$decisionThresholds$yes[[1]]
    rules$noScore <- item$decisionThresholds$no[[1]]
    flatRubric <- rbind(flatRubric, rules)
  }

  flatRubric
}

## Legacy helper utilities retained for compatibility

#' Derive rubric-level assessment labels
#'
#' @param total Numeric total score.
#' @param yesScore Numeric threshold for case.
#' @param noScore Numeric threshold for non-case.
#'
#' @return Character vector with `case`, `nonCase`, or `insufficient`.
#' @examples
#' deriveRubricAssessment(total = c(5, 1, 3), yesScore = c(5, 5, 5), noScore = c(2, 2, 2))
#' @export
deriveRubricAssessment <- function(total, yesScore, noScore) {
  data.table::fcase(
    is.na(total) | is.na(yesScore) | is.na(noScore), "insufficient",
    total >= yesScore, "case",
    total <= noScore, "nonCase",
    default = "insufficient"
  )
}

#' Classify a subject into a 4-level outcome
#'
#' @param nCase Number of rubric-level case classifications.
#' @param nNonCase Number of rubric-level non-case classifications.
#' @param nInsufficient Number of rubric-level insufficient classifications.
#' @param nTotal Total rubric count.
#' @param densityCaseThreshold Evidence density threshold for high-confidence case.
#' @param densityNonCaseThreshold Evidence density threshold for likely non-case.
#' @param positiveRateCaseThreshold Positive-rate threshold for high-confidence case.
#' @param positiveRatePossibleThreshold Positive-rate threshold for possible case.
#' @param positiveRateNonCaseThreshold Positive-rate threshold for likely non-case.
#' @param minCaseN Minimum case count for high-confidence case.
#' @param minNonCaseN Minimum non-case count for likely non-case.
#' @param minNetCase Minimum net case score.
#' @param maxNetNonCase Maximum net non-case score.
#'
#' @return Named list containing classification metrics and `subjectAssessment`.
#' @examples
#' classifySubject4Level(nCase = 3, nNonCase = 0, nInsufficient = 1, nTotal = 4)
#' @export
classifySubject4Level <- function(
    nCase,
    nNonCase,
    nInsufficient,
    nTotal,
    densityCaseThreshold = 0.50,
    densityNonCaseThreshold = 0.50,
    positiveRateCaseThreshold = 0.75,
    positiveRatePossibleThreshold = 0.75,
    positiveRateNonCaseThreshold = 0.25,
    minCaseN = 2,
    minNonCaseN = 2,
    minNetCase = 2,
    maxNetNonCase = -2
) {
  nObserved <- nCase + nNonCase
  evidenceDensity <- ifelse(nTotal > 0, nObserved / nTotal, NA_real_)
  positiveRate <- ifelse(nObserved > 0, nCase / nObserved, NA_real_)
  netScore <- nCase - nNonCase

  subjectAssessment <- data.table::fcase(
    nCase >= minCaseN & evidenceDensity >= densityCaseThreshold &
      !is.na(positiveRate) & positiveRate >= positiveRateCaseThreshold & netScore >= minNetCase,
    "highConfidenceCase",
    nCase >= 1 & evidenceDensity < densityCaseThreshold &
      !is.na(positiveRate) & positiveRate >= positiveRatePossibleThreshold,
    "possibleCase",
    nNonCase >= minNonCaseN & evidenceDensity >= densityNonCaseThreshold &
      !is.na(positiveRate) & positiveRate <= positiveRateNonCaseThreshold & netScore <= maxNetNonCase,
    "likelyNonCase",
    default = "indeterminate"
  )

  list(
    nObserved = nObserved,
    evidenceDensity = evidenceDensity,
    positiveRate = positiveRate,
    netScore = netScore,
    subjectAssessment = subjectAssessment
  )
}

#' Score subject outcomes from rubric-level totals
#'
#' @param df Data frame/data.table with rubric-level totals and thresholds.
#' @param subjectCol Subject ID column name.
#' @param rubricCol Rubric ID column name.
#' @param cohortCol Cohort ID column name.
#' @param startDateCol Cohort start date column name.
#' @param totalCol Total score column name.
#' @param yesCol Yes-threshold column name.
#' @param noCol No-threshold column name.
#'
#' @return List with `rubricLevel` and `subjectLevel` data.tables.
#' @examples
#' df <- data.frame(
#'   subjectId = c(1, 1),
#'   rubricId = c("R1", "R2"),
#'   cohortDefinitionId = c(10, 10),
#'   cohortStartDate = as.Date(c("2020-01-01", "2020-01-01")),
#'   total = c(5, 1),
#'   yesScore = c(5, 5),
#'   noScore = c(2, 2)
#' )
#' scoreSubjectsFromRubricTable(df)
#' @export
scoreSubjectsFromRubricTable <- function(
    df,
    subjectCol = "subjectId",
    rubricCol = "rubricId",
    cohortCol = "cohortDefinitionId",
    startDateCol = "cohortStartDate",
    totalCol = "total",
    yesCol = "yesScore",
    noCol = "noScore"
) {
  data.table::setDT(df)

  neededCols <- c(subjectCol, rubricCol, cohortCol, startDateCol, totalCol, yesCol, noCol)
  missingCols <- setdiff(neededCols, names(df))
  if (length(missingCols) > 0) {
    stop("Missing columns: ", paste(missingCols, collapse = ", "))
  }

  rubricLevel <- data.table::copy(df)
  rubricLevel[, rubricAssessment := deriveRubricAssessment(get(totalCol), get(yesCol), get(noCol))]
  rubricLevel[, rubricCaseNumeric := data.table::fcase(
    rubricAssessment == "case", 1L,
    rubricAssessment == "nonCase", -1L,
    default = 0L
  )]

  subjectLevel <- rubricLevel[
    , .(
      nCase = sum(rubricAssessment == "case", na.rm = TRUE),
      nNonCase = sum(rubricAssessment == "nonCase", na.rm = TRUE),
      nInsufficient = sum(rubricAssessment == "insufficient", na.rm = TRUE),
      nTotal = .N
    ),
    by = c(cohortCol, subjectCol, startDateCol)
  ]

  subjectLevel[
    , c("nObserved", "evidenceDensity", "positiveRate", "netScore", "subjectAssessment") :=
      classifySubject4Level(
        nCase = nCase,
        nNonCase = nNonCase,
        nInsufficient = nInsufficient,
        nTotal = nTotal
      )
  ]

  list(rubricLevel = rubricLevel, subjectLevel = subjectLevel)
}

ithVectorSearchStandard <- function(term,
                                    domains = NULL,
                                    conceptClasses = NULL,
                                    limit = 10,
                                    maxRetries = 3,
                                    waitTime = 2) {
  term <- gsub("_", " ", term)
  params <- list(q = term, limit = limit)

  if (!is.null(domains)) {
    params$domain_id <- paste(domains, collapse = ",")
  }
  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }

  url <- "https://hecate.pantheon-hds.com/api/search_standard"
  for (attempt in seq_len(maxRetries)) {
    response <- tryCatch(httr::GET(url, query = params), error = function(e) NULL)
    if (!is.null(response) && httr::status_code(response) == 200) {
      data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      if (length(data) == 0) {
        return(data.frame())
      }
      merged <- dplyr::bind_rows(data$concepts) |>
        SqlRender::snakeCaseToCamelCaseNames()
      return(cbind(data.frame(searchTerm = term, stringsAsFactors = FALSE), merged))
    }
    if (attempt < maxRetries) {
      Sys.sleep(waitTime)
    }
  }
  stop("All attempts failed for term: ", term)
}

ithCheckForExclusions <- function(term, llmClient = ithGetLlmClientFromConfig(list(), "reasoning")) {
  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    term = ellmer::type_string(),
    yesNo = ellmer::type_enum(values = c("YES", "NO")),
    rationale = ellmer::type_string(),
    exclusions = ellmer::type_string()
  ))

  prompt <- paste0(
    "Does the term: ", term, " EXPLICITLY suggest there should be exclusions? ",
    "If yes, create a comprehensive set of excluded terms without billing codes."
  )

  ithQueryLlm(
    llmClient = llmClient,
    prompt = prompt,
    output = "list",
    ellmerTypeObject = ellmerTypeObject
  )
}

#' Vector search helper over Hecate standard concepts
#'
#' @param term Search term.
#' @param domains Optional domain filters.
#' @param conceptClasses Optional concept class filters.
#' @param limit Maximum row count.
#' @param maxRetries Retry count.
#' @param waitTime Delay between retries (seconds).
#'
#' @return Data frame of matched concepts.
#' @examples
#' \dontrun{
#' out <- vectorSearchStandard("heart failure", domains = c("Condition"), limit = 5)
#' }
#' @export
vectorSearchStandard <- function(term, domains = NULL, conceptClasses = NULL, limit = 10, maxRetries = 3, waitTime = 2) {
  ithVectorSearchStandard(
    term = term,
    domains = domains,
    conceptClasses = conceptClasses,
    limit = limit,
    maxRetries = maxRetries,
    waitTime = waitTime
  )
}

#' Check whether exclusions are implied by a term
#'
#' @param term Clinical term text.
#'
#' @return Structured exclusion recommendation from the LLM.
#' @examples
#' \dontrun{
#' ex <- checkForExclusions("nonrheumatic valve disorder")
#' }
#' @export
checkForExclusions <- function(term) {
  ithCheckForExclusions(term)
}

#' Flatten rubric structures into row-wise rule records
#'
#' @param rubric Rubric list object.
#'
#' @return Data frame with one row per rule.
#' @examples
#' \dontrun{
#' flat <- flattenRubric(rubric = readRDS("P:/llm/rubrics/GERD/rubricList_V1.rds")$rubricList)
#' }
#' @export
flattenRubric <- function(rubric) {
  ithFlattenRubric(rubric)
}

#' Compute a single binomial confidence interval
#'
#' @param i Row index.
#' @param x Numerator vector.
#' @param n Denominator vector.
#'
#' @return Length-2 numeric confidence interval.
#' @examples
#' computeSingleProportion(1, x = c(10, 20), n = c(100, 200))
#' @export
computeSingleProportion <- function(i, x, n) {
  ithComputeSingleProportion(i, x, n)
}

#' Compute proportions and confidence intervals
#'
#' @param x Numerator vector.
#' @param n Denominator vector.
#' @param name Metric name label.
#'
#' @return Data frame with proportion and confidence bounds.
#' @examples
#' computeProportions(x = c(10, 20), n = c(100, 200), name = "ppv")
#' @export
computeProportions <- function(x, n, name) {
  ithComputeProportions(x, n, name)
}

#' Validate comma-separated numeric input strings
#'
#' @param s Input string.
#'
#' @return Logical scalar.
#' @examples
#' isCommaSeparatedNumbersRegex("1, 2, 3")
#' @export
isCommaSeparatedNumbersRegex <- function(s) {
  ithIsNumericCsv(s)
}

