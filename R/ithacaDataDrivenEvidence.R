# Data-driven rule evidence: runs every rubric rule on constructed populations in several databases and
# compares hit rates (likelihood ratios). See docs/RubricImprovementProcess.md.
#
# POSITIVE: ER2's SQL on a base cohort of each person's first diagnosis date.
# NEGATIVE: ER2's SQL with its condition concept set swapped for the relevant-diagnosis set, on a base
#   cohort of each person's first differential diagnosis, for people who never have the condition code.
# FAILED ER2: people with the diagnosis who do not meet ER2 (outside the cohort definition).
# The three populations never share a person, so each rule runs once on their union.

# Points for a log likelihood ratio: scaleMax points at a ratio of capLr, 0 at 1, limited to +/-scaleMax.
ithDataDrivenScaleScore <- function(logLr, scaleMax = 6, capLr = 10, digits = 0L) {
  round(pmax(-scaleMax, pmin(scaleMax, scaleMax * logLr / log(capLr))), digits)
}

# Renders a rule's SQL for the database, resolves its concept-set placeholders, and translates it.
# Returns list(sql, unresolved) where unresolved names sets that fell back to a dummy concept.
ithDataDrivenRenderRuleSql <- function(sql, database, cohortTable, cohortId, conceptSets) {
  sql <- gsub("(?s)--[^\n]*|/\\*.*?\\*/", " ", as.character(sql), perl = TRUE)
  plugins <- unique(stringr::str_extract_all(sql, "\\B@[A-Za-z0-9_]+")[[1]])
  plugins <- plugins[!(plugins %in% c("@databaseSchema", "@cohortSchema", "@cohortTable", "@cohortId"))]
  plugins <- plugins[order(-nchar(plugins))]
  if (length(plugins) > 0L && !is.null(conceptSets$error)) {
    stop("concept sets could not be loaded: ", conceptSets$error, call. = FALSE)
  }
  rendered <- suppressWarnings(SqlRender::render(
    sql, databaseSchema = database$cdmDatabaseSchema, cohortSchema = database$workDatabaseSchema,
    cohortTable = cohortTable, cohortId = cohortId
  ))
  unresolved <- character(0)
  for (plugin in plugins) {
    detail <- ithResolveConceptSetPluginDetails(
      substr(plugin, 2, nchar(plugin)), matchedConceptSets = conceptSets$matched,
      conceptSetJsonLibrary = conceptSets$library, database = database
    )
    if (!grepl("^resolved", as.character(detail$status))) {
      unresolved <- c(unresolved, paste0(substr(plugin, 2, nchar(plugin)), "(", detail$status, ")"))
    }
    rendered <- gsub(plugin, detail$value, rendered, fixed = TRUE)
  }
  list(sql = SqlRender::translate(rendered, database$connectionDetails$dbms), unresolved = unresolved)
}

ithDataDrivenStripSemicolon <- function(sql) trimws(sub(";\\s*$", "", trimws(sql)))

# Hit rates in the positive and negative populations with the likelihood ratio and its Katz interval
# (0.5 added to zero cells). direction: +1 when the rule should be more common in the positives.
ithDataDrivenCompareRates <- function(hitsPos, nPos, hitsNeg, nNeg, direction) {
  if (hitsPos == 0 && hitsNeg == 0) {
    return(data.frame(
      proportionPositive = 0, proportionNegative = 0, riskDifference = 0, likelihoodRatio = NA_real_,
      lrLower = NA_real_, lrUpper = NA_real_, logLr = NA_real_, direction = direction, verdict = "no hits",
      stringsAsFactors = FALSE
    ))
  }
  pPos <- hitsPos / nPos
  pNeg <- hitsNeg / nNeg
  a <- ifelse(hitsPos == 0, 0.5, hitsPos)
  b <- ifelse(hitsNeg == 0, 0.5, hitsNeg)
  logLr <- log((a / nPos) / (b / nNeg))
  se <- sqrt(1 / a - 1 / nPos + 1 / b - 1 / nNeg)
  lower <- exp(logLr - 1.96 * se)
  upper <- exp(logLr + 1.96 * se)
  fits <- if (lower > 1) 1L else if (upper < 1) -1L else 0L
  verdict <- if (fits == 0L) "inconclusive" else if (fits == direction) "supports" else "refutes"
  data.frame(
    proportionPositive = pPos, proportionNegative = pNeg, riskDifference = pPos - pNeg,
    likelihoodRatio = exp(logLr), lrLower = lower, lrUpper = upper, logLr = logLr, direction = direction,
    verdict = verdict, stringsAsFactors = FALSE
  )
}

# Runs one SQL task: list(kind = "exec" or "count", sql, connectionDetails, connection). Used in this
# session (with its connection) and on workers (one connection per worker, kept for reuse); returns
# list(ok, value, error, seconds) instead of raising.
ithDataDrivenRunSqlTask <- function(task) {
  startedAt <- Sys.time()
  outcome <- tryCatch({
    connection <- task[["connection"]]
    if (is.null(connection)) {
      state <- get0("ithScoringWorkerState", envir = globalenv(), inherits = FALSE)
      if (is.null(state)) state <- list(connections = list())
      key <- if (is.null(task[["connectionKey"]])) "probe" else paste0("probe_", task[["connectionKey"]])
      connection <- state$connections[[key]]
      if (is.null(connection)) {
        connection <- suppressMessages(DatabaseConnector::connect(task$connectionDetails))
        state$connections[[key]] <- connection
        assign("ithScoringWorkerState", state, envir = globalenv())
      }
    }
    value <- if (identical(task$kind, "count")) {
      as.numeric(DatabaseConnector::querySql(connection, task$sql, snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE)[[1]][[1]])
    } else {
      DatabaseConnector::executeSql(connection, task$sql, progressBar = FALSE, reportOverallTime = FALSE)
      NA_real_
    }
    list(ok = TRUE, value = value, error = NULL)
  }, error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e)))
  outcome$seconds <- as.numeric(difftime(Sys.time(), startedAt, units = "secs"))
  outcome
}

# Person-by-rule 0/1 matrix for one rubric from the sampled persons and hits.
ithDataDrivenBuildMatrix <- function(collected, rr, posId, negId, otherIds = integer(0)) {
  persons <- collected$persons[collected$persons$cohortId %in% c(posId, negId, otherIds), , drop = FALSE]
  key <- paste(persons$cohortId, persons$subjectId)
  hits <- collected$hits[collected$hits$cohortId %in% c(posId, negId, otherIds), , drop = FALSE]
  rowIndex <- match(paste(hits$cohortId, hits$subjectId), key)
  keep <- !is.na(rowIndex)
  hits <- hits[keep, , drop = FALSE]
  rowIndex <- rowIndex[keep]
  X <- matrix(0, nrow(persons), nrow(rr), dimnames = list(NULL, as.character(rr$ruleIdx)))
  column <- match(hits$ruleIdx, rr$ruleIdx)
  isRule <- !is.na(column)
  X[cbind(rowIndex[isRule], column[isRule])] <- 1
  flag <- function(pseudoIdx) {
    out <- numeric(nrow(persons))
    out[rowIndex[hits$ruleIdx == pseudoIdx]] <- 1
    out
  }
  list(X = X, y = as.integer(persons$cohortId == posId), er1 = flag(-2L), cohortId = persons$cohortId)
}

# PPV and sensitivity of the cohort definition (ER2) when the rubric is the judge. The cohort is the
# positive population; people outside it (differential negatives and candidates failing ER2) who reach
# YES are misses. `total` is each sampled person's rubric total.
ithDataDrivenCohortMeasures <- function(total, group, threshold) {
  yes <- total >= threshold
  inCohort <- group == "cohort"
  tp <- sum(yes & inCohort)
  missedFailed <- sum(yes & group == "failedEr2")
  missedDifferential <- sum(yes & group == "differential")
  c(
    ppv = tp / sum(inCohort),
    sensitivity = if (tp + missedFailed + missedDifferential > 0) tp / (tp + missedFailed + missedDifferential) else NA_real_,
    missedFailedEr2 = missedFailed, missedDifferential = missedDifferential
  )
}

ithDataDrivenFormatTable <- function(frame) {
  old <- options(width = 250)
  on.exit(options(old))
  utils::capture.output(print(frame, row.names = FALSE, right = FALSE))
}

# Report section: cohort PPV/sensitivity under current and proposed scores, with ER1 at its value and raised.
ithDataDrivenCohortCheckLines <- function(dbs, perDb, rules, recs, params, er1Increments = c(0, 2, 4)) {
  lines <- character(0)
  add <- function(...) lines <<- c(lines, paste0(...))
  fixedPoints <- function(name, rubricId) {
    value <- params[[name]][[rubricId]]
    if (is.null(value) || is.na(value)) 0 else value
  }
  bases <- list(current = recs$score, proposed = recs$proposedScore)
  fmt <- function(x) ifelse(is.na(x), "-", sprintf("%.1f%%", 100 * x))

  add("COHORT PPV/SENSITIVITY CHECK (ER2 = the cohort definition; the rubric is the judge)")
  add("  PPV = cohort members reaching YES / all cohort members.")
  add("  Sensitivity = cohort members reaching YES / (cohort members reaching YES + people outside the cohort reaching YES).")
  add("  Outside the cohort = differential-diagnosis negatives + people with the diagnosis who fail ER2. AE1 = 0 for everyone.")
  add("  Cohort members get ER1 + ER2; failed-ER2 candidates get ER1 if they meet it; differential negatives get neither.")
  add("  Counts are from the person sample, not weighted to database size.")
  add("")

  for (rubricId in unique(rules$rubric)) {
    yes <- rules$yesScore[rules$rubric == rubricId][[1]]
    er1 <- fixedPoints("er1Score", rubricId)
    er2 <- fixedPoints("er2Score", rubricId)
    data <- list()
    for (db in dbs) {
      oc <- perDb[[db]]$ocMatrices[[rubricId]]
      if (is.null(oc) || !any(oc$group == "cohort")) next
      data[[db]] <- oc
    }
    if (length(data) == 0L) {
      add("  ", rubricId, ": no sampled populations available.")
      next
    }
    add("Rubric ", rubricId, ": YES >= ", format(yes), "; ER1 = ", format(er1), ", ER2 = ", format(er2), ", AE1 = 0")
    for (db in names(data)) {
      oc <- data[[db]]
      add(sprintf(
        "  %s sample: cohort %s; outside: failed ER2 %s, differential %s%s", db,
        format(sum(oc$group == "cohort"), big.mark = ","), format(sum(oc$group == "failedEr2"), big.mark = ","),
        format(sum(oc$group == "differential"), big.mark = ","),
        if (isTRUE(oc$er1Assumed)) " (ER1 not tested for failed-ER2 candidates; assumed met)" else ""
      ))
    }
    add("")

    for (baseName in names(bases)) {
      scores <- stats::setNames(bases[[baseName]], as.character(recs$ruleIdx))
      rows <- list()
      for (increment in er1Increments) {
        er1Used <- er1 + increment
        for (db in names(data)) {
          oc <- data[[db]]
          s <- scores[colnames(oc$X)]
          s[is.na(s)] <- 0
          m <- ithDataDrivenCohortMeasures(as.vector(oc$X %*% s) + er2 * (oc$group == "cohort") + er1Used * oc$er1, oc$group, yes)
          rows[[length(rows) + 1L]] <- data.frame(
            scores = baseName, database = db,
            er1 = if (increment == 0) format(er1Used) else sprintf("%s (+%s)", format(er1Used), format(increment)),
            ppv = fmt(m[["ppv"]]), sensitivity = fmt(m[["sensitivity"]]),
            missedFailedEr2 = m[["missedFailedEr2"]], missedDifferential = m[["missedDifferential"]], stringsAsFactors = FALSE
          )
        }
      }
      lines <- c(lines, paste0("  ", ithDataDrivenFormatTable(do.call(rbind, rows))))
    }
    add("")
  }
  lines
}

# Share of POSITIVE and NEGATIVE persons whose summed rule scores reach YES or fall below NO. `baseline`
# adds fixed points per class (positives all meet ER2, so they start with its score).
ithDataDrivenTotals <- function(mat, scores, yesScore, noScore, baseline = c(POSITIVE = 0, NEGATIVE = 0)) {
  s <- scores[colnames(mat$X)]
  s[is.na(s)] <- 0
  total <- as.vector(mat$X %*% s) + ifelse(mat$y == 1L, baseline[["POSITIVE"]], baseline[["NEGATIVE"]])
  do.call(rbind, lapply(c(POSITIVE = 1L, NEGATIVE = 0L), function(cls) {
    inClass <- mat$y == cls
    data.frame(
      population = if (cls == 1L) "POSITIVE" else "NEGATIVE", persons = sum(inClass),
      proportionYes = mean(total[inClass] >= yesScore), proportionBelowNo = mean(total[inClass] < noScore),
      stringsAsFactors = FALSE
    )
  }))
}

# Analysis of one database: per-rule rates and the person-level matrices for the total and PPV checks.
ithDataDrivenAnalyzeDatabase <- function(collected, rules, params) {
  uniList <- list()
  matrices <- list()
  ocMatrices <- list()
  for (rubricId in unique(rules$rubric)) {
    rr <- rules[rules$rubric == rubricId, , drop = FALSE]
    posId <- collected$popIds$positive[[rubricId]]
    negId <- collected$popIds$negative[[rubricId]]
    nPos <- collected$sizes$persons[match(posId, collected$sizes$cohortId)]
    nNeg <- collected$sizes$persons[match(negId, collected$sizes$cohortId)]
    countOf <- function(ruleIdx, cohortId) {
      value <- collected$counts$n[collected$counts$ruleIdx == ruleIdx & collected$counts$cohortId == cohortId]
      if (length(value) > 0L) value[[1]] else 0
    }
    uniList[[rubricId]] <- do.call(rbind, lapply(seq_len(nrow(rr)), function(i) {
      hitsPos <- countOf(rr$ruleIdx[[i]], posId)
      hitsNeg <- countOf(rr$ruleIdx[[i]], negId)
      direction <- if (isTRUE(rr$score[[i]] < 0) || rr$type[[i]] %in% c("hard_exclusion", "alternativeExplanations")) -1L else 1L
      cbind(
        data.frame(ruleIdx = rr$ruleIdx[[i]], hitsPositive = hitsPos, hitsNegative = hitsNeg, personsPositive = nPos, personsNegative = nNeg),
        ithDataDrivenCompareRates(hitsPos, nPos, hitsNeg, nNeg, direction)
      )
    }))

    mat <- ithDataDrivenBuildMatrix(collected, rr, posId, negId)
    matrices[[rubricId]] <- c(mat, list(ruleIdx = rr$ruleIdx))
    missedId <- collected$popIds$missed[[rubricId]]
    if (!is.null(missedId)) {
      oc <- ithDataDrivenBuildMatrix(collected, rr, posId, negId, missedId)
      oc$group <- ifelse(oc$cohortId == posId, "cohort", ifelse(oc$cohortId == negId, "differential", "failedEr2"))
      oc$er1Assumed <- !isTRUE(collected$er1Tested[[rubricId]])
      oc$er1 <- ifelse(oc$group == "cohort", 1, ifelse(oc$group == "failedEr2", if (oc$er1Assumed) 1 else oc$er1, 0))
      ocMatrices[[rubricId]] <- oc[c("X", "group", "er1", "er1Assumed")]
    }
  }
  list(
    uni = if (length(uniList) > 0L) do.call(rbind, uniList) else NULL,
    matrices = matrices, ocMatrices = ocMatrices, unresolved = collected$unresolved, errors = collected$errors, sizes = collected$sizes,
    ocErrors = collected$ocErrors
  )
}

# Proposed score per rule = mean log likelihood ratio across databases on the points scale. Every rule
# is kept. Hard exclusions, rules without hits and rules whose query failed or whose concept set did not
# resolve keep their current score and are marked `skipped`.
ithDataDrivenRecommend <- function(rules, perDb, params) {
  dbs <- names(perDb)
  rows <- lapply(seq_len(nrow(rules)), function(i) {
    idx <- rules$ruleIdx[[i]]
    key <- as.character(idx)
    score <- rules$score[[i]]
    type <- rules$type[[i]]
    uni <- lapply(perDb, function(r) if (is.null(r$uni)) NULL else r$uni[r$uni$ruleIdx == idx, , drop = FALSE])
    has <- function(u) !is.null(u) && nrow(u) > 0L && is.finite(u$logLr)
    direction <- vapply(uni, function(u) if (!has(u)) 0L else if (u$lrLower > 1) 1L else if (u$lrUpper < 1) -1L else 0L, integer(1))
    logLr <- vapply(uni, function(u) if (has(u)) u$logLr else NA_real_, numeric(1))
    errors <- unique(unlist(lapply(perDb, function(r) r$errors[[key]])))
    unresolved <- unique(unlist(lapply(perDb, function(r) r$unresolved[[key]])))
    perDbText <- paste(vapply(dbs, function(db) {
      u <- uni[[db]]
      if (is.null(u) || nrow(u) == 0L) return(paste0(db, ": no result"))
      if (is.finite(u$logLr)) sprintf("%s: LR %.2f (%.2f-%.2f), %s", db, u$likelihoodRatio, u$lrLower, u$lrUpper, u$verdict) else paste0(db, ": ", u$verdict)
    }, character(1)), collapse = "; ")
    meanLogLr <- if (all(is.na(logLr))) NA_real_ else mean(logLr, na.rm = TRUE)
    lrScore <- ithDataDrivenScaleScore(meanLogLr, params$scaleMax, params$capLr)

    recommendation <- "KEEP"
    flag <- ""
    skipped <- FALSE
    proposedType <- type
    proposedScore <- score
    if (length(errors) > 0L) {
      recommendation <- "REVIEW"; flag <- "review"; skipped <- TRUE
      reason <- paste0("a query failed: ", substr(errors[[1]], 1, 120), "; score unchanged")
    } else if (length(unresolved) > 0L) {
      recommendation <- "REVIEW"; flag <- "review"; skipped <- TRUE
      reason <- paste0("concept set not resolved (", paste(unresolved, collapse = ", "), "); score unchanged")
    } else if (identical(type, "hard_exclusion")) {
      reason <- "hard exclusion kept as is"
    } else if (is.na(lrScore)) {
      skipped <- TRUE
      reason <- "0 records found in either population; score unchanged"
    } else {
      proposedScore <- lrScore
      if (lrScore > 0) proposedType <- "evidenceRules" else if (lrScore < 0) proposedType <- "alternativeExplanations"
      recommendation <- if (!identical(proposedType, type)) "CHANGE_TYPE" else if (!isTRUE(lrScore == score)) "RESCALE" else "KEEP"
      if (recommendation == "CHANGE_TYPE") flag <- "review"
      reason <- sprintf("mean likelihood ratio %.2f (%s)", exp(meanLogLr), paste(dbs[!is.na(logLr)], collapse = " and "))
      if (lrScore == 0) reason <- paste0(reason, "; no effect, score 0 (rule kept)")
      if (any(direction == 1L) && any(direction == -1L)) reason <- paste0(reason, "; direction differs between databases")
    }
    data.frame(
      ruleIdx = idx, rubric = rules$rubric[[i]], ruleId = rules$ruleId[[i]], type = type, score = score, criteria = rules$criteria[[i]],
      recommendation = recommendation, flag = flag, skipped = skipped, proposedType = proposedType, proposedScore = proposedScore,
      lrScore = lrScore, reason = reason, evidence = perDbText, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Gate for applying the proposed scores. For every database and rubric, people in the cohort (ER2
# positives, who get ER1 + ER2 points) and differential negatives (who get neither) are totalled with the
# proposed scores. `gate` limits: positiveYesMax, positiveYesMin, negativeYesMax (shares reaching YES)
# and separationMin (cohort YES share minus negative YES share); a NULL or NA limit is not checked.
ithDataDrivenGate <- function(perDb, rules, recs, params, gate) {
  limit <- function(name) {
    value <- gate[[name]]
    if (is.null(value) || length(value) != 1L || is.na(value)) NULL else as.numeric(value)
  }
  limits <- list(
    positiveYesMax = limit("positiveYesMax"), positiveYesMin = limit("positiveYesMin"),
    negativeYesMax = limit("negativeYesMax"), separationMin = limit("separationMin")
  )
  fixedPoints <- function(name, rubricId) {
    value <- params[[name]][[rubricId]]
    if (is.null(value) || is.na(value)) 0 else value
  }
  scores <- stats::setNames(recs$proposedScore, as.character(recs$ruleIdx))
  rows <- list()
  for (db in names(perDb)) {
    for (rubricId in unique(rules$rubric)) {
      yes <- rules$yesScore[rules$rubric == rubricId][[1]]
      oc <- perDb[[db]]$ocMatrices[[rubricId]]
      row <- data.frame(
        database = db, rubric = rubricId, cohort = NA_integer_, differential = NA_integer_,
        cohortYes = NA_real_, differentialYes = NA_real_, separation = NA_real_, passed = FALSE, reason = "",
        stringsAsFactors = FALSE
      )
      if (is.null(oc) || !any(oc$group == "cohort") || !any(oc$group == "differential")) {
        row$reason <- "no sampled cohort or differential-negative people"
        rows[[length(rows) + 1L]] <- row
        next
      }
      s <- scores[colnames(oc$X)]
      s[is.na(s)] <- 0
      inCohort <- oc$group == "cohort"
      total <- as.vector(oc$X %*% s) + (fixedPoints("er1Score", rubricId) + fixedPoints("er2Score", rubricId)) * inCohort
      cohortYes <- mean(total[inCohort] >= yes)
      differentialYes <- mean(total[oc$group == "differential"] >= yes)
      failed <- character(0)
      pct <- function(x) sprintf("%.1f%%", 100 * x)
      if (!is.null(limits$positiveYesMax) && cohortYes > limits$positiveYesMax) {
        failed <- c(failed, paste0("cohort YES ", pct(cohortYes), " is above ", pct(limits$positiveYesMax)))
      }
      if (!is.null(limits$positiveYesMin) && cohortYes < limits$positiveYesMin) {
        failed <- c(failed, paste0("cohort YES ", pct(cohortYes), " is below ", pct(limits$positiveYesMin)))
      }
      if (!is.null(limits$negativeYesMax) && differentialYes > limits$negativeYesMax) {
        failed <- c(failed, paste0("differential-negative YES ", pct(differentialYes), " is above ", pct(limits$negativeYesMax)))
      }
      if (!is.null(limits$separationMin) && cohortYes - differentialYes < limits$separationMin) {
        failed <- c(failed, paste0("separation ", pct(cohortYes - differentialYes), " is below ", pct(limits$separationMin)))
      }
      row$cohort <- sum(inCohort)
      row$differential <- sum(oc$group == "differential")
      row$cohortYes <- cohortYes
      row$differentialYes <- differentialYes
      row$separation <- cohortYes - differentialYes
      row$passed <- length(failed) == 0L
      row$reason <- paste(failed, collapse = "; ")
      rows[[length(rows) + 1L]] <- row
    }
  }
  table <- do.call(rbind, rows)
  list(table = table, passed = !is.null(table) && nrow(table) > 0L && all(table$passed), limits = limits)
}

ithDataDrivenGateLines <- function(gate) {
  lines <- character(0)
  add <- function(...) lines <<- c(lines, paste0(...))
  limits <- gate$limits
  pct <- function(x) if (is.null(x)) "not checked" else sprintf("%.1f%%", 100 * x)
  add("GATE FOR APPLYING THE PROPOSED SCORES (every database and rubric must pass; otherwise nothing is applied)")
  add("  Totals use the proposed scores; cohort members get ER1 + ER2 points, differential negatives get neither; AE1 = 0.")
  add("  Limits: cohort YES at most ", pct(limits$positiveYesMax), " and at least ", pct(limits$positiveYesMin),
    "; differential-negative YES at most ", pct(limits$negativeYesMax), "; separation (cohort minus negative) at least ", pct(limits$separationMin), ".")
  if (!is.null(gate$table) && nrow(gate$table) > 0L) {
    shown <- gate$table
    for (column in c("cohortYes", "differentialYes", "separation")) shown[[column]] <- ifelse(is.na(shown[[column]]), "-", sprintf("%.1f%%", 100 * shown[[column]]))
    shown$passed <- ifelse(shown$passed, "pass", "FAIL")
    lines <- c(lines, paste0("  ", ithDataDrivenFormatTable(shown)))
  }
  add("  Result: ", if (isTRUE(gate$passed)) "passed" else "NOT passed")
  add("")
  lines
}

# Rules the data could not score (no records, failed query, unresolved concept set); they keep their score.
ithDataDrivenSkippedLines <- function(recs) {
  skipped <- recs[recs$skipped, c("rubric", "ruleId", "type", "score", "reason"), drop = FALSE]
  if (nrow(skipped) == 0L) {
    return(character(0))
  }
  c(
    "RULES NOT SCORED FROM THE DATA (score unchanged)",
    paste0("  ", ithDataDrivenFormatTable(skipped)),
    ""
  )
}

ithDataDrivenBuildReport <- function(conditionName, dbs, perDb, rules, recs, params, gate) {
  lines <- character(0)
  add <- function(...) lines <<- c(lines, paste0(...))
  add("DATA-DRIVEN RULE SCORES: ", conditionName)
  add("Databases: ", paste(dbs, collapse = ", "), "    Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
  add("")
  for (db in dbs) {
    sz <- perDb[[db]]$sizes
    add("Populations in ", db, ": ", paste0(sz$population, ifelse(is.na(sz$rubric), "", paste0(" ", sz$rubric)), "=", format(sz$persons, big.mark = ",", scientific = FALSE, trim = TRUE), collapse = "; "))
  }
  add("")
  add("SUMMARY")
  counts <- table(recs$recommendation)
  for (name in names(counts)) add("  ", name, ": ", counts[[name]], " rule(s)")
  add("")

  add("RECOMMENDATIONS (proposed score from the mean likelihood ratio; all rules kept)")
  shown <- recs[, c("ruleId", "type", "score", "recommendation", "proposedType", "proposedScore", "reason")]
  lines <- c(lines, ithDataDrivenFormatTable(shown), "")

  lines <- c(lines, ithDataDrivenSkippedLines(recs))

  add("EVIDENCE BY DATABASE")
  lines <- c(lines, ithDataDrivenFormatTable(recs[, c("ruleId", "evidence")]), "")

  lines <- c(lines, ithDataDrivenGateLines(gate))

  add("TOTAL-SCORE CHECK (tested rules only; positives start with ER2's own points, negatives with none; ER1 and AE1 are not included)")
  after <- stats::setNames(recs$proposedScore, as.character(recs$ruleIdx))
  before <- stats::setNames(recs$score, as.character(recs$ruleIdx))
  er2Points <- function(rubricId) {
    value <- params$er2Score[[rubricId]]
    if (is.null(value) || is.na(value)) 0 else value
  }
  for (db in dbs) {
    for (rubricId in unique(rules$rubric)) {
      mat <- perDb[[db]]$matrices[[rubricId]]
      if (is.null(mat)) next
      yes <- rules$yesScore[rules$rubric == rubricId][[1]]
      no <- rules$noScore[rules$rubric == rubricId][[1]]
      b <- ithDataDrivenTotals(mat, before, yes, no, c(POSITIVE = er2Points(rubricId), NEGATIVE = 0))
      a <- ithDataDrivenTotals(mat, after, yes, no, c(POSITIVE = er2Points(rubricId), NEGATIVE = 0))
      add(sprintf(
        "  %s %s (YES >= %s, NO < %s): reach YES now %.2f%% positives / %.2f%% negatives; with proposed scores %.2f%% / %.2f%%",
        db, rubricId, format(yes), format(no), 100 * b$proportionYes[1], 100 * b$proportionYes[2], 100 * a$proportionYes[1], 100 * a$proportionYes[2]
      ))
      if (a$proportionYes[1] < 0.8 * b$proportionYes[1]) {
        for (k in seq(1, 6, by = 0.1)) {
          scaled <- ithDataDrivenTotals(mat, after * k, yes, no, c(POSITIVE = er2Points(rubricId), NEGATIVE = 0))
          if (scaled$proportionYes[1] >= b$proportionYes[1]) break
        }
        add(sprintf(
          "    Calibration: the proposed scores are smaller than the current ones. Multiplying them by about %.1f keeps YES for %.2f%% of positives and gives YES to %.2f%% of negatives.",
          k, 100 * scaled$proportionYes[1], 100 * scaled$proportionYes[2]
        ))
      }
    }
  }
  add("")
  lines <- c(lines, ithDataDrivenCohortCheckLines(dbs, perDb, rules, recs, params))
  for (db in dbs) {
    ocErrors <- perDb[[db]]$ocErrors
    if (length(ocErrors) > 0L) add("  ", db, " failed-ER2 hit sets that failed (counted as no hits): ", paste(names(ocErrors), collapse = ", "))
  }
  add("")
  add("NOTES")
  add("  - Positives are people meeting ER2 around their first diagnosis, so the data show agreement with ER2, not with a clinical reference standard.")
  add("  - Negatives are people with a differential diagnosis who never have the condition; they are harder than a general sample.")
  add(
    "  - Proposed score = ", params$scaleMax, " x ln(mean likelihood ratio) / ln(", params$capLr, "), rounded and limited to +/-", params$scaleMax,
    "; the log likelihood ratio is averaged over the databases. Rules are scored independently: overlap between rules is not adjusted for."
  )
  add("  - A hard exclusion is never changed automatically.")
  lines
}

# Builds the populations and runs every rule in one database. Returns the counts, the person sample and
# its rule hits. The scratch tables are dropped when the function ends.
ithDataDrivenCollectDatabase <- function(conditionName, databaseId, cdmMap, config, rules, er2Text, conceptSets, conditionSet, relevantSet,
                                         params, cluster, say, er1Text = list()) {
  config$cdm <- cdmMap
  database <- ithResolveDatabasesFromConfig(config = config, envir = globalenv())[[databaseId]]
  dbms <- database$connectionDetails$dbms
  table <- tolower(paste0("ithacaprobe_improve_", databaseId, "_", format(Sys.time(), "%Y%m%d%H%M%S")))
  popQ <- paste0(database$workDatabaseSchema, ".", table)
  hitsQ <- paste0(popQ, "_hits")
  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  exec <- function(sql) DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  query <- function(sql) DatabaseConnector::querySql(connection, SqlRender::translate(sql, dbms), snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE)
  on.exit({
    for (name in c(popQ, hitsQ)) try(exec(SqlRender::translate(paste0("DROP TABLE IF EXISTS ", name), dbms)), silent = TRUE)
    suppressWarnings(try(DatabaseConnector::disconnect(connection), silent = TRUE))
  }, add = TRUE)

  render <- function(sql, cohortId = NA) ithDataDrivenRenderRuleSql(sql, database, table, cohortId, conceptSets)
  asOutcome <- function(value) {
    if (is.list(value) && !is.null(value$ok)) value else list(ok = FALSE, value = NULL, error = paste(as.character(value), collapse = " "), seconds = NA_real_)
  }
  runTasks <- function(tasks) {
    tasks <- lapply(tasks, function(task) {
      task$connectionDetails <- database$connectionDetails
      task$connectionKey <- databaseId
      if (is.null(cluster)) task$connection <- connection
      task
    })
    finished <- function(index, outcome) {
      say("    ", tasks[[index]]$label, if (isTRUE(outcome$ok)) "" else paste0(" FAILED - ", substr(outcome$error, 1, 120)), " (", round(outcome$seconds, 1), "s)")
    }
    if (is.null(cluster)) {
      return(lapply(seq_along(tasks), function(index) {
        outcome <- ithDataDrivenRunSqlTask(tasks[[index]])
        finished(index, outcome)
        outcome
      }))
    }
    values <- ithRunOnCluster(cluster, tasks, ithDataDrivenRunSqlTask, onResult = function(index, value, done, total) finished(index, asOutcome(value)))
    lapply(values, asOutcome)
  }
  stopOnFailed <- function(tasks, outcomes) {
    failed <- which(!vapply(outcomes, function(outcome) isTRUE(outcome$ok), logical(1)))
    if (length(failed) > 0L) {
      stop(paste0(vapply(failed, function(i) paste0(tasks[[i]]$label, " failed: ", outcomes[[i]]$error), character(1)), collapse = "\n"), call. = FALSE)
    }
  }
  insertHeader <- paste0("INSERT INTO ", popQ, " (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)\n")
  modulusFilter <- if (params$personModulus > 1L) paste0(" AND co.person_id % ", params$personModulus, " = 0") else ""

  say("Database ", databaseId, " (", database$cdmDatabaseSchema, "): building populations in ", popQ)
  exec(paste0("CREATE TABLE ", popQ, " (cohort_definition_id INT, subject_id BIGINT, cohort_start_date DATE, cohort_end_date DATE) USING DELTA"))
  exec(paste0("CREATE TABLE ", hitsQ, " (rule_idx INT, cohort_definition_id INT, subject_id BIGINT) USING DELTA"))

  baseId <- 90L
  negativeBaseId <- 91L
  baseTasks <- list()
  built <- render(paste0(
    insertHeader, "SELECT ", baseId, ", co.person_id, MIN(co.condition_start_date), MIN(co.condition_start_date)\n",
    "FROM @databaseSchema.condition_occurrence co\n", "WHERE co.condition_concept_id IN (@", conditionSet, ")", modulusFilter, "\nGROUP BY co.person_id"
  ), baseId)
  if (length(built$unresolved) > 0L) stop("Concept set not resolved: ", paste(built$unresolved, collapse = ", "), call. = FALSE)
  baseTasks[[1]] <- list(kind = "exec", sql = built$sql, label = "candidate people (first diagnosis date)")
  built <- render(paste0(
    insertHeader, "SELECT ", negativeBaseId, ", r.person_id, r.first_date, r.first_date\n",
    "FROM (SELECT co.person_id, MIN(co.condition_start_date) AS first_date\n", "      FROM @databaseSchema.condition_occurrence co\n",
    "      WHERE co.condition_concept_id IN (@", relevantSet, ")", modulusFilter, "\n", "      GROUP BY co.person_id) r\n",
    "WHERE NOT EXISTS (SELECT 1 FROM @databaseSchema.condition_occurrence c2\n",
    "                  WHERE c2.person_id = r.person_id AND c2.condition_concept_id IN (@", conditionSet, "))"
  ), negativeBaseId)
  if (length(built$unresolved) > 0L) stop("Concept set not resolved: ", paste(built$unresolved, collapse = ", "), call. = FALSE)
  baseTasks[[2]] <- list(kind = "exec", sql = built$sql, label = "candidate negatives (first differential diagnosis date, never the condition)")
  stopOnFailed(baseTasks, runTasks(baseTasks))

  rubricIds <- unique(as.character(rules$rubric))
  positiveIds <- stats::setNames(10L * seq_along(rubricIds) + 1L, rubricIds)
  negativeIds <- stats::setNames(10L * seq_along(rubricIds) + 2L, rubricIds)
  conditionPattern <- paste0("@", conditionSet, "\\b")
  populationTasks <- list()
  for (rubricId in rubricIds) {
    sqlText <- er2Text[[rubricId]]
    if (is.null(sqlText) || !grepl(conditionPattern, sqlText, ignore.case = TRUE)) {
      stop("ER2's SQL for rubric ", rubricId, " is missing or does not use @", conditionSet, call. = FALSE)
    }
    for (population in c("POSITIVE", "NEGATIVE")) {
      isPositive <- population == "POSITIVE"
      text <- if (isPositive) sqlText else gsub(conditionPattern, paste0("@", relevantSet), sqlText, ignore.case = TRUE)
      built <- render(text, if (isPositive) baseId else negativeBaseId)
      if (length(built$unresolved) > 0L) stop("ER2 concept set not resolved: ", paste(built$unresolved, collapse = ", "), call. = FALSE)
      populationTasks[[length(populationTasks) + 1L]] <- list(
        kind = "exec",
        sql = paste0(
          "INSERT INTO ", popQ, " (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)\n",
          "SELECT ", if (isPositive) positiveIds[[rubricId]] else negativeIds[[rubricId]], ", subject_id, cohort_start_date, cohort_start_date FROM (",
          ithDataDrivenStripSemicolon(built$sql), ") er2_subjects"
        ),
        label = paste0(population, " population for rubric ", rubricId)
      )
    }
  }
  stopOnFailed(populationTasks, runTasks(populationTasks))

  missedIds <- stats::setNames(10L * seq_along(rubricIds) + 3L, rubricIds)
  missedTasks <- lapply(rubricIds, function(rubricId) list(
    kind = "exec",
    sql = paste0(
      insertHeader, "SELECT ", missedIds[[rubricId]], ", c.subject_id, c.cohort_start_date, c.cohort_start_date FROM ", popQ, " c\n",
      "WHERE c.cohort_definition_id = ", baseId, " AND NOT EXISTS (SELECT 1 FROM ", popQ, " p WHERE p.cohort_definition_id = ",
      positiveIds[[rubricId]], " AND p.subject_id = c.subject_id)"
    ),
    label = paste0("OUTSIDE population for rubric ", rubricId, " (diagnosed, failing ER2)")
  ))
  stopOnFailed(missedTasks, runTasks(missedTasks))

  combinedIds <- stats::setNames(10L * seq_along(rubricIds) + 9L, rubricIds)
  memberIds <- function(rubricId) paste(positiveIds[[rubricId]], negativeIds[[rubricId]], missedIds[[rubricId]], sep = ", ")
  combinedTasks <- lapply(rubricIds, function(rubricId) list(
    kind = "exec",
    sql = paste0(
      insertHeader, "SELECT ", combinedIds[[rubricId]], ", subject_id, cohort_start_date, cohort_end_date FROM ", popQ,
      " WHERE cohort_definition_id IN (", memberIds(rubricId), ")"
    ),
    label = paste0("combined population for rubric ", rubricId)
  ))
  stopOnFailed(combinedTasks, runTasks(combinedTasks))

  sizes <- query(paste0(
    "SELECT cohort_definition_id, COUNT(*) AS persons FROM ", popQ, " WHERE cohort_definition_id NOT IN (",
    paste(combinedIds, collapse = ", "), ") GROUP BY cohort_definition_id"
  ))
  sizes$persons <- as.numeric(sizes$persons)
  names(sizes)[names(sizes) == "cohortDefinitionId"] <- "cohortId"
  sizes$population <- ifelse(sizes$cohortId == baseId, "CANDIDATE", ifelse(sizes$cohortId == negativeBaseId, "CANDIDATE NEGATIVE",
    ifelse(sizes$cohortId %in% negativeIds, "NEGATIVE", ifelse(sizes$cohortId %in% missedIds, "FAILED ER2", "POSITIVE"))))
  sizes$rubric <- c(names(positiveIds), names(negativeIds), names(missedIds))[match(sizes$cohortId, c(positiveIds, negativeIds, missedIds))]
  say("  populations: ", paste0(sizes$population, ifelse(is.na(sizes$rubric), "", paste0(" ", sizes$rubric)), "=", format(sizes$persons, big.mark = ","), collapse = "; "))
  emptyIds <- setdiff(c(positiveIds, negativeIds), sizes$cohortId)
  if (length(emptyIds) > 0L) {
    emptyNames <- c(paste("POSITIVE", names(positiveIds)), paste("NEGATIVE", names(negativeIds)))[match(emptyIds, c(positiveIds, negativeIds))]
    isBase <- sizes$cohortId %in% c(baseId, negativeBaseId)
    stop(
      "Empty population(s) in ", databaseId, ": ", paste(emptyNames, collapse = ", "), " (no ER2 subjects). Candidates: ",
      paste0(sizes$population[isBase], "=", sizes$persons[isBase], collapse = "; "),
      ". Check personModulus (a large value leaves too few people) and that ER2 can match in this database.", call. = FALSE
    )
  }

  # Rule hits, plus pseudo rule -2 (ER1 on failed-ER2 candidates).
  hitTasks <- list()
  unresolved <- list()
  errors <- list()
  ocErrors <- list()
  hitInsert <- function(ruleIdx, popId, selectSql) {
    paste0("INSERT INTO ", hitsQ, " (rule_idx, cohort_definition_id, subject_id)\nSELECT DISTINCT ", ruleIdx, ", ", popId, ", subject_id FROM (", ithDataDrivenStripSemicolon(selectSql), ") rule_subjects")
  }
  combinedHitInsert <- function(ruleIdx, rubricId, selectSql) {
    paste0(
      "INSERT INTO ", hitsQ, " (rule_idx, cohort_definition_id, subject_id)\nSELECT DISTINCT ", ruleIdx, ", p.cohort_definition_id, p.subject_id FROM (",
      ithDataDrivenStripSemicolon(selectSql), ") rule_subjects\nINNER JOIN ", popQ, " p ON p.subject_id = rule_subjects.subject_id AND p.cohort_definition_id IN (",
      memberIds(rubricId), ")"
    )
  }
  for (i in seq_len(nrow(rules))) {
    key <- as.character(rules$ruleIdx[[i]])
    rubricId <- as.character(rules$rubric[[i]])
    built <- tryCatch(render(rules$sqlStatement[[i]], combinedIds[[rubricId]]), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(built$error)) {
      errors[[key]] <- c(errors[[key]], built$error)
      next
    }
    unresolved[[key]] <- unique(c(unresolved[[key]], built$unresolved))
    hitTasks[[length(hitTasks) + 1L]] <- list(
      kind = "exec", sql = combinedHitInsert(rules$ruleIdx[[i]], rubricId, built$sql), ruleKey = key,
      label = paste0(rubricId, "/", rules$ruleId[[i]])
    )
  }
  er1Tested <- list()
  for (rubricId in rubricIds) {
    er1Sql <- er1Text[[rubricId]]
    built <- if (is.null(er1Sql)) NULL else tryCatch(render(er1Sql, missedIds[[rubricId]]), error = function(e) NULL)
    if (is.null(built) || length(built$unresolved) > 0L) next
    er1Tested[[rubricId]] <- TRUE
    hitTasks[[length(hitTasks) + 1L]] <- list(
      kind = "exec", sql = hitInsert(-2L, missedIds[[rubricId]], built$sql), ocKey = paste0(rubricId, "/ER1"), er1Rubric = rubricId,
      label = paste0(rubricId, "/ER1 failed ER2")
    )
  }
  say("  materializing ", length(hitTasks), " hit set(s)", if (!is.null(cluster)) " in parallel" else "")
  hitOutcomes <- runTasks(hitTasks)
  for (i in seq_along(hitTasks)) {
    if (!isTRUE(hitOutcomes[[i]]$ok) && !is.null(hitTasks[[i]]$ruleKey)) {
      errors[[hitTasks[[i]]$ruleKey]] <- c(errors[[hitTasks[[i]]$ruleKey]], hitOutcomes[[i]]$error)
    }
    if (!isTRUE(hitOutcomes[[i]]$ok) && !is.null(hitTasks[[i]]$ocKey)) {
      ocErrors[[hitTasks[[i]]$ocKey]] <- hitOutcomes[[i]]$error
      if (!is.null(hitTasks[[i]]$er1Rubric)) er1Tested[[hitTasks[[i]]$er1Rubric]] <- FALSE
    }
  }

  counts <- query(paste0("SELECT rule_idx, cohort_definition_id, COUNT(*) AS n FROM ", hitsQ, " GROUP BY rule_idx, cohort_definition_id"))
  names(counts)[names(counts) == "cohortDefinitionId"] <- "cohortId"
  counts$n <- as.numeric(counts$n)

  positiveSizes <- sizes$persons[sizes$cohortId %in% positiveIds]
  negativeSizes <- sizes$persons[sizes$cohortId %in% negativeIds]
  stepPositive <- max(1L, as.integer(ceiling(max(positiveSizes) / (params$matrixPersons / 2))))
  stepNegative <- max(1L, as.integer(ceiling(max(negativeSizes) / (params$matrixPersons / 2))))
  missedSizes <- c(0, sizes$persons[sizes$cohortId %in% missedIds])
  stepMissed <- max(1L, as.integer(ceiling(max(missedSizes) / (params$matrixPersons / 2))))
  sampleWhere <- paste0(
    "((cohort_definition_id IN (", paste(positiveIds, collapse = ","), ") AND subject_id % ", stepPositive, " = 0) OR ",
    "(cohort_definition_id IN (", paste(negativeIds, collapse = ","), ") AND subject_id % ", stepNegative, " = 0) OR ",
    "(cohort_definition_id IN (", paste(missedIds, collapse = ","), ") AND subject_id % ", stepMissed, " = 0))"
  )
  say("  sampling every ", stepPositive, "th positive, ", stepNegative, "th negative and ", stepMissed, "th failed-ER2 person for the total-score and PPV checks")
  persons <- query(paste0("SELECT cohort_definition_id, subject_id FROM ", popQ, " WHERE ", sampleWhere))
  hits <- query(paste0("SELECT rule_idx, cohort_definition_id, subject_id FROM ", hitsQ, " WHERE ", sampleWhere))
  names(persons) <- c("cohortId", "subjectId")
  names(hits) <- c("ruleIdx", "cohortId", "subjectId")
  persons$subjectId <- as.numeric(persons$subjectId)
  hits$subjectId <- as.numeric(hits$subjectId)

  list(
    databaseId = databaseId, sizes = sizes, counts = counts, persons = persons, hits = hits, unresolved = unresolved, errors = errors,
    ocErrors = ocErrors, er1Tested = er1Tested, popIds = list(positive = positiveIds, negative = negativeIds, missed = missedIds)
  )
}

# Latest saved query results for this condition and database whose fingerprint matches; NULL otherwise.
ithDataDrivenLoadQueryResults <- function(queryFolder, conditionName, databaseId, fingerprint, say) {
  prefix <- paste0(conditionName, "_queryResults_", databaseId, "_")
  files <- list.files(queryFolder, pattern = "\\.rds$", full.names = TRUE)
  files <- sort(files[startsWith(basename(files), prefix)], decreasing = TRUE)
  for (file in files) {
    saved <- tryCatch(readRDS(file), error = function(e) NULL)
    if (!is.null(saved) && identical(saved$fingerprint, fingerprint)) {
      say("Database ", databaseId, ": reusing query results from ", file)
      return(saved$collected)
    }
  }
  say("Database ", databaseId, ": no saved query results match the current rules and settings; running the SQL")
  NULL
}

# Evidence for one condition: query results (saved per database), proposed scores, the gate and a report.
# `cdm` maps database ids that are not in the config's general.cdm to their CDM schema. A non-NULL
# yesScore/noScore replaces the rubric's thresholds for the checks.
ithDataDrivenEvidence <- function(conditionName,
                                  config,
                                  outputDirectory,
                                  databaseIds,
                                  cdm = NULL,
                                  excludeRuleIds = c("ER1", "ER2", "AE1"),
                                  personModulus = 1L,
                                  workers = NULL,
                                  matrixPersons = 200000L,
                                  scaleMax = 6,
                                  capLr = 10,
                                  yesScore = NULL,
                                  noScore = NULL,
                                  gate = list(),
                                  reuseQueryResults = FALSE,
                                  saveResults = TRUE,
                                  verbose = TRUE) {
  params <- list(personModulus = as.integer(personModulus), matrixPersons = matrixPersons, scaleMax = scaleMax, capLr = capLr)
  conditionDir <- file.path(outputDirectory, conditionName)
  say <- function(...) ithMaybeMessage(verbose, ...)

  shortRubric <- ithLoadLatestShortRubric(conditionName, outputDirectory)
  concise <- ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
  if (is.null(concise)) stop("No concise rubric found for condition: ", conditionName)
  info <- concise[, intersect(c("rubric", "ruleId", "type", "score", "criteria", "rubricYesScore", "rubricNoScore"), names(concise)), drop = FALSE]
  shortRubric <- merge(shortRubric[, c("rubric", "ruleId", "sqlStatement")], info, by = c("rubric", "ruleId"), all.x = TRUE, sort = FALSE)
  shortRubric <- shortRubric[order(shortRubric$rubric, ithRuleIdOrder(shortRubric$ruleId)), , drop = FALSE]
  ruleSql <- function(ruleId) lapply(stats::setNames(unique(shortRubric$rubric), unique(shortRubric$rubric)), function(rubricId) {
    rows <- shortRubric[shortRubric$rubric == rubricId & toupper(trimws(shortRubric$ruleId)) == ruleId, , drop = FALSE]
    if (nrow(rows) == 0L || is.na(rows$sqlStatement[[1]]) || !nzchar(trimws(rows$sqlStatement[[1]]))) NULL else rows$sqlStatement[[1]]
  })
  ruleScore <- function(ruleId) lapply(stats::setNames(unique(concise$rubric), unique(concise$rubric)), function(rubricId) {
    suppressWarnings(as.numeric(concise$score[concise$rubric == rubricId & toupper(trimws(concise$ruleId)) == ruleId]))[1]
  })
  er2Text <- ruleSql("ER2")
  er1Text <- ruleSql("ER1")
  params$er2Score <- ruleScore("ER2")
  params$er1Score <- ruleScore("ER1")
  rules <- shortRubric[!(toupper(trimws(shortRubric$ruleId)) %in% toupper(trimws(excludeRuleIds))), , drop = FALSE]
  rules$score <- suppressWarnings(as.numeric(rules$score))
  rules$yesScore <- suppressWarnings(as.numeric(rules$rubricYesScore))
  rules$yesScore[is.na(rules$yesScore)] <- 10
  rules$noScore <- suppressWarnings(as.numeric(rules$rubricNoScore))
  rules$noScore[is.na(rules$noScore)] <- 0
  if (!is.null(yesScore)) rules$yesScore <- yesScore
  if (!is.null(noScore)) rules$noScore <- noScore
  rules$ruleIdx <- seq_len(nrow(rules))
  if (nrow(rules) == 0L) stop("No rules left to test after excluding ", paste(excludeRuleIds, collapse = ", "))
  say("Condition '", conditionName, "': testing ", nrow(rules), " rule(s) in ", paste(databaseIds, collapse = " and "))

  conceptSets <- tryCatch(list(
    library = ithLoadConceptSetLibraryForScoring(
      outputDirectory = outputDirectory, seedLibraryPath = config$seedLibraryPath, libraryVersion = config$conceptLibraryVersion
    )[[1]],
    matched = ithLoadLatest(conditionDir, "matchedConceptSets.rds")
  ), error = function(e) list(error = conditionMessage(e)))
  conditionSet <- ithAe1ConditionConceptSetName(conditionName)
  relevantSet <- ithAe1ConceptSetName(conditionName)

  workers <- if (is.null(workers)) ithConfigParallelWorkers(config, "scoring") else max(1L, as.integer(workers))
  cluster <- NULL
  queryFolder <- file.path(outputDirectory, "rubricEvidenceProbe", "queryResults")

  perDb <- list()
  for (databaseId in databaseIds) {
    cdmMap <- if (databaseId %in% names(config$cdm)) config$cdm[databaseId] else cdm[databaseId]
    if (is.null(cdmMap[[1]])) stop("No CDM schema for database '", databaseId, "' in the config (general.cdm) or in cdm")
    fingerprint <- list(
      rules = rules[, c("rubric", "ruleId", "sqlStatement")], er1Text = er1Text, er2Text = er2Text, cdmSchema = cdmMap[[1]],
      personModulus = params$personModulus, matrixPersons = params$matrixPersons
    )
    rownames(fingerprint$rules) <- NULL
    collected <- if (isTRUE(reuseQueryResults)) ithDataDrivenLoadQueryResults(queryFolder, conditionName, databaseId, fingerprint, say) else NULL
    if (is.null(collected)) {
      if (is.null(cluster) && workers > 1L) {
        say("Starting ", workers, " worker(s) for the SQL calls")
        cluster <- ithStartWorkerCluster(workers)
        on.exit(ithStopWorkerCluster(cluster), add = TRUE)
      }
      collected <- ithDataDrivenCollectDatabase(
        conditionName, databaseId, cdmMap, config, rules, er2Text, conceptSets, conditionSet, relevantSet, params, cluster, say, er1Text
      )
      dir.create(queryFolder, recursive = TRUE, showWarnings = FALSE)
      queryFile <- file.path(queryFolder, paste0(conditionName, "_queryResults_", databaseId, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".rds"))
      saveRDS(list(fingerprint = fingerprint, collected = collected, savedAt = Sys.time()), queryFile)
      say("  query results saved: ", queryFile)
    }
    perDb[[databaseId]] <- ithDataDrivenAnalyzeDatabase(collected, rules, params)
  }

  recs <- ithDataDrivenRecommend(rules, perDb, params)
  gateResult <- ithDataDrivenGate(perDb, rules, recs, params, gate)
  report <- ithDataDrivenBuildReport(conditionName, databaseIds, perDb, rules, recs, params, gateResult)
  if (verbose) cat(report, sep = "\n")

  savedTo <- NULL
  if (isTRUE(saveResults)) {
    folder <- file.path(outputDirectory, "rubricEvidenceProbe")
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    path <- function(name, extension) file.path(folder, paste0(conditionName, "_", name, "_", stamp, ".", extension))
    savedTo <- c(report = path("improvementReport", "txt"), recommendations = path("improvementRules", "csv"), flaggedRubric = path("conciseRubricFlagged", "csv"))
    writeLines(report, savedTo[["report"]])
    utils::write.csv(recs, savedTo[["recommendations"]], row.names = FALSE)
    flagged <- merge(
      concise, recs[, c("rubric", "ruleId", "flag", "reason", "proposedType", "proposedScore")], by = c("rubric", "ruleId"), all.x = TRUE, sort = FALSE
    )
    names(flagged)[names(flagged) == "flag"] <- "probeFlag"
    names(flagged)[names(flagged) == "reason"] <- "probeReason"
    names(flagged)[names(flagged) == "proposedType"] <- "probeProposedType"
    names(flagged)[names(flagged) == "proposedScore"] <- "probeProposedScore"
    flagged <- flagged[order(flagged$rubric, ithRuleIdOrder(flagged$ruleId)), , drop = FALSE]
    utils::write.csv(flagged, savedTo[["flaggedRubric"]], row.names = FALSE)
    say("Saved: ", paste(savedTo, collapse = " ; "))
  }

  list(recommendations = recs, gate = gateResult, report = report, perDatabase = perDb, savedTo = savedTo)
}
