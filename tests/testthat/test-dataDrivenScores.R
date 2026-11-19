writeDataDrivenConfig <- function(outputDirectory, conditions = "COPD", extra = "") {
  specs <- vapply(conditions, function(name) {
    paste0(
      "    - testCondition: ", name, "\n      dxCriteria: outpatientOnly\n      additionalInformation: \"\"\n",
      "      rubricName: ", name, "\n      randomCohortDefinitionId: 1\n"
    )
  }, character(1))
  path <- tempfile(fileext = ".yaml")
  writeLines(paste0(
    "configVersion: 1\ngeneral:\n  outputDirectory: ", outputDirectory, "\n  verbose: false\n",
    "  dxCriteria:\n    outpatientOnly:\n      criteria: \"Two outpatient codes.\"\n      additionalPoints: 0\n",
    "  conditionSpecs:\n", paste(specs, collapse = ""),
    "other:\n  databaseSpecNames:\n    - fakeSpec\n",
    extra
  ), path)
  path
}

# Recommendations as the evidence step returns them, for the makeRubricFixture() rules.
fakeDataDrivenRecs <- function(...) {
  base <- data.frame(
    ruleIdx = 1:3, rubric = "R1", ruleId = c("ER3", "AE1", "HE1"),
    type = c("evidenceRules", "alternativeExplanations", "hard_exclusion"), score = c(4, -4, -100),
    recommendation = "KEEP", skipped = FALSE, proposedType = c("evidenceRules", "alternativeExplanations", "hard_exclusion"),
    proposedScore = c(4, -4, -100), stringsAsFactors = FALSE
  )
  changes <- list(...)
  for (ruleId in names(changes)) {
    row <- base$ruleId == ruleId
    for (column in names(changes[[ruleId]])) base[[column]][row] <- changes[[ruleId]][[column]]
  }
  base
}

fakeDataDrivenEvidence <- function(recs, passed = TRUE, reason = "") {
  list(
    recommendations = recs,
    gate = list(
      table = data.frame(database = "A", rubric = "R1", passed = passed, reason = reason, stringsAsFactors = FALSE),
      passed = passed
    ),
    report = "report", perDatabase = list(), savedTo = NULL
  )
}

mockDataDrivenEvidence <- function(evidence, env = parent.frame()) {
  force(evidence)
  testthat::local_mocked_bindings(ithDataDrivenEvidence = function(...) evidence, .package = "Ithaca", .env = env)
}

runDataDriven <- function(folder, conditions = "COPD", llm = makeFakeLlm(), ..., extra = "dataDrivenScoring:\n  databaseIds: [A, B]\n") {
  config <- writeDataDrivenConfig(folder, conditions, extra)
  suppressMessages(applyDataDrivenRuleScores(
    conditions, configPath = config, llmClient = llm, promptDir = Ithaca:::ithPromptDir(),
    updateSummary = FALSE, verbose = FALSE, ...
  ))
}

test_that("a likelihood ratio maps to points on the capLr scale", {
  scale <- Ithaca:::ithDataDrivenScaleScore

  expect_equal(scale(log(c(10, 1, 0.1, 1000, 0.001)), scaleMax = 6, capLr = 10), c(6, 0, -6, 6, -6))
  expect_equal(scale(log(c(10, 100)), scaleMax = 6, capLr = 100), c(3, 6))
  expect_equal(scale(log(3), scaleMax = 6, capLr = 10), 3)
  expect_true(is.na(scale(NA_real_)))
})

dataDrivenRules <- function() {
  data.frame(
    ruleIdx = 1:6, rubric = "R1", ruleId = c("ER3", "ER4", "AE2", "HE1", "ER5", "ER6"),
    type = c("evidenceRules", "evidenceRules", "alternativeExplanations", "hard_exclusion", "evidenceRules", "evidenceRules"),
    score = c(2, 3, -3, -100, 2, 2), criteria = "x", stringsAsFactors = FALSE
  )
}

dataDrivenUni <- function() {
  rates <- Ithaca:::ithDataDrivenCompareRates
  rbind(
    cbind(data.frame(ruleIdx = 1L), rates(1000, 1e5, 100, 1e5, 1L)),
    cbind(data.frame(ruleIdx = 2L), rates(100, 1e5, 100, 1e5, 1L)),
    cbind(data.frame(ruleIdx = 3L), rates(1000, 1e5, 100, 1e5, -1L)),
    cbind(data.frame(ruleIdx = 4L), rates(1000, 1e5, 100, 1e5, -1L)),
    cbind(data.frame(ruleIdx = 5L), rates(0, 1e5, 0, 1e5, 1L)),
    cbind(data.frame(ruleIdx = 6L), rates(500, 1e5, 100, 1e5, 1L))
  )
}

test_that("proposed scores follow the likelihood ratio, the type follows the sign, and unscorable rules keep their score", {
  perDb <- list(
    A = list(uni = dataDrivenUni(), errors = list("6" = "boom"), unresolved = list()),
    B = list(uni = dataDrivenUni(), errors = list("6" = "boom"), unresolved = list())
  )

  recs <- Ithaca:::ithDataDrivenRecommend(dataDrivenRules(), perDb, list(scaleMax = 6, capLr = 10))

  byRule <- stats::setNames(seq_len(nrow(recs)), recs$ruleId)
  expect_equal(recs$proposedScore[byRule[["ER3"]]], 6)
  expect_equal(recs$recommendation[byRule[["ER3"]]], "RESCALE")
  expect_equal(recs$proposedScore[byRule[["ER4"]]], 0)
  expect_equal(recs$proposedType[byRule[["ER4"]]], "evidenceRules")
  expect_equal(recs$proposedType[byRule[["AE2"]]], "evidenceRules")
  expect_equal(recs$recommendation[byRule[["AE2"]]], "CHANGE_TYPE")
  expect_equal(recs$proposedScore[byRule[["AE2"]]], 6)
  expect_equal(recs$proposedScore[byRule[["HE1"]]], -100)
  expect_equal(recs$recommendation[byRule[["HE1"]]], "KEEP")
  expect_true(recs$skipped[byRule[["ER5"]]])
  expect_match(recs$reason[byRule[["ER5"]]], "^0 records found")
  expect_equal(recs$proposedScore[byRule[["ER5"]]], 2)
  expect_true(recs$skipped[byRule[["ER6"]]])
  expect_equal(recs$recommendation[byRule[["ER6"]]], "REVIEW")
  expect_false(any(recs$skipped[c(byRule[["ER3"]], byRule[["HE1"]])]))
})

dataDrivenCollected <- function(seed) {
  set.seed(seed)
  ids <- c(11, 12, 13)
  n <- c(3000, 3000, 2000)
  persons <- data.frame(cohortId = rep(ids, n), subjectId = unlist(lapply(n, seq_len)) + rep(c(0, 1e6, 2e6), n))
  rate <- rbind(c(0.5, 0.1, 0.3), c(0.3, 0.3, 0.2), c(0.05, 0.2, 0.1), c(0.01, 0.02, 0.02))
  hits <- do.call(rbind, lapply(1:4, function(r) {
    keep <- runif(nrow(persons)) < rate[r, match(persons$cohortId, ids)]
    data.frame(ruleIdx = rep(r, sum(keep)), cohortId = persons$cohortId[keep], subjectId = persons$subjectId[keep])
  }))
  counts <- stats::aggregate(subjectId ~ ruleIdx + cohortId, hits, length)
  names(counts)[3] <- "n"
  list(
    persons = persons, hits = hits, counts = counts,
    sizes = data.frame(cohortId = ids, persons = n, population = c("POSITIVE", "NEGATIVE", "FAILED ER2"), rubric = "R1"),
    popIds = list(positive = c(R1 = 11), negative = c(R1 = 12), missed = c(R1 = 13)),
    er1Tested = list(), unresolved = list(), errors = list()
  )
}

test_that("evidence, gate and report fit together on simulated query results", {
  rules <- data.frame(
    ruleIdx = 1:5, rubric = "R1", ruleId = c("ER3", "ER4", "AE2", "HE1", "ER5"),
    type = c("evidenceRules", "evidenceRules", "alternativeExplanations", "hard_exclusion", "evidenceRules"),
    score = c(2, 4, -3, -100, 2), criteria = "x", yesScore = 10, noScore = 2, stringsAsFactors = FALSE
  )
  params <- list(scaleMax = 6, capLr = 10, er1Score = list(R1 = 3), er2Score = list(R1 = 2))
  perDb <- list(
    A = Ithaca:::ithDataDrivenAnalyzeDatabase(dataDrivenCollected(1), rules, params),
    B = Ithaca:::ithDataDrivenAnalyzeDatabase(dataDrivenCollected(2), rules, params)
  )
  recs <- Ithaca:::ithDataDrivenRecommend(rules, perDb, params)
  gate <- Ithaca:::ithDataDrivenGate(perDb, rules, recs, params, Ithaca:::ithDataDrivenDefaults()$gate)

  report <- Ithaca:::ithDataDrivenBuildReport("test", names(perDb), perDb, rules, recs, params, gate)

  expect_equal(recs$ruleId[recs$skipped], "ER5")
  expect_equal(gate$table$database, c("A", "B"))
  expect_true(all(c("cohortYes", "differentialYes", "separation", "passed") %in% names(gate$table)))
  expect_true(any(grepl("GATE FOR APPLYING THE PROPOSED SCORES", report, fixed = TRUE)))
  expect_true(any(grepl("RULES NOT SCORED FROM THE DATA", report, fixed = TRUE)))
  expect_true(any(grepl("COHORT PPV/SENSITIVITY CHECK", report, fixed = TRUE)))
})

gateInput <- function(proposed, differentialHits = 0, cohortHits = 700) {
  X <- matrix(0, 2000, 1, dimnames = list(NULL, "1"))
  X[seq_len(cohortHits), 1] <- 1
  if (differentialHits > 0) X[1000 + seq_len(differentialHits), 1] <- 1
  group <- rep(c("cohort", "differential"), each = 1000)
  list(
    perDb = list(A = list(ocMatrices = list(R1 = list(X = X, group = group, er1 = as.numeric(group == "cohort"), er1Assumed = FALSE)))),
    rules = data.frame(rubric = "R1", yesScore = 10, ruleIdx = 1L, stringsAsFactors = FALSE),
    recs = data.frame(ruleIdx = 1L, proposedScore = proposed),
    params = list(er1Score = list(R1 = 3), er2Score = list(R1 = 2))
  )
}

test_that("the gate passes when cohort members reach YES and differential negatives rarely do", {
  input <- gateInput(proposed = 6, differentialHits = 0)
  gate <- Ithaca:::ithDataDrivenGate(input$perDb, input$rules, input$recs, input$params, Ithaca:::ithDataDrivenDefaults()$gate)

  expect_true(gate$passed)
  expect_equal(gate$table$cohortYes, 0.7)
  expect_equal(gate$table$differentialYes, 0)
  expect_equal(gate$table$separation, 0.7)
})

test_that("the gate fails on over-calling negatives, saturation, a collapsed cohort and weak separation", {
  defaults <- Ithaca:::ithDataDrivenDefaults()$gate

  overCalling <- gateInput(proposed = 12, differentialHits = 200)
  result <- Ithaca:::ithDataDrivenGate(overCalling$perDb, overCalling$rules, overCalling$recs, overCalling$params, defaults)
  expect_false(result$passed)
  expect_match(result$table$reason, "differential-negative YES 20.0% is above 10.0%")

  saturated <- gateInput(proposed = 6, cohortHits = 1000)
  result <- Ithaca:::ithDataDrivenGate(saturated$perDb, saturated$rules, saturated$recs, saturated$params, defaults)
  expect_false(result$passed)
  expect_match(result$table$reason, "cohort YES 100.0% is above 95.0%")

  collapsed <- gateInput(proposed = 6, cohortHits = 300)
  result <- Ithaca:::ithDataDrivenGate(collapsed$perDb, collapsed$rules, collapsed$recs, collapsed$params, defaults)
  expect_false(result$passed)
  expect_match(result$table$reason, "cohort YES 30.0% is below 50.0%")

  weak <- gateInput(proposed = 12, differentialHits = 60, cohortHits = 700)
  result <- Ithaca:::ithDataDrivenGate(weak$perDb, weak$rules, weak$recs, weak$params, list(separationMin = 0.8))
  expect_false(result$passed)
  expect_match(result$table$reason, "separation 64.0% is below 80.0%")
})

test_that("a gate limit set to NA is not checked and a missing population fails the gate", {
  input <- gateInput(proposed = 12, differentialHits = 200)
  result <- Ithaca:::ithDataDrivenGate(
    input$perDb, input$rules, input$recs, input$params, list(negativeYesMax = NA, separationMin = NA, positiveYesMax = NA, positiveYesMin = NA)
  )
  expect_true(result$passed)

  input$perDb$A$ocMatrices <- list()
  result <- Ithaca:::ithDataDrivenGate(input$perDb, input$rules, input$recs, input$params, list())
  expect_false(result$passed)
  expect_match(result$table$reason, "no sampled cohort")
})

test_that("settings layer the defaults, the config, the condition's section and the arguments", {
  config <- list(dataDrivenScoring = list(
    capLr = 5, gate = list(negativeYesMax = 0.2),
    conditions = list(Seizure = list(capLr = 3, gate = list(separationMin = NULL)))
  ))

  plain <- Ithaca:::ithDataDrivenSettings(config, "Asthma")
  expect_equal(plain$capLr, 5)
  expect_equal(plain$scaleMax, 6)
  expect_equal(plain$yesScore, 10)
  expect_equal(plain$noScore, 2)
  expect_equal(plain$gate$negativeYesMax, 0.2)
  expect_equal(plain$gate$separationMin, 0.3)

  seizure <- Ithaca:::ithDataDrivenSettings(config, "seizure", overrides = list(scaleMax = 4, gate = list(positiveYesMax = 0.9)))
  expect_equal(seizure$capLr, 3)
  expect_equal(seizure$scaleMax, 4)
  expect_null(seizure$gate$separationMin)
  expect_equal(seizure$gate$positiveYesMax, 0.9)
  expect_equal(seizure$gate$negativeYesMax, 0.2)

  expect_equal(Ithaca:::ithDataDrivenSettings(list(), "Asthma")$capLr, 10)
})

test_that("invalid settings are rejected with the setting named", {
  settings <- function(...) Ithaca:::ithDataDrivenSettings(list(dataDrivenScoring = list(...)), "Asthma")

  expect_error(settings(capLr = 1), "capLr must be a single number above 1")
  expect_error(settings(scaleMax = 0), "scaleMax")
  expect_error(settings(yesScore = 2, noScore = 2), "yesScore must be greater than noScore")
  expect_error(settings(gate = list(negativeYesMax = 1.5)), "between 0 and 1")
  expect_error(settings(gate = list(nonsense = 0.5)), "Unknown dataDrivenScoring gate limit 'nonsense'")
  expect_error(settings(capLR = 5), "Unknown dataDrivenScoring setting")
  expect_error(settings(reuseQueryResults = "yes"), "reuseQueryResults must be true or false")
})

test_that("passing gates apply the scores, retype by sign, keep the SQL and make no LLM call", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  recs <- fakeDataDrivenRecs(
    ER3 = list(recommendation = "RESCALE", proposedScore = 2),
    AE1 = list(recommendation = "CHANGE_TYPE", proposedType = "evidenceRules", proposedScore = 3, skipped = FALSE)
  )
  mockDataDrivenEvidence(fakeDataDrivenEvidence(recs))
  llm <- makeFakeLlm()

  result <- runDataDriven(folder, llm = llm)

  expect_equal(result$status, "applied")
  expect_equal(c(result$rescaled, result$retyped), c(1L, 1L))
  concise <- readConcise(folder, 2)
  expect_equal(concise$score[concise$ruleId == "ER3"], 2)
  expect_false("AE1" %in% concise$ruleId)
  expect_equal(concise$type[concise$ruleId == "ER4"], "evidenceRules")
  expect_equal(concise$score[concise$ruleId == "ER4"], 3)
  expect_equal(concise$score[concise$ruleId == "HE1"], -100)
  shortRubric <- readRDS(conditionFile(folder, "COPD_shortRubric_V2.rds"))$shortRubric
  expect_equal(shortRubric$sqlStatement[shortRubric$ruleId == "ER4"], "select 1 -- AE1")
  expect_length(llm$state$prompts, 0L)
  delta <- utils::read.csv(conditionFile(folder, "COPD_rubricDelta_V2.csv"), stringsAsFactors = FALSE)
  expect_equal(unique(delta$changeType[delta$ruleId == "ER4"]), "retyped")
})

test_that("a failed gate applies nothing and says why", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  recs <- fakeDataDrivenRecs(ER3 = list(recommendation = "RESCALE", proposedScore = 2))
  mockDataDrivenEvidence(fakeDataDrivenEvidence(recs, passed = FALSE, reason = "differential-negative YES 23.0% is above 10.0%"))

  result <- runDataDriven(folder)

  expect_equal(result$status, "gateFailed")
  expect_match(result$detail, "A R1: differential-negative YES 23.0%")
  expect_false(file.exists(conditionFile(folder, "COPD_conciseRubric_V2.csv")))
  expect_equal(readConcise(folder, 1)$score[readConcise(folder, 1)$ruleId == "ER3"], 4)
})

test_that("applyChanges = FALSE reports only, and no change is reported as such", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  mockDataDrivenEvidence(fakeDataDrivenEvidence(fakeDataDrivenRecs(ER3 = list(recommendation = "RESCALE", proposedScore = 2))))

  expect_equal(runDataDriven(folder, applyChanges = FALSE)$status, "reportOnly")
  expect_false(file.exists(conditionFile(folder, "COPD_conciseRubric_V2.csv")))

  mockDataDrivenEvidence(fakeDataDrivenEvidence(fakeDataDrivenRecs()))
  expect_equal(runDataDriven(folder)$status, "noChanges")
  expect_false(file.exists(conditionFile(folder, "COPD_conciseRubric_V2.csv")))
})

test_that("rules that could not be scored are counted and keep their score", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  recs <- fakeDataDrivenRecs(
    ER3 = list(recommendation = "RESCALE", proposedScore = 2),
    AE1 = list(recommendation = "REVIEW", skipped = TRUE)
  )
  mockDataDrivenEvidence(fakeDataDrivenEvidence(recs))

  result <- runDataDriven(folder)

  expect_equal(result$status, "applied")
  expect_equal(result$notScored, 1L)
  concise <- readConcise(folder, 2)
  expect_equal(concise$score[concise$ruleId == "AE1"], -4)
})

test_that("unapplied concise rubric edits stop the condition before any query runs", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  concisePath <- conditionFile(folder, "COPD_conciseRubric_V1.csv")
  concise <- utils::read.csv(concisePath, stringsAsFactors = FALSE)
  concise$score[concise$ruleId == "ER3"] <- 3
  utils::write.csv(concise, concisePath, row.names = FALSE)
  called <- FALSE
  testthat::local_mocked_bindings(
    ithDataDrivenEvidence = function(...) {
      called <<- TRUE
      stop("should not run")
    },
    .package = "Ithaca"
  )

  result <- suppressWarnings(runDataDriven(folder))

  expect_match(result$status, "^error: The latest concise rubric has edits that are not applied yet")
  expect_false(called)
})

test_that("the config wrapper runs each configured condition and reports a failing one", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  recs <- fakeDataDrivenRecs(ER3 = list(recommendation = "RESCALE", proposedScore = 2))
  mockDataDrivenEvidence(fakeDataDrivenEvidence(recs))
  testthat::local_mocked_bindings(ithGetLlmClientFromConfig = function(...) makeFakeLlm(), .package = "Ithaca")
  config <- writeDataDrivenConfig(folder, c("COPD", "Missing"), "dataDrivenScoring:\n  databaseIds: [A, B]\n")

  results <- suppressWarnings(suppressMessages(applyDataDrivenRuleScoresFromConfig(config)))

  expect_equal(results$conditionName, c("COPD", "Missing"))
  expect_equal(results$status[1], "applied")
  expect_match(results$status[2], "^error: No concise rubric found")
  expect_named(attr(results, "evidence"), c("COPD", "Missing"))
  expect_warning(
    suppressMessages(applyDataDrivenRuleScoresFromConfig(config, conditionNames = "Missing")),
    "1 condition\\(s\\) failed"
  )
  expect_error(applyDataDrivenRuleScoresFromConfig("config.yaml", conditionNames = "nonsense.yaml"), "looks like a config file")
})

test_that("a retyped rule keeps its SQL when applying concise edits", {
  withr::local_options(ithaca.yesCombinationReport = FALSE)
  folder <- makeRubricFixture("COPD")
  concisePath <- conditionFile(folder, "COPD_conciseRubric_V1.csv")
  concise <- utils::read.csv(concisePath, stringsAsFactors = FALSE)
  concise$type[concise$ruleId == "AE1"] <- "hard_exclusion"
  utils::write.csv(concise, concisePath, row.names = FALSE)
  llm <- makeFakeLlm()

  suppressMessages(applyConciseRubricEdits(
    "COPD", outputDirectory = folder, llmClient = llm, promptDir = Ithaca:::ithPromptDir(), updateSummary = FALSE, verbose = FALSE
  ))

  shortRubric <- readRDS(conditionFile(folder, "COPD_shortRubric_V2.rds"))$shortRubric
  expect_equal(shortRubric$sqlStatement[shortRubric$ruleId == "HE2"], "select 1 -- AE1")
  expect_length(llm$state$prompts, 0L)
})
