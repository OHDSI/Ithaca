setupScoring <- function(usePlugin = FALSE, ruleSql = NULL, env = parent.frame()) {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  outputDirectory <- withr::local_tempdir(.local_envir = env)
  libraryPath <- writeEunomiaRubric(outputDirectory, usePlugin = usePlugin, ruleSql = ruleSql)
  list(database = database, outputDirectory = outputDirectory, libraryPath = libraryPath)
}

test_that("every rule is scored for the random cohort and totals add up", {
  setup <- setupScoring()
  scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath)

  directory <- scoringDirectory(setup$outputDirectory)
  counts <- utils::read.csv(file.path(directory, "subjectCount_Cohort2_V1.csv"))
  expected <- list(
    ER1 = expectedRuleSubjects(setup$database, 192671, 30),
    ER2 = expectedRuleSubjects(setup$database, 4112343, 365),
    AE1 = expectedRuleSubjects(setup$database, 80180, 5)
  )
  for (ruleId in names(expected)) {
    expect_equal(counts$subjectCount[counts$ruleId == ruleId], length(expected[[ruleId]]), info = ruleId)
  }

  scores <- readRDS(file.path(directory, "sumCohortScores_Cohort2_V1.rds"))
  everyone <- eunomiaCohortRows(setup$database, 2)
  expect_equal(nrow(scores), nrow(everyone))
  expect_setequal(scores$subjectId, everyone$subjectId)

  points <- c(ER1 = 7, ER2 = 2, AE1 = -4)
  expectedTotal <- vapply(scores$subjectId, function(id) {
    sum(points[vapply(names(expected), function(rule) id %in% expected[[rule]], logical(1))])
  }, numeric(1))
  expect_equal(as.numeric(scores$total[order(scores$subjectId)]), as.numeric(expectedTotal[order(scores$subjectId)]))
})

test_that("case labels follow the yes and no thresholds", {
  setup <- setupScoring()
  scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath)

  scores <- readRDS(file.path(scoringDirectory(setup$outputDirectory), "sumCohortScores_Cohort2_V1.rds"))
  expectedCase <- ifelse(scores$total >= 7, 1, ifelse(scores$total > 3, 0, -1))

  expect_equal(as.numeric(scores$case), as.numeric(expectedCase))
  expect_true(all(c(-1, 0, 1) %in% scores$case) || any(scores$case == 1))
  expect_true(all(scores$yesScore == 7 & scores$noScore == 3))
})

test_that("concept sets in rule SQL are resolved from the library and logged", {
  setup <- setupScoring(usePlugin = TRUE)
  scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath)

  directory <- scoringDirectory(setup$outputDirectory)
  resolution <- utils::read.csv(file.path(directory, "pluginResolution_Cohort2_V1.csv"), stringsAsFactors = FALSE)
  expect_equal(resolution$pluginName, "GI_Hemorrhage_DIAGNOSIS_CODES")
  expect_match(resolution$resolutionStatus, "^resolved")
  expect_gte(resolution$resolvedConceptCount, 1L)

  counts <- utils::read.csv(file.path(directory, "subjectCount_Cohort2_V1.csv"))
  expectedIds <- expectedRuleSubjects(setup$database, eunomiaDescendants(setup$database, 192671), 30)
  expect_equal(counts$subjectCount[counts$ruleId == "ER1"], length(expectedIds))
})

test_that("a concept set missing from the library is reported and matches nobody", {
  setup <- setupScoring(usePlugin = TRUE)
  conditionDir <- file.path(setup$outputDirectory, "GI bleed")
  Ithaca:::ithSaveVersioned(
    conditionDir,
    data.frame(csName = "Other_DIAGNOSIS_CODES", libCsName = "Other_DIAGNOSIS_CODES", stringsAsFactors = FALSE),
    "matchedConceptSets.rds"
  )

  scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath)

  directory <- scoringDirectory(setup$outputDirectory)
  resolution <- utils::read.csv(file.path(directory, "pluginResolution_Cohort2_V1.csv"), stringsAsFactors = FALSE)
  expect_equal(resolution$resolutionStatus, "missingMatch")
  counts <- utils::read.csv(file.path(directory, "subjectCount_Cohort2_V1.csv"))
  expect_equal(counts$subjectCount[counts$ruleId == "ER1"], 0L)
})

test_that("SQL that fails is corrected by the LLM and the corrected SQL is saved", {
  broken <- gsub("co.person_id", "co.nonexistent_column", eunomiaRuleSql("4112343", 365), fixed = TRUE)
  setup <- setupScoring(ruleSql = list(ER2 = broken))
  llm <- makeFakeLlm(structured = function(kind, prompt, call) {
    list(originalQuery = broken, changesMade = "fixed column", correctedQuery = eunomiaRuleSql("4112343", 365), unableToCorrect = "NO")
  })

  scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath, llmClient = llm, retryLimit = 3)

  counts <- utils::read.csv(file.path(scoringDirectory(setup$outputDirectory), "subjectCount_Cohort2_V1.csv"))
  expect_equal(counts$subjectCount[counts$ruleId == "ER2"], length(expectedRuleSubjects(setup$database, 4112343, 365)))
  expect_equal(llm$state$structuredCalls, 1L)
  saved <- readRDS(file.path(setup$outputDirectory, "GI bleed", "GI bleed_shortRubric_V2.rds"))$shortRubric
  expect_equal(saved$sqlStatement[saved$ruleId == "ER2"], eunomiaRuleSql("4112343", 365))
})

test_that("a rule whose SQL cannot be corrected scores nobody without stopping the others", {
  broken <- gsub("co.person_id", "co.nonexistent_column", eunomiaRuleSql("4112343", 365), fixed = TRUE)
  setup <- setupScoring(ruleSql = list(ER2 = broken))
  llm <- makeFakeLlm(structured = function(kind, prompt, call) {
    list(originalQuery = broken, changesMade = "none", correctedQuery = broken, unableToCorrect = "YES")
  })

  scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath, llmClient = llm, retryLimit = 2)

  counts <- utils::read.csv(file.path(scoringDirectory(setup$outputDirectory), "subjectCount_Cohort2_V1.csv"))
  expect_equal(counts$subjectCount[counts$ruleId == "ER2"], 0L)
  expect_gt(counts$subjectCount[counts$ruleId == "ER1"], 0L)
  expect_equal(llm$state$structuredCalls, 1L)
})

test_that("existing scoring outputs are reused without querying the database", {
  setup <- setupScoring()
  scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath)

  reused <- scoreEunomia(setup$database, setup$outputDirectory, setup$libraryPath, useExisting = TRUE)

  expect_false(is.null(reused$scoredFile))
  expect_equal(sort(reused$subjectCounts$ruleId), c("AE1", "ER1", "ER2"))
  expect_false(file.exists(file.path(scoringDirectory(setup$outputDirectory), "subjectCount_Cohort2_V2.csv")))
})

test_that("the full scoring workflow generates cohorts, scores, and writes a summary", {
  database <- makeEunomiaDatabase()
  outputDirectory <- withr::local_tempdir()
  libraryPath <- writeEunomiaRubric(outputDirectory, usePlugin = TRUE)
  withr::local_dir(withr::local_tempdir())

  results <- suppressMessages(utils::capture.output(
    output <- Ithaca:::ithScoreSubjects(
      conditionSpecs = list(list(rubricName = "GI bleed", randomCohortDefinitionId = 2L)),
      databases = list(Eunomia = database),
      outputDirectory = outputDirectory,
      seedLibraryPath = libraryPath,
      generateCohorts = TRUE,
      cohortDefinitionSet = eunomiaCohortDefinitions(),
      llmClient = makeFakeLlm(),
      verbose = FALSE
    )
  ))

  expect_named(output, "GI bleed::Eunomia")
  expect_false(is.null(output[[1]]$scoringSummary))
  expect_equal(nrow(output[[1]]$finalScores), nrow(eunomiaCohortRows(database, 2)))
  expect_true(file.exists(file.path(scoringDirectory(outputDirectory), "finalScores_Cohort2_V1.rds")))
})
