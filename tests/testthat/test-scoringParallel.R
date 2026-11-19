scoringOutputs <- function(outputDirectory) {
  directory <- scoringDirectory(outputDirectory)
  list(
    fullScoringFile = readRDS(file.path(directory, "fullScoringFile_Cohort2_V1.rds")),
    subjectCount = utils::read.csv(file.path(directory, "subjectCount_Cohort2_V1.csv"), stringsAsFactors = FALSE),
    pluginResolution = utils::read.csv(file.path(directory, "pluginResolution_Cohort2_V1.csv"), stringsAsFactors = FALSE),
    sumCohortScores = readRDS(file.path(directory, "sumCohortScores_Cohort2_V1.rds")),
    finalScores = readRDS(file.path(directory, "finalScores_Cohort2_V1.rds"))
  )
}

test_that("parallel scoring saves the same outputs as serial scoring, including LLM SQL correction", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  broken <- gsub("co.person_id", "co.nonexistent_column", eunomiaRuleSql("4112343", 365), fixed = TRUE)
  fakeLlm <- function() {
    makeFakeLlm(structured = function(kind, prompt, call) {
      list(originalQuery = broken, changesMade = "fixed column", correctedQuery = eunomiaRuleSql("4112343", 365), unableToCorrect = "NO")
    })
  }

  serialDirectory <- withr::local_tempdir()
  serialLibrary <- writeEunomiaRubric(serialDirectory, usePlugin = TRUE, ruleSql = list(ER2 = broken))
  scoreEunomia(database, serialDirectory, serialLibrary, llmClient = fakeLlm(), retryLimit = 3)

  parallelDirectory <- withr::local_tempdir()
  parallelLibrary <- writeEunomiaRubric(parallelDirectory, usePlugin = TRUE, ruleSql = list(ER2 = broken))
  cluster <- Ithaca:::ithStartWorkerCluster(2L)
  on.exit(Ithaca:::ithStopWorkerCluster(cluster), add = TRUE)
  llm <- fakeLlm()
  scoreEunomia(database, parallelDirectory, parallelLibrary, llmClient = llm, retryLimit = 3, cluster = cluster)

  serial <- scoringOutputs(serialDirectory)
  parallel <- scoringOutputs(parallelDirectory)
  expect_identical(parallel, serial)
  expect_gt(sum(serial$subjectCount$subjectCount), 0)
  expect_equal(llm$state$structuredCalls, 1L)
  saved <- readRDS(file.path(parallelDirectory, "GI bleed", "GI bleed_shortRubric_V2.rds"))$shortRubric
  expect_equal(saved$sqlStatement[saved$ruleId == "ER2"], eunomiaRuleSql("4112343", 365))

  times <- utils::read.csv(file.path(scoringDirectory(parallelDirectory), "ruleQueryTimes.csv"), stringsAsFactors = FALSE)
  expect_equal(times$ruleId, c("ER1", "ER2", "AE1"))
  expect_equal(times$rowCount, parallel$subjectCount$subjectCount)
  expect_true(all(times$seconds >= 0))
  # Each worker wrote its query to its own file in the working directory the scorer ran in.
  expect_gte(length(list.files("lastQuery", pattern = "^query_worker[0-9]+[.]sql$")), 1L)
})

test_that("rule queries are dispatched slowest first, then AE rules, then rubric order", {
  shortRubric <- data.frame(
    rubric = "R1", ruleId = c("ER1", "ER2", "ER3", "AE1", "AE2"), stringsAsFactors = FALSE
  )
  cohortDir <- withr::local_tempdir()

  expect_equal(Ithaca:::ithScoringDispatchOrder(shortRubric, cohortDir), c(4L, 5L, 1L, 2L, 3L))

  utils::write.csv(
    data.frame(rubricId = "R1", ruleId = c("ER1", "ER2", "AE1", "AE2"), seconds = c(5, 60, 90, 1)),
    file.path(cohortDir, "ruleQueryTimes.csv"),
    row.names = FALSE
  )
  # ER3 has no saved time, so it goes first.
  expect_equal(Ithaca:::ithScoringDispatchOrder(shortRubric, cohortDir), c(3L, 4L, 2L, 1L, 5L))
})

test_that("SQL complexity orders unknown rules and history overrides the estimate", {
  shortRubric <- data.frame(
    rubric = "R1",
    ruleId = c("ER1", "ER2", "ER3"),
    sqlStatement = c(
      "select 1",
      "select distinct x.person_id from x join y on x.id = y.id where exists (select 1 from z)",
      "select count(distinct person_id) from x join y on x.id = y.id group by x.person_id"
    ),
    stringsAsFactors = FALSE
  )
  cohortDir <- withr::local_tempdir()

  dispatch <- Ithaca:::ithScoringDispatchOrder(shortRubric, cohortDir)
  expect_equal(dispatch[[1]], 3L)

  utils::write.csv(
    data.frame(rubricId = "R1", ruleId = "ER1", seconds = 200),
    file.path(cohortDir, "ruleQueryTimes.csv"),
    row.names = FALSE
  )
  dispatch <- Ithaca:::ithScoringDispatchOrder(shortRubric, cohortDir)
  expect_equal(dispatch[[1]], 1L)
})

test_that("worker counts come from general.parallelWorkers with the legacy key as fallback", {
  expect_equal(Ithaca:::ithConfigParallelWorkers(list(), "scoring"), 1L)
  expect_equal(Ithaca:::ithConfigParallelWorkers(list(parallelWorkers = list(scoring = 4)), "scoring"), 4L)
  expect_equal(
    Ithaca:::ithConfigParallelWorkers(list(conceptSetWorkers = 3), "conceptSetCreation", legacyKey = "conceptSetWorkers"),
    3L
  )
  expect_equal(
    Ithaca:::ithConfigParallelWorkers(
      list(parallelWorkers = list(conceptSetCreation = 5), conceptSetWorkers = 3),
      "conceptSetCreation",
      legacyKey = "conceptSetWorkers"
    ),
    5L
  )
  expect_error(
    Ithaca:::ithConfigParallelWorkers(list(parallelWorkers = list(scoring = 0)), "scoring"),
    "general.parallelWorkers.scoring must be a whole number >= 1"
  )
})

test_that("existing scoring is reusable only when all summary artifacts exist", {
  cohortDir <- withr::local_tempdir()
  required <- c(
    "sumCohortScores_Cohort2_V1.rds",
    "finalScores_Cohort2_V1.rds",
    "subjectCount_Cohort2_V1.csv",
    "fullScoringFile_Cohort2_V1.rds"
  )

  expect_false(Ithaca:::ithScoringArtifactsComplete(cohortDir, 2L))
  file.create(file.path(cohortDir, required[1:3]))
  expect_false(Ithaca:::ithScoringArtifactsComplete(cohortDir, 2L))
  file.create(file.path(cohortDir, required[[4]]))
  expect_true(Ithaca:::ithScoringArtifactsComplete(cohortDir, 2L))
})

test_that("the config templates define general.parallelWorkers", {
  for (path in c(system.file("config_template.yaml", package = "Ithaca"), testthat::test_path("..", "..", "extras", "config_template.yaml"))) {
    skip_if_not(file.exists(path))
    template <- yaml::read_yaml(path)
    expect_equal(template$general$parallelWorkers, list(conceptSetCreation = 1L, scoring = 1L, cohortGeneration = 1L))
    expect_null(template$conceptSetBuilding$conceptSetWorkers)
  }
})
