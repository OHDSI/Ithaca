phenotypeReport <- function(database, evaluation, randomCohortDefinitionId = NULL, splay = 30, ...) {
  suppressMessages(utils::capture.output(
    report <- Ithaca::testPhenotypeAlgorithmRubric(
      phenotype = "GI bleed", analysisName = "test", runDateTime = "20260101000000",
      connectionDetails = database$connectionDetails, evaluationCohortFile = evaluation,
      phenotypeCohortId = 1, cdmDatabaseSchema = "main", databaseId = "Eunomia",
      cohortDatabaseSchema = "main", cohortTable = database$cohortTable,
      splayPrior = splay, splayPost = splay, randomCohortDefinitionId = randomCohortDefinitionId, ...
    )
  ))
  report
}

# Every third random cohort subject scores as a case; the rest as non-cases.
syntheticEvaluation <- function(database) {
  everyone <- eunomiaCohortRows(database, 2)
  everyone$total <- ifelse(everyone$subjectId %% 3 == 0, 10, 0)
  everyone$yesScore <- 7
  everyone$noScore <- 3
  everyone
}

# The expected confusion counts, worked out in R from the cohort rows.
expectedConfusion <- function(database, evaluation, splay) {
  pheno <- eunomiaCohortRows(database, 1)
  names(pheno)[names(pheno) == "cohortStartDate"] <- "phenoStart"
  merged <- merge(evaluation, pheno, by = "subjectId", all.x = TRUE)
  merged$matched <- !is.na(merged$phenoStart) &
    abs(as.numeric(merged$cohortStartDate - merged$phenoStart)) <= splay
  matchedSubjects <- unique(merged$subjectId[merged$matched])
  testedNoMatch <- setdiff(unique(pheno$subjectId[pheno$subjectId %in% evaluation$subjectId]), matchedSubjects)
  kept <- evaluation[!(evaluation$subjectId %in% testedNoMatch), ]
  inPhenotype <- kept$subjectId %in% matchedSubjects
  isCase <- kept$total >= kept$yesScore
  list(
    tp = sum(inPhenotype & isCase), fp = sum(inPhenotype & !isCase),
    tn = sum(!inPhenotype & !isCase), fn = sum(!inPhenotype & isCase),
    total = nrow(kept)
  )
}

test_that("metrics match a confusion table worked out in R", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  evaluation <- syntheticEvaluation(database)

  report <- phenotypeReport(database, evaluation, splay = 30)
  expected <- expectedConfusion(database, evaluation, 30)

  expect_equal(report$truePositives, expected$tp)
  expect_equal(report$falsePositives, expected$fp)
  expect_equal(report$trueNegatives, expected$tn)
  expect_equal(report$falseNegatives, expected$fn)
  expect_equal(report$totalSubjects, expected$total)
  expect_gt(expected$tp + expected$fp, 0)
})

test_that("a wider date window matches more phenotype subjects", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  evaluation <- syntheticEvaluation(database)

  narrow <- phenotypeReport(database, evaluation, splay = 0)
  wide <- phenotypeReport(database, evaluation, splay = 36500)

  expect_equal(narrow$truePositives + narrow$falsePositives, expectedConfusion(database, evaluation, 0)$tp +
    expectedConfusion(database, evaluation, 0)$fp)
  expect_gt(wide$truePositives + wide$falsePositives, narrow$truePositives + narrow$falsePositives)
})

test_that("restricting the download to the random cohort gives identical results", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  evaluation <- syntheticEvaluation(database)

  unrestricted <- phenotypeReport(database, evaluation, randomCohortDefinitionId = NULL)
  restricted <- phenotypeReport(database, evaluation, randomCohortDefinitionId = 2)

  expect_equal(restricted, unrestricted)
})

test_that("the restricted download returns only random cohort subjects", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  # Keep half of the random cohort so some phenotype subjects fall outside it.
  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  DatabaseConnector::executeSql(
    connection,
    paste0("DELETE FROM main.", database$cohortTable, " WHERE cohort_definition_id = 2 AND subject_id % 2 = 0"),
    progressBar = FALSE, reportOverallTime = FALSE
  )

  everything <- Ithaca:::getPhenotypePopulation(
    database$connectionDetails, "main", "main", database$cohortTable, 1
  )
  restricted <- Ithaca:::getPhenotypePopulation(
    database$connectionDetails, "main", "main", database$cohortTable, 1, randomCohortDefinitionId = 2
  )

  random <- eunomiaCohortRows(database, 2)$subjectId
  expect_lt(nrow(restricted), nrow(everything))
  expect_true(all(restricted$subjectId %in% random))
  expect_equal(sort(restricted$subjectId), sort(everything$subjectId[everything$subjectId %in% random]))
})

test_that("supporting reports are written next to the results", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  outputFolder <- withr::local_tempdir()

  phenotypeReport(database, syntheticEvaluation(database), outputFolder = outputFolder, sampleSubjectCount = 3)

  files <- list.files(outputFolder)
  expect_true(any(grepl("subject_samples_V1\\.csv$", files)))
})

test_that("an evaluation file without thresholds needs overrides", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  evaluation <- syntheticEvaluation(database)
  evaluation$yesScore <- NULL
  evaluation$noScore <- NULL

  expect_error(phenotypeReport(database, evaluation), "does not contain yesScore")

  report <- phenotypeReport(database, evaluation, yesScoreOverride = 7, noScoreOverride = 3)
  expect_equal(report$totalSubjects, expectedConfusion(database, syntheticEvaluation(database), 30)$total)
})
