test_that("generated cohorts match the CDM and incremental reruns do not duplicate rows", {
  database <- makeEunomiaDatabase()
  definitions <- generateEunomiaCohorts(database)

  expectedGi <- eunomiaQuery(database, paste0(
    "SELECT COUNT(DISTINCT person_id) AS n FROM main.condition_occurrence WHERE condition_concept_id = 192671"
  ))$n
  expectedEveryone <- eunomiaQuery(database, "SELECT COUNT(DISTINCT person_id) AS n FROM main.visit_occurrence")$n
  counts <- table(Ithaca:::ithGetCohortSubjects(c(1, 2), database, verbose = FALSE)$cohortDefinitionId)

  expect_equal(unname(counts[["1"]]), expectedGi)
  expect_equal(unname(counts[["2"]]), expectedEveryone)

  generateEunomiaCohorts(database, definitions)

  again <- table(Ithaca:::ithGetCohortSubjects(c(1, 2), database, verbose = FALSE)$cohortDefinitionId)
  expect_equal(again, counts)
})

test_that("the cohort start date is the first qualifying date per subject", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)

  firstDates <- eunomiaQuery(database, paste0(
    "SELECT person_id, MIN(condition_start_date) AS first_date FROM main.condition_occurrence ",
    "WHERE condition_concept_id = 192671 GROUP BY person_id"
  ))
  cohort <- eunomiaCohortRows(database, 1)
  merged <- merge(cohort, firstDates, by.x = "subjectId", by.y = "personId")

  expect_equal(nrow(merged), nrow(cohort))
  expect_equal(merged$cohortStartDate, as.Date(merged$firstDate))
})

test_that("cohort counts are reported per cohort", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)

  counts <- Ithaca:::ithReportCohortCounts(
    database$connectionDetails, "main", database$cohortTable, c(1, 2, 99), verbose = FALSE
  )

  expect_equal(counts$cohortId, c(1L, 2L, 99L))
  expect_equal(counts$subjectCount[counts$cohortId == 2], nrow(eunomiaCohortRows(database, 2)))
  expect_equal(counts$subjectCount[counts$cohortId == 99], 0L)
})

test_that("cohort subjects can be fetched for selected cohorts and subjects", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)
  everyone <- eunomiaCohortRows(database, 2)

  onlyGi <- Ithaca:::ithGetCohortSubjects(1, database, verbose = FALSE)
  expect_true(all(onlyGi$cohortDefinitionId == 1))
  expect_equal(nrow(Ithaca:::ithGetCohortSubjects(99, database, verbose = FALSE)), 0L)

  picked <- everyone$subjectId[1:3]
  subjects <- Ithaca:::ithGetSubjects(paste(picked, collapse = ","), 2, database)
  expect_setequal(subjects$subjectId, picked)

  sample <- Ithaca:::ithGetSpecifiedSample(2, database, paste(everyone$subjectId[1:20], collapse = ","), count = 5)
  expect_equal(nrow(sample), 5L)
  expect_true(all(sample$subjectId %in% everyone$subjectId[1:20]))
})

test_that("a random sample has the requested size and comes from the cohort", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)

  sample <- Ithaca:::ithGetRandomSample(2, database, 7)

  expect_equal(nrow(sample), 7L)
  expect_true(all(sample$subjectId %in% eunomiaCohortRows(database, 2)$subjectId))
})

test_that("cohort generation needs definitions or WebAPI details", {
  database <- makeEunomiaDatabase()

  expect_error(
    Ithaca:::ithGenerateCohorts(NULL, list(database), verbose = FALSE),
    "Provide cohortDefinitionSet or provide both cohortIds and webapiBaseUrl"
  )
  expect_error(
    Ithaca:::ithGenerateCohorts(data.frame(cohortId = 1), list(database), verbose = FALSE),
    "must contain cohortId, cohortName, and sql"
  )
})
