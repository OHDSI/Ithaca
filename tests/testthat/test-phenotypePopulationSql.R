capturePhenotypeQuery <- function(randomCohortDefinitionId = NULL, requireObservationPeriod = TRUE) {
  captured <- new.env()
  testthat::local_mocked_bindings(
    connect = function(...) "connection",
    disconnect = function(...) invisible(NULL),
    querySql = function(connection, sql, ...) {
      captured$sql <- sql
      data.frame(subjectId = 1, phenoCohortStartDate = as.Date("2020-01-01"))
    },
    .package = "DatabaseConnector"
  )
  Ithaca:::getPhenotypePopulation(
    connectionDetails = list(dbms = "spark"),
    cdmDatabaseSchema = "cdm",
    cohortDatabaseSchema = "scratch.me",
    cohortTable = "cohort",
    phenotypeCohortId = 5,
    requireObservationPeriod = requireObservationPeriod,
    randomCohortDefinitionId = randomCohortDefinitionId
  )
  captured$sql
}

test_that("the phenotype query is unrestricted without a random cohort", {
  sql <- capturePhenotypeQuery()

  expect_match(sql, "cohort_definition_id = 5", fixed = TRUE)
  expect_no_match(sql, "EXISTS", fixed = TRUE)
})

test_that("the phenotype query is limited to random cohort subjects in the database", {
  for (requireObservationPeriod in c(TRUE, FALSE)) {
    sql <- capturePhenotypeQuery(randomCohortDefinitionId = 9, requireObservationPeriod = requireObservationPeriod)

    expect_match(sql, "EXISTS", fixed = TRUE)
    expect_match(sql, "cohort_definition_id = 9", fixed = TRUE)
    expect_match(sql, "r.subject_id = c.subject_id", fixed = TRUE)
  }
})

test_that("the observation period join is only added when required", {
  expect_match(capturePhenotypeQuery(requireObservationPeriod = TRUE), "observation_period", fixed = TRUE)
  expect_no_match(capturePhenotypeQuery(requireObservationPeriod = FALSE), "observation_period", fixed = TRUE)
})
