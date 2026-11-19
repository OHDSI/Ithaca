sqlServerFiles <- function() {
  list.files(testthat::test_path("..", "..", "inst", "sql", "sql_server"), pattern = "\\.sql$", full.names = TRUE)
}

test_that("package SQL files contain no Spark or Oracle only syntax", {
  forbidden <- c(
    "TO_DATE", "MONTHS_BETWEEN", "USING DELTA", "\\bRAND\\(\\)", "AS STRING", "\\bILIKE\\b", "::",
    "DATEDIFF\\(\\s*[A-Za-z_.]+\\s*,\\s*[A-Za-z_.@]+\\s*\\)"
  )
  for (file in sqlServerFiles()) {
    sql <- paste(readLines(file, warn = FALSE), collapse = "\n")
    sql <- gsub("(?s)\\{[^}]*\\}", "", sql, perl = TRUE)
    for (pattern in forbidden) {
      expect_false(grepl(pattern, sql, ignore.case = TRUE, perl = TRUE), info = paste(basename(file), pattern))
    }
    expect_false(grepl("\"[A-Za-z0-9 ]+\"", sql), info = paste(basename(file), "double-quoted literal"))
  }
})

test_that("R code never hardcodes a SQL translation dialect", {
  rFiles <- list.files(testthat::test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  for (file in rFiles) {
    code <- readLines(file, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    expect_false(any(grepl("targetDialect\\s*=\\s*\"", code)), info = basename(file))
  }
})

test_that("every package SQL file translates to the supported dialects", {
  for (file in sqlServerFiles()) {
    sql <- paste(readLines(file, warn = FALSE), collapse = "\n")
    for (dialect in c("postgresql", "spark", "sqlite", "snowflake")) {
      expect_no_error(SqlRender::translate(sql, targetDialect = dialect))
    }
  }
})

test_that("cohort subject data and ingredient queries run on the Eunomia database", {
  database <- makeEunomiaDatabase()
  generateEunomiaCohorts(database)

  rows <- Ithaca:::ithGetCohortSubjectData(
    cohortsToTest = 1, database = database, endDays = 30,
    sqlFile = testthat::test_path("..", "..", "inst", "sql", "sql_server", "subjectDataCohorts.sql"),
    cohortTable = database$cohortTable
  )
  expect_true(all(c("conceptId", "conceptName") %in% names(rows)))
  expect_gt(nrow(rows), 0L)

  ingredients <- Ithaca:::ithGetIngredients(
    database, conceptList = c(1125315),
    sqlFile = testthat::test_path("..", "..", "inst", "sql", "sql_server", "getIngredients.sql")
  )
  expect_true("conceptId" %in% names(ingredients))
})
