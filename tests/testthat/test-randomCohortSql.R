test_that("the random sample pool constrains both subject and visit date", {
  sql <- paste(
    readLines(file.path(testthat::test_path("..", "..", "inst", "sql", "sql_server", "build_random_omop_cohort.sql")), warn = FALSE),
    collapse = "\n"
  )

  expect_match(sql, "EXISTS \\(\\s*SELECT 1", perl = TRUE)
  expect_match(sql, "sample_pool\\.subject_id = vo\\.person_id", perl = TRUE)
  expect_match(sql, "sample_pool\\.cohort_start_date.*vo\\.visit_start_date", perl = TRUE)
  expect_false(grepl("vo\\.person_id IN \\(", sql, fixed = FALSE))
})