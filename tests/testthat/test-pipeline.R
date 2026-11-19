pipelineConfigText <- function(outputDirectory, databases = c(Ccae = "cdm_ccae_v1"), conditions = c("GERD"),
                                databaseVersion = NULL) {
  cdmLines <- paste0("    ", names(databases), ": ", databases, collapse = "\n")
  conditionLines <- paste(vapply(seq_along(conditions), function(i) {
    paste0(
      "    - testCondition: ", conditions[[i]], "\n",
      "      rubricName: ", conditions[[i]], "\n",
      "      dxCriteria: outpatientOnly\n",
      "      additionalInformation: \"\"\n",
      "      randomCohortDefinitionId: ", 900 + i, "\n",
      "      sourceCohortDefinitionId: ", 800 + i, "\n",
      "      phenotypeCohortId: [", 100 + i, ", ", 200 + i, "]"
    )
  }, character(1)), collapse = "\n")
  versionLines <- if (is.null(databaseVersion)) "" else paste0(
    "  databaseVersion:\n", paste0("    ", names(databaseVersion), ": ", databaseVersion, collapse = "\n"), "\n"
  )
  paste0(
    "general:\n",
    "  outputDirectory: ", normalizePath(outputDirectory, winslash = "/"), "\n",
    "  webapiBaseUrl: \"https://example.org/WebAPI\"\n",
    "  verbose: false\n",
    "  cdm:\n", cdmLines, "\n",
    versionLines,
    "  dxCriteria:\n",
    "    outpatientOnly:\n",
    "      criteria: \"Two outpatient codes.\"\n",
    "      additionalPoints: 0\n",
    "  conditionSpecs:\n", conditionLines, "\n",
    "randomCohort:\n",
    "  sampleSize: 1000\n",
    "phenotypeReportScoring:\n",
    "  default:\n",
    "    analysisName: base\n",
    "    splayPrior: 30\n",
    "    splayPost: 30\n",
    "    sampleSubjectCount: 10\n",
    "    frequencyPlotFpFnScores: false\n",
    "    createSubjectSample: false\n",
    "    classLabels: [\"yes\", \"no\"]\n"
  )
}

# Sets up a temporary project with mocked step runners; `calls` records every executed unit.
setupPipeline <- function(env = parent.frame(), ...) {
  outputDirectory <- withr::local_tempdir(.local_envir = env)
  state <- new.env()
  state$calls <- character(0)
  state$definitions <- c("801" = "a", "802" = "a", "101" = "a", "201" = "a", "102" = "a", "202" = "a")
  state$codeHashes <- c(cohort = "c1", score = "s1", report = "r1")
  state$failScore <- character(0)
  state$configPath <- file.path(outputDirectory, "config.yaml")
  state$write <- function(...) writeLines(pipelineConfigText(outputDirectory, ...), state$configPath)
  state$write(...)
  state$outputDirectory <- outputDirectory

  local_mocked_bindings(
    ithPipelineFetchDefinitionHashes = function(cohortIds, webapiBaseUrl) state$definitions[as.character(cohortIds)],
    ithPipelineStepCodeHash = function(step) state$codeHashes[[step]],
    ithPipelineLoadConceptLibrary = function(config) NULL,
    ithPipelineRunCohortUnits = function(configPath, units, envir, onUnitDone = NULL) {
      for (index in seq_len(nrow(units))) {
        state$calls <- c(state$calls, paste("cohort", units$conditionName[[index]], units$databaseId[[index]]))
        if (!is.null(onUnitDone)) onUnitDone(index, NULL)
      }
    },
    ithPipelineRunScoreUnit = function(configPath, conditionName, databaseId, envir) {
      state$calls <- c(state$calls, paste("score", conditionName, databaseId))
      if (databaseId %in% state$failScore) stop("score exploded")
    },
    ithPipelineGeneratePhenotypeCohorts = function(configPath, cohortIdsByDatabase, envir) list(),
    ithPipelineRunReportGroup = function(configPath, units, phenotypePopulationCache, envir) {
      state$calls <- c(state$calls, paste("report", units$conditionName, units$databaseId, units$phenotypeCohortId))
      data.frame(
        phenotype = units$conditionName, databaseId = units$databaseId, cohortId = units$phenotypeCohortId,
        analysisName = units$analysisName, conditionName = units$conditionName, testCondition = units$conditionName,
        reportStatus = "ok", sensitivity = 0.5, stringsAsFactors = FALSE
      )
    },
    .env = env
  )
  state
}

runPipeline <- function(state, ...) {
  state$calls <- character(0)
  suppressMessages(runIthacaPipeline(configPath = state$configPath, ...))
}

test_that("the first run runs every unit and the second run skips them all", {
  state <- setupPipeline()

  first <- runPipeline(state)
  expect_equal(as.vector(table(first$step)[c("cohort", "score", "report")]), c(1, 1, 2))
  expect_true(all(first$status == "done"))
  expect_setequal(state$calls, c("cohort GERD Ccae", "score GERD Ccae", "report GERD Ccae 101", "report GERD Ccae 201"))
  expect_true(file.exists(file.path(state$outputDirectory, "pipeline", "pipelineLedger.csv")))
  expect_length(list.files(file.path(state$outputDirectory, "scoring"), pattern = "^base_phenotypeAlgorithmRubricReports_V1\\.rds$"), 1)

  second <- runPipeline(state)
  expect_true(all(second$action == "skip"))
  expect_length(state$calls, 0)
})

test_that("a changed phenotype definition reruns only that cohort's report", {
  state <- setupPipeline()
  runPipeline(state)

  state$definitions[["201"]] <- "b"
  result <- runPipeline(state)

  expect_equal(state$calls, "report GERD Ccae 201")
  rerun <- result[result$action == "run", ]
  expect_equal(rerun$phenotypeCohortId, "201")
  expect_match(rerun$reason, "phenotype cohort definition changed")
})

test_that("a changed source definition reruns the cohort and everything downstream", {
  state <- setupPipeline()
  runPipeline(state)

  state$definitions[["801"]] <- "b"
  result <- runPipeline(state)

  expect_setequal(state$calls, c("cohort GERD Ccae", "score GERD Ccae", "report GERD Ccae 101", "report GERD Ccae 201"))
  expect_match(result$reason[result$step == "cohort"], "source cohort definition changed")
  expect_match(result$reason[result$step == "score"], "evaluation cohort was rebuilt")
})

test_that("a new database or condition runs only the new units", {
  state <- setupPipeline()
  runPipeline(state)

  state$write(databases = c(Ccae = "cdm_ccae_v1", Mdcd = "cdm_mdcd_v1"))
  runPipeline(state)
  expect_setequal(state$calls, c("cohort GERD Mdcd", "score GERD Mdcd", "report GERD Mdcd 101", "report GERD Mdcd 201"))

  state$write(databases = c(Ccae = "cdm_ccae_v1", Mdcd = "cdm_mdcd_v1"), conditions = c("GERD", "COPD"))
  runPipeline(state)
  expect_true(all(grepl(" COPD ", state$calls)))
  expect_length(state$calls, 2 * (2 + 2))
})

test_that("a database version change reruns that database only", {
  state <- setupPipeline(databases = c(Ccae = "cdm_ccae_v1", Mdcd = "cdm_mdcd_v1"))
  runPipeline(state)

  state$write(databases = c(Ccae = "cdm_ccae_v2", Mdcd = "cdm_mdcd_v1"))
  result <- runPipeline(state)
  expect_true(all(grepl("Ccae", state$calls)))
  expect_match(result$reason[result$step == "cohort" & result$databaseId == "Ccae"], "database or database version changed")

  state$write(databases = c(Ccae = "cdm_ccae_v2", Mdcd = "cdm_mdcd_v1"), databaseVersion = c(Mdcd = "2025q1"))
  runPipeline(state)
  expect_true(all(grepl("Mdcd", state$calls)))
  expect_length(state$calls, 4)
})

test_that("a code change in one step reruns that step and its dependants", {
  state <- setupPipeline()
  runPipeline(state)

  state$codeHashes[["report"]] <- "r2"
  result <- runPipeline(state)
  expect_setequal(state$calls, c("report GERD Ccae 101", "report GERD Ccae 201"))
  expect_true(all(grepl("code", result$reason[result$action == "run"])))

  state$codeHashes[["score"]] <- "s2"
  runPipeline(state)
  expect_setequal(state$calls, c("score GERD Ccae", "report GERD Ccae 101", "report GERD Ccae 201"))
})

test_that("a failed unit blocks its dependants and is retried on the next run", {
  state <- setupPipeline()
  state$failScore <- "Ccae"

  expect_warning(result <- runPipeline(state), "score exploded")
  expect_equal(result$status[result$step == "score"], "failed")
  expect_equal(unique(result$action[result$step == "report"]), "blocked")
  expect_false(any(startsWith(state$calls, "report")))

  state$failScore <- character(0)
  result <- runPipeline(state)
  expect_setequal(state$calls, c("score GERD Ccae", "report GERD Ccae 101", "report GERD Ccae 201"))
  expect_equal(result$reason[result$step == "score"], "last run failed")
})

test_that("fresh reruns the requested steps and dryRun runs nothing", {
  state <- setupPipeline()
  runPipeline(state)

  runPipeline(state, fresh = "score")
  expect_setequal(state$calls, c("score GERD Ccae", "report GERD Ccae 101", "report GERD Ccae 201"))

  state$definitions[["101"]] <- "b"
  plan <- runPipeline(state, dryRun = TRUE, fresh = TRUE)
  expect_length(state$calls, 0)
  expect_true(all(plan$status == "planned"))
  expect_true(all(plan$action == "run"))

  plan <- runPipeline(state, dryRun = TRUE)
  expect_equal(plan$phenotypeCohortId[plan$action == "run"], "101")
})

test_that("dryRun shows units downstream of a planned run as needing to run", {
  state <- setupPipeline()
  runPipeline(state)

  state$definitions[["801"]] <- "b"
  plan <- runPipeline(state, dryRun = TRUE)
  expect_true(all(plan$action == "run"))
  expect_match(plan$reason[plan$step == "report"][[1]], "scores were recomputed")
})

test_that("adoptExisting records existing outputs instead of rerunning them", {
  state <- setupPipeline()
  scoringDirectory <- file.path(state$outputDirectory, "scoring", "GERD", "Ccae")
  dir.create(scoringDirectory, recursive = TRUE)
  saveRDS(data.frame(), file.path(scoringDirectory, "sumCohortScores_Cohort901_V1.rds"))
  saveRDS(data.frame(), file.path(scoringDirectory, "fullScoringFile_Cohort901_V1.rds"))
  saveRDS(
    data.frame(databaseId = "Ccae", cohortId = 101L, analysisName = "base", sensitivity = 0.9),
    file.path(state$outputDirectory, "scoring", "GERD", "GERD_base_phenotypeAlgorithmRubricReports_V1.rds")
  )

  result <- runPipeline(state, adoptExisting = TRUE)
  expect_equal(state$calls, "report GERD Ccae 201")
  expect_equal(result$action[result$step != "report" | result$phenotypeCohortId == "101"], rep("adopt", 3))

  combined <- readRDS(file.path(state$outputDirectory, "scoring", "base_phenotypeAlgorithmRubricReports_V1.rds"))
  expect_setequal(combined$cohortId, c(101L, 201L))
  expect_equal(combined$sensitivity[combined$cohortId == 101L], 0.9)

  runPipeline(state)
  expect_length(state$calls, 0)
})

test_that("adoptExisting does not adopt a report that is older than its scores", {
  state <- setupPipeline()
  scoringDirectory <- file.path(state$outputDirectory, "scoring", "GERD", "Ccae")
  dir.create(scoringDirectory, recursive = TRUE)
  reportPath <- file.path(state$outputDirectory, "scoring", "GERD", "GERD_base_phenotypeAlgorithmRubricReports_V1.rds")
  saveRDS(data.frame(databaseId = "Ccae", cohortId = c(101L, 201L), analysisName = "base", sensitivity = 0.9), reportPath)
  saveRDS(data.frame(), file.path(scoringDirectory, "sumCohortScores_Cohort901_V1.rds"))
  saveRDS(data.frame(), file.path(scoringDirectory, "fullScoringFile_Cohort901_V1.rds"))
  Sys.setFileTime(reportPath, Sys.time() - 3600)

  result <- runPipeline(state, adoptExisting = TRUE)
  expect_equal(result$action[result$step != "report"], c("adopt", "adopt"))
  expect_setequal(state$calls, c("report GERD Ccae 101", "report GERD Ccae 201"))
})

test_that("an unreachable WebAPI reuses the last definition hashes", {
  state <- setupPipeline()
  runPipeline(state)

  local_mocked_bindings(ithPipelineFetchDefinitionHashes = function(cohortIds, webapiBaseUrl) stop("offline"))
  expect_warning(result <- runPipeline(state), "last fetched definition hashes")
  expect_true(all(result$action == "skip"))
})

test_that("the code hash ignores comments but sees code and called-function changes", {
  namespace <- new.env()
  evalCode <- function(text) {
    for (name in ls(namespace)) rm(list = name, envir = namespace)
    eval(parse(text = text, keep.source = TRUE), envir = namespace)
    Ithaca:::ithPipelineCodeHash("entry", namespace)
  }

  base <- evalCode("helper <- function(x) {\n  x + 1\n}\nentry <- function(x) helper(x)\nunused <- function() 1")
  expect_equal(evalCode("helper <- function(x) {\n  # a comment\n  x   +   1\n}\nentry <- function(x) helper(x)\nunused <- function() 1"), base)
  expect_equal(evalCode("helper <- function(x) {\n  x + 1\n}\nentry <- function(x) helper(x)\nunused <- function() 2"), base)
  expect_false(identical(evalCode("helper <- function(x) {\n  x + 2\n}\nentry <- function(x) helper(x)\nunused <- function() 1"), base))
})

test_that("random cohort building from config reports which condition and database it is on", {
  outputDirectory <- withr::local_tempdir()
  configPath <- file.path(outputDirectory, "config.yaml")
  writeLines(pipelineConfigText(outputDirectory, databases = c(Ccae = "cdm_ccae_v1", Optum = "cdm_optum_v1"), conditions = c("GERD", "Asthma")), configPath)
  calls <- character(0)
  testthat::local_mocked_bindings(
    ithResolveDatabricksConnectionDetails = function(...) list(dbms = "spark"),
    ithResolveDatabasesFromConfig = function(config, envir) {
      list(
        Ccae = list(databaseId = "Ccae", cdmDatabaseSchema = "cdm_ccae_v1", cohortTable = "ithaca_ccae"),
        Optum = list(databaseId = "Optum", cdmDatabaseSchema = "cdm_optum_v1", cohortTable = "ithaca_optum")
      )
    },
    buildRandomOmopCohort = function(database, cohort_definition_id, ...) {
      calls <<- c(calls, paste(database$cdmDatabaseSchema, cohort_definition_id))
      invisible(NULL)
    },
    .package = "Ithaca"
  )

  messages <- testthat::capture_messages(buildRandomOmopCohortFromConfig(configPath))

  expect_equal(calls, c("cdm_ccae_v1 901", "cdm_optum_v1 901", "cdm_ccae_v1 902", "cdm_optum_v1 902"))
  starts <- grep("^=== \\[.*\\] Random cohort", messages, value = TRUE)
  expect_length(starts, 4L)
  expect_match(starts[[1]], "Random cohort 1/4: condition 'GERD' on database 'Ccae' (source cohort 801 -> random cohort 901", fixed = TRUE)
  expect_match(starts[[4]], "Random cohort 4/4: condition 'Asthma' on database 'Optum' (source cohort 802 -> random cohort 902", fixed = TRUE)
  expect_length(grep("Finished random cohort for condition 'Asthma' on database 'Optum' in", messages, fixed = TRUE), 1L)
})

test_that("random cohort building from config stops at the failing job and names it", {
  outputDirectory <- withr::local_tempdir()
  configPath <- file.path(outputDirectory, "config.yaml")
  writeLines(pipelineConfigText(outputDirectory, databases = c(Ccae = "cdm_ccae_v1"), conditions = c("GERD", "Asthma", "Gout")), configPath)
  calls <- character(0)
  testthat::local_mocked_bindings(
    ithResolveDatabricksConnectionDetails = function(...) list(dbms = "spark"),
    ithResolveDatabasesFromConfig = function(config, envir) {
      list(Ccae = list(databaseId = "Ccae", cdmDatabaseSchema = "cdm_ccae_v1", cohortTable = "ithaca_ccae"))
    },
    buildRandomOmopCohort = function(database, cohort_definition_id, ...) {
      calls <<- c(calls, as.character(cohort_definition_id))
      if (cohort_definition_id == 902) stop("warehouse exploded")
      invisible(NULL)
    },
    .package = "Ithaca"
  )

  expect_error(
    suppressMessages(buildRandomOmopCohortFromConfig(configPath)),
    "Random cohort build failed for condition 'Asthma' on database 'Ccae': warehouse exploded"
  )
  expect_equal(calls, c("901", "902"))
})

