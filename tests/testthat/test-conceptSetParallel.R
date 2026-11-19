# A builder that runs on a worker, so it must not depend on test helpers or the test environment.
parallelStubBuilder <- function(failNames = character()) {
  builder <- eval(substitute(function(conceptSetTarget, csDirectory) {
    if (conceptSetTarget %in% failNames) {
      return(list(conceptSet = NULL, isPlaceholder = TRUE, message = "stub failure"))
    }
    conceptSet <- list(items = list(list(
      concept = list(
        CONCEPT_ID = 192671L, CONCEPT_NAME = conceptSetTarget, DOMAIN_ID = "Condition",
        VOCABULARY_ID = "SNOMED", CONCEPT_CLASS_ID = "Clinical Finding", STANDARD_CONCEPT = "S",
        CONCEPT_CODE = "1", INVALID_REASON = "V"
      ),
      isExcluded = FALSE, includeDescendants = TRUE, includeMapped = FALSE
    )))
    list(conceptSet = conceptSet, isPlaceholder = FALSE, message = "")
  }, list(failNames = failNames)))
  environment(builder) <- globalenv()
  builder
}

test_that("parallel creation builds missing sets in workers and writes the library once at the end", {
  mockConceptServices()
  outputDirectory <- setupCreation(c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  conceptSetFolder <- file.path(outputDirectory, "sharedSets")

  createConceptSets(
    outputDirectory, runId = "parallelRun", conceptSetWorkers = 2L,
    conceptSetFolder = conceptSetFolder, conceptSetBuilder = parallelStubBuilder()
  )

  expect_equal(libraryNames(outputDirectory), c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  expect_true(file.exists(file.path(conceptSetFolder, "asthma_diagnosis_codes", "result.rds")))
  expect_true(file.exists(file.path(conceptSetFolder, "copd_diagnosis_codes", "result.rds")))
  expect_true(file.exists(file.path(conceptSetFolder, "conceptSetBuildTimes.csv")))
  history <- libraryHistory(outputDirectory)
  expect_equal(history$sourceFile[history$changeType == "upsertConceptSet"],
               file.path(conceptSetFolder, c("asthma_diagnosis_codes", "copd_diagnosis_codes")))

  matched <- readRDS(file.path(outputDirectory, "COPD", "matchedConceptSets_V1.rds"))
  expect_setequal(matched$libCsName, c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
})

test_that("concept-set dispatch combines historical and heuristic duration estimates", {
  conceptSetFolder <- withr::local_tempdir()
  conceptSetNames <- c(
    "prednisone_medication_codes",
    "corticosteroid_route_potency_medication_codes",
    "abdominal_trauma_diagnosis_codes"
  )

  plan <- Ithaca:::ithConceptSetDispatchPlan(conceptSetNames, conceptSetFolder)
  expect_gt(
    plan$estimates$estimatedSeconds[plan$estimates$conceptSetName == conceptSetNames[[2]]],
    plan$estimates$estimatedSeconds[plan$estimates$conceptSetName == conceptSetNames[[1]]]
  )
  expect_gt(
    plan$estimates$estimatedSeconds[plan$estimates$conceptSetName == conceptSetNames[[3]]],
    plan$estimates$estimatedSeconds[plan$estimates$conceptSetName == conceptSetNames[[1]]]
  )

  utils::write.csv(
    data.frame(conceptSetName = conceptSetNames[[1]], seconds = 1000, outcome = "ok"),
    file.path(conceptSetFolder, "conceptSetBuildTimes.csv"),
    row.names = FALSE
  )
  plan <- Ithaca:::ithConceptSetDispatchPlan(conceptSetNames, conceptSetFolder)
  expect_equal(plan$estimates$estimateSource[plan$estimates$conceptSetName == conceptSetNames[[1]]], "history")
  expect_equal(plan$order[[1]], 1L)
})

test_that("parallel creation reuses finished sets, retries failed ones, and leaves failures out of the library", {
  mockConceptServices()
  outputDirectory <- setupCreation(c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  conceptSetFolder <- file.path(outputDirectory, "sharedSets")
  dir.create(file.path(conceptSetFolder, "asthma_diagnosis_codes"), recursive = TRUE)
  saveRDS(conceptSetWithConcept(), file.path(conceptSetFolder, "asthma_diagnosis_codes", "result.rds"))

  warnings <- testthat::capture_warnings(
    createConceptSets(
      outputDirectory, conceptSetWorkers = 2L, conceptSetFolder = conceptSetFolder,
      conceptSetBuilder = parallelStubBuilder(failNames = "copd diagnosis codes")
    )
  )
  expect_true(any(grepl("could not be built", warnings)))

  expect_equal(libraryNames(outputDirectory), "asthma_diagnosis_codes")
  expect_false(file.exists(file.path(conceptSetFolder, "copd_diagnosis_codes", "result.rds")))
  expect_true(file.exists(file.path(conceptSetFolder, "copd_diagnosis_codes", "error.txt")))

  createConceptSets(
    outputDirectory, conceptSetWorkers = 2L, conceptSetFolder = conceptSetFolder,
    conceptSetBuilder = parallelStubBuilder()
  )
  expect_equal(libraryNames(outputDirectory), c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
})

test_that("session database options are copied to the workers", {
  withr::local_options(sqlRenderTempEmulationSchema = "scratch.testSchema")
  outputDirectory <- withr::local_tempdir()
  builder <- function(conceptSetTarget, csDirectory) {
    list(
      conceptSet = list(items = list(getOption("sqlRenderTempEmulationSchema", "notSet"))),
      isPlaceholder = FALSE, message = ""
    )
  }
  environment(builder) <- globalenv()

  Ithaca:::ithBuildConceptSetsParallel("a_b", outputDirectory, workers = 1L, builder = builder, verbose = FALSE)

  stored <- readRDS(file.path(outputDirectory, "a_b", "result.rds"))
  expect_equal(stored$items[[1]], "scratch.testSchema")
})

test_that("a shared concept set folder is also used by serial builds", {
  state <- mockConceptServices()
  outputDirectory <- setupCreation("asthma_diagnosis_codes")
  conceptSetFolder <- file.path(outputDirectory, "sharedSets")

  createConceptSets(outputDirectory, conceptSetFolder = conceptSetFolder)

  expect_equal(state$builderCalls[[1]]$outputDirectory, file.path(conceptSetFolder, "asthma_diagnosis_codes"))
})
