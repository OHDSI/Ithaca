emptyBuilder <- function(args) list(finalConceptSet = list(items = list()))

emptySetEmbeddings <- function() {
  fakeEmbeddingFunction(list(
    asthma_diagnosis_codes = c(1, 0, 0), "asthma diagnosis codes" = c(1, 0, 0),
    levothyroxine_brand_medication_codes = c(0, 1, 0), "levothyroxine brand medication codes" = c(0, 1, 0),
    levothyroxine_medication_codes = c(0, 0, 1), "levothyroxine medication codes" = c(0, 0, 1)
  ))
}

libraryConceptSetNamed <- function(outputDirectory, name) {
  library <- libraryConceptSets(outputDirectory)[[1]]
  library$conceptSet[[which(library$conceptSetName == name)]]
}

# A COPD rubric whose ER3 rule uses asthma_diagnosis_codes.
emptySetFixture <- function(env = parent.frame()) {
  outputDirectory <- makeRubricFixture(ruleIds = c("ER3", "AE1"), env = env)
  shortRubric <- Ithaca:::ithLoadLatestShortRubric("COPD", outputDirectory)
  shortRubric$neededConceptSets <- c("asthma_diagnosis_codes", "other_diagnosis_codes")
  shortRubric$sqlStatement <- c("select 1 from @asthma_diagnosis_codes", "select 1 from @other_diagnosis_codes")
  Ithaca:::ithSaveShortRubric(shortRubric, "COPD", outputDirectory, yesCombinationReport = FALSE)
  writeNeededConcepts(outputDirectory, "COPD", c("asthma_diagnosis_codes", "other_diagnosis_codes"))
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "other_diagnosis_codes", embedding = c(0, 0, 1))
  outputDirectory
}

test_that("simplified names drop only a qualifier word and keep the head word and suffix", {
  simplify <- Ithaca:::ithSimplifiedConceptSetNames
  expect_equal(simplify("levothyroxine_brand_medication_codes"), "levothyroxine_medication_codes")
  expect_equal(simplify("Levothyroxine_Brand_MEDICATION_CODES"), "levothyroxine_medication_codes")
  expect_equal(simplify("metastatic_liver_disease_diagnosis_codes"), character())
  expect_equal(simplify("brand_diagnosis_codes"), character())
  expect_equal(simplify("asthma_diagnosis_codes"), character())
})

test_that("an empty build is recorded as known empty, skipped on rerun, and summarised", {
  state <- mockConceptServices(builder = emptyBuilder)
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  createConceptSets(outputDirectory, runId = "emptyRun")

  expect_length(state$builderCalls, 1L)
  expect_true(Ithaca:::ithIsKnownEmptyConceptSet(libraryConceptSetNamed(outputDirectory, "asthma_diagnosis_codes")))
  history <- libraryHistory(outputDirectory)
  expect_match(history$sourceFile[history$changeType == "upsertConceptSet"], "^knownEmpty: empty")
  recordPath <- file.path(outputDirectory, "COPD", "asthma_diagnosis_codes", "knownEmpty.json")
  expect_true(file.exists(recordPath))
  expect_equal(jsonlite::read_json(recordPath)$outcome, "empty")
  matched <- readRDS(file.path(outputDirectory, "COPD", "matchedConceptSets_V1.rds"))
  expect_equal(matched$csName, "asthma_diagnosis_codes")
  summaryTable <- utils::read.csv(file.path(outputDirectory, "conceptSetBuildSummary_V1.csv"), stringsAsFactors = FALSE)
  expect_equal(summaryTable$status, "knownEmpty")

  createConceptSets(outputDirectory)
  expect_length(state$builderCalls, 1L)
})

test_that("an unreadable result is retried once", {
  calls <- 0L
  state <- mockConceptServices(builder = function(args) {
    calls <<- calls + 1L
    if (calls == 1L) list() else list(finalConceptSet = conceptSetWithConcept())
  })
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  createConceptSets(outputDirectory)

  expect_length(state$builderCalls, 2L)
  expect_false(Ithaca:::ithIsKnownEmptyConceptSet(libraryConceptSetNamed(outputDirectory, "asthma_diagnosis_codes")))
})

test_that("an empty set is aliased to an existing set under a simplified name", {
  state <- mockConceptServices(builder = emptyBuilder, embeddings = emptySetEmbeddings())
  outputDirectory <- setupCreation("levothyroxine_brand_medication_codes")
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "levothyroxine_medication_codes", embedding = c(0, 0, 1))

  createConceptSets(outputDirectory, fuzzyThreshold = 0.999, similarityThreshold = 0.999)

  expect_length(state$builderCalls, 1L)
  expect_equal(libraryNames(outputDirectory), "levothyroxine_medication_codes")
  matched <- readRDS(file.path(outputDirectory, "COPD", "matchedConceptSets_V1.rds"))
  expect_equal(matched$csName, "levothyroxine_brand_medication_codes")
  expect_equal(matched$libCsName, "levothyroxine_medication_codes")
  summaryTable <- utils::read.csv(file.path(outputDirectory, "conceptSetBuildSummary_V1.csv"), stringsAsFactors = FALSE)
  expect_equal(summaryTable$status, "aliased")
  expect_equal(summaryTable$aliasTarget, "levothyroxine_medication_codes")
})

test_that("emptyConceptSet = 'stop' stops after the run without recording the set", {
  mockConceptServices(builder = emptyBuilder)
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  expect_error(
    suppressWarnings(createConceptSets(outputDirectory, emptyConceptSet = "stop")),
    "came back empty"
  )
  expect_equal(nrow(libraryConceptSets(outputDirectory)[[1]]), 0L)
})

test_that("rules using a known-empty set are flagged okToUse = FALSE, readiness warns, and a rebuild restores them", {
  mockConceptServices(builder = emptyBuilder)
  outputDirectory <- emptySetFixture()

  createConceptSets(outputDirectory)

  shortRubric <- Ithaca:::ithLoadLatestShortRubric("COPD", outputDirectory)
  expect_equal(shortRubric$okToUse[shortRubric$ruleId == "ER3"], FALSE)
  expect_equal(shortRubric$okToUse[shortRubric$ruleId == "AE1"], TRUE)
  expect_equal(shortRubric$sqlStatement[shortRubric$ruleId == "ER3"], "select 1 from @asthma_diagnosis_codes")
  sidecar <- Ithaca:::ithLoadLatest(file.path(outputDirectory, "COPD"), "COPD_emptyConceptSetRules.csv")
  expect_equal(sidecar$ruleId, "ER3")

  expect_warning(
    Ithaca:::ithCheckConceptSetsReadyForScoring(list(list(rubricName = "COPD")), outputDirectory),
    "recorded as empty"
  )

  mockConceptServices(builder = function(args) list(finalConceptSet = conceptSetWithConcept()))
  createConceptSets(outputDirectory, rebuildEmptyConceptSets = TRUE)

  expect_false(Ithaca:::ithIsKnownEmptyConceptSet(libraryConceptSetNamed(outputDirectory, "asthma_diagnosis_codes")))
  shortRubric <- Ithaca:::ithLoadLatestShortRubric("COPD", outputDirectory)
  expect_true(all(shortRubric$okToUse))
  expect_no_warning(
    Ithaca:::ithCheckConceptSetsReadyForScoring(list(list(rubricName = "COPD")), outputDirectory)
  )
})
