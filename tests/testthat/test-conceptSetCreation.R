test_that("missing concept sets are built, saved to the library, and matched", {
  state <- mockConceptServices()
  outputDirectory <- setupCreation(c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  llm <- makeFakeLlm()

  createConceptSets(outputDirectory, llm = llm, runId = "testRun")

  expect_equal(libraryNames(outputDirectory), c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  expect_equal(vapply(state$builderCalls, function(x) x$conceptSetTarget, character(1)),
               c("asthma diagnosis codes", "copd diagnosis codes"))
  expect_equal(state$builderCalls[[1]]$clinicalContext, "any clinical context")
  expect_identical(state$builderCalls[[1]]$llmClientReasoning, llm)
  expect_equal(state$builderCalls[[1]]$outputDirectory, file.path(outputDirectory, "COPD", "asthma_diagnosis_codes"))

  history <- libraryHistory(outputDirectory)
  expect_equal(max(history$libraryVersion), 3)
  expect_equal(history$runId[history$changeType == "upsertConceptSet"], c("testRun", "testRun"))

  matched <- readRDS(file.path(outputDirectory, "COPD", "matchedConceptSets_V1.rds"))
  expect_setequal(matched$csName, c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  expect_setequal(matched$libCsName, c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
})

test_that("an existing readable library concept set is reused and not rebuilt", {
  state <- mockConceptServices()
  outputDirectory <- setupCreation(c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "Asthma_DIAGNOSIS_CODES", embedding = c(1, 0, 0))

  createConceptSets(outputDirectory)

  expect_equal(vapply(state$builderCalls, function(x) x$conceptSetTarget, character(1)), "copd diagnosis codes")
  expect_equal(max(libraryHistory(outputDirectory)$libraryVersion), 3)
  matched <- readRDS(file.path(outputDirectory, "COPD", "matchedConceptSets_V1.rds"))
  expect_equal(matched$libCsName[matched$csName == "asthma_diagnosis_codes"], "Asthma_DIAGNOSIS_CODES")
})

test_that("a matched concept set with no usable concepts is rebuilt in place", {
  state <- mockConceptServices(builder = function(args) {
    list(finalConceptSet = conceptSetWithConcept(4112343L, "Acute viral pharyngitis"))
  })
  outputDirectory <- setupCreation("asthma_diagnosis_codes")
  Ithaca:::ithDuckdbInitializeLibrary(libraryPathFor(outputDirectory), verbose = FALSE)
  addLibraryConceptSet(outputDirectory, "Asthma_DIAGNOSIS_CODES", conceptSet = list(items = list()))

  createConceptSets(outputDirectory)

  expect_length(state$builderCalls, 1L)
  library <- libraryConceptSets(outputDirectory)[[1]]
  items <- library$conceptSet[[which(library$conceptSetName == "Asthma_DIAGNOSIS_CODES")]]$items
  expect_equal(items[[1]]$concept$CONCEPT_ID, 4112343)
})

test_that("a failing builder leaves the set out of the library so a rerun builds it again", {
  state <- mockConceptServices(builder = function(args) stop("builder exploded"))
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  result <- captureMessages(
    warnings <- testthat::capture_warnings(createConceptSets(outputDirectory, verbose = TRUE))
  )

  expect_true(any(grepl("Using placeholder conceptId 36433327", result$messages, fixed = TRUE)))
  expect_true(any(grepl("could not be created or matched", warnings)))
  expect_equal(nrow(libraryConceptSets(outputDirectory)[[1]]), 0L)
})

test_that("a builder that returns nothing usable also gets the placeholder", {
  mockConceptServices(builder = function(args) list())
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  result <- captureMessages(createConceptSets(outputDirectory, verbose = TRUE))

  expect_true(any(grepl("empty/unreadable concept set", result$messages, fixed = TRUE)))
  expect_equal(libraryConceptSets(outputDirectory)[[1]]$conceptSet[[1]]$items[[1]]$concept$CONCEPT_NAME, "NO_CONCEPT_PLACEHOLDER")
})

test_that("an embedding failure still saves the concept set without an embedding", {
  mockConceptServices(embeddings = fakeEmbeddingFunction(list()))
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  result <- captureMessages(createConceptSets(outputDirectory, verbose = TRUE))

  expect_true(any(grepl("Embedding generation failed", result$messages, fixed = TRUE)))
  library <- libraryConceptSets(outputDirectory)
  expect_equal(as.character(library[[1]]$conceptSetName), "asthma_diagnosis_codes")
  expect_true(is.na(library[[2]]$embed[[1]][[1]]))
})

test_that("a malformed name that matches one valid name is repaired before building", {
  state <- mockConceptServices()
  outputDirectory <- setupCreation(c("p pulmonology_provider_codes", "pulmonology_provider_codes"))

  result <- captureMessages(createConceptSets(outputDirectory, verbose = TRUE))

  expect_true(any(grepl("Repaired concept-set name", result$messages, fixed = TRUE)))
  expect_length(state$builderCalls, 1L)
  expect_equal(libraryNames(outputDirectory), "pulmonology_provider_codes")
})

test_that("names that cannot be repaired stop the run before anything is built", {
  state <- mockConceptServices()
  outputDirectory <- setupCreation("bad name!")

  expect_error(createConceptSets(outputDirectory), "Invalid concept-set names in the rubric for 'COPD'")
  expect_length(state$builderCalls, 0L)
  expect_false(file.exists(libraryPathFor(outputDirectory)) && nrow(libraryConceptSets(outputDirectory)[[1]]) > 0)
})

test_that("a condition without a rubric list is an error", {
  mockConceptServices()
  outputDirectory <- withr::local_tempdir()

  expect_error(createConceptSets(outputDirectory), "No rubricList found for condition: COPD")
})

test_that("rerunning creates nothing new and saves a new match file", {
  state <- mockConceptServices()
  outputDirectory <- setupCreation(c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  createConceptSets(outputDirectory)
  versionAfterFirst <- max(libraryHistory(outputDirectory)$libraryVersion)

  createConceptSets(outputDirectory)

  expect_length(state$builderCalls, 2L)
  expect_equal(max(libraryHistory(outputDirectory)$libraryVersion), versionAfterFirst)
  expect_true(file.exists(file.path(outputDirectory, "COPD", "matchedConceptSets_V2.rds")))
})

test_that("match diagnostics are saved before and after creation", {
  mockConceptServices()
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  createConceptSets(outputDirectory)

  before <- utils::read.csv(file.path(outputDirectory, "COPD", "conceptSetMatchDiagnostics_V1.csv"), stringsAsFactors = FALSE)
  after <- utils::read.csv(file.path(outputDirectory, "COPD", "conceptSetMatchDiagnostics_V2.csv"), stringsAsFactors = FALSE)
  expect_equal(before$phase, "beforeCreateMissing")
  expect_equal(before$matchMethod, "none")
  expect_equal(after$phase, "afterCreateMissing")
  expect_equal(after$matchMethod, "exact")
  expect_equal(after$conditionName, "COPD")
})

test_that("the run ID defaults to a timestamp and is recorded with the library change", {
  mockConceptServices()
  outputDirectory <- setupCreation("asthma_diagnosis_codes")

  createConceptSets(outputDirectory)

  history <- libraryHistory(outputDirectory)
  expect_match(history$runId[history$changeType == "upsertConceptSet"], "^createRubricConceptSets_[0-9]{14}$")
  expect_equal(history$conditionName[history$changeType == "upsertConceptSet"], "COPD")
})

test_that("a second condition reuses concept sets the first one created", {
  state <- mockConceptServices()
  outputDirectory <- withr::local_tempdir()
  writeNeededConcepts(outputDirectory, "COPD", c("asthma_diagnosis_codes", "copd_diagnosis_codes"))
  writeNeededConcepts(outputDirectory, "Bronchiectasis", "asthma_diagnosis_codes")

  createConceptSets(outputDirectory, conditions = c("COPD", "Bronchiectasis"))

  expect_length(state$builderCalls, 2L)
  matched <- readRDS(file.path(outputDirectory, "Bronchiectasis", "matchedConceptSets_V1.rds"))
  expect_equal(matched$libCsName, "asthma_diagnosis_codes")
})

test_that("concept sets created for a rubric resolve to real concepts in the CDM", {
  database <- makeEunomiaDatabase()
  mockConceptServices(embeddings = creationEmbeddings())
  outputDirectory <- setupCreation("gi_hemorrhage_diagnosis_codes")

  createConceptSets(outputDirectory, database = database)

  matched <- readRDS(file.path(outputDirectory, "COPD", "matchedConceptSets_V1.rds"))
  resolution <- Ithaca:::ithResolveConceptSetPluginDetails(
    pluginName = "gi_hemorrhage_diagnosis_codes", matchedConceptSets = matched,
    conceptSetJsonLibrary = libraryConceptSets(outputDirectory)[[1]], database = database
  )

  expect_match(resolution$status, "^resolved")
  expect_true("192671" %in% strsplit(resolution$value, ",\\s*")[[1]])
  expect_gte(resolution$resolvedConceptCount, 1L)
})
