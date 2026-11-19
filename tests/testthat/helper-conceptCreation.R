# Fake embeddings for the names these tests use; anything else is an error so surprises show up.
creationEmbeddings <- function() {
  fakeEmbeddingFunction(list(
    asthma = c(1, 0, 0), "asthma diagnosis codes" = c(1, 0, 0),
    copd = c(0, 1, 0), "copd diagnosis codes" = c(0, 1, 0),
    pulmonology_provider_codes = c(0, 0, 1), "pulmonology provider codes" = c(0, 0, 1),
    "gi hemorrhage diagnosis codes" = c(0.6, 0.8, 0), "gi_hemorrhage_diagnosis_codes" = c(0.6, 0.8, 0),
    gi_hemorrhage = c(0.6, 0.8, 0)
  ))
}

# Replaces the concept set builder, the embedding service, and Hecate for one test.
mockConceptServices <- function(env = parent.frame(), builder = NULL, embeddings = creationEmbeddings()) {
  state <- new.env()
  state$builderCalls <- list()
  state$hecateTargets <- character()
  buildResult <- if (is.null(builder)) function(args) list(finalConceptSet = conceptSetWithConcept()) else builder

  mock <- function() {
    args <- as.list(environment())
    args <- args[!vapply(args, function(x) identical(x, quote(expr = )), logical(1))]
    state$builderCalls[[length(state$builderCalls) + 1L]] <- args
    buildResult(args)
  }
  formals(mock) <- formals(RubricConcepts::createConceptSet)

  testthat::local_mocked_bindings(createConceptSet = mock, .package = "RubricConcepts", .env = env)
  testthat::local_mocked_bindings(
    ithGetEmbeddingVectorsInternal = embeddings,
    ithGetHecateCandidateDetails = function(targetName, limit = 5, llmClient = NULL) {
      state$hecateTargets <- c(state$hecateTargets, targetName)
      list(searchTerm = "", candidates = character(), rawCandidates = character(),
           rejectedCandidates = character(), usedFallback = FALSE)
    },
    .package = "Ithaca", .env = env
  )
  state
}

createConceptSets <- function(outputDirectory,
                              conditions = "COPD",
                              llm = makeFakeLlm(),
                              database = list(connectionDetails = "unused", cdmDatabaseSchema = "main"),
                              verbose = FALSE,
                              ...) {
  specs <- lapply(seq_along(conditions), function(i) list(testCondition = conditions[[i]], randomCohortDefinitionId = i))
  Ithaca:::ithCreateRubricConceptSets(
    conditionSpecs = specs, outputDirectory = outputDirectory, database = database,
    llmClientReasoning = llm, llmClientNonReasoning = llm, verbose = verbose, ...
  )
}

setupCreation <- function(conceptSetNames, env = parent.frame()) {
  outputDirectory <- withr::local_tempdir(.local_envir = env)
  writeNeededConcepts(outputDirectory, "COPD", conceptSetNames)
  outputDirectory
}

libraryNames <- function(outputDirectory) {
  sort(as.character(libraryConceptSets(outputDirectory)[[1]]$conceptSetName))
}
