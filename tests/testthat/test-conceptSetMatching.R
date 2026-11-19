matchSets <- function(needed, library, ..., embeddings = list(), llm = NULL, adjudicate = FALSE) {
  testthat::local_mocked_bindings(
    ithGetEmbeddingVectorsInternal = fakeEmbeddingFunction(embeddings),
    .package = "Ithaca", .env = parent.frame()
  )
  Ithaca:::ithMatchConceptSets(
    needed, library, returnDiagnostics = TRUE, useLlmAdjudication = adjudicate, llmClient = llm,
    verbose = FALSE, ...
  )
}

copdTarget <- "copd_diagnosis_codes"
copdLibraryName <- "Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES"

test_that("concept-set names are normalized, stripped, and classified", {
  expect_equal(Ithaca:::ithNormalizeConceptSetName("  Chronic  Obstructive-Pulmonary!!_Codes "), "chronic_obstructive_pulmonary_codes")
  expect_equal(Ithaca:::ithNormalizeConceptSetName("asthma_asthma_DIAGNOSIS_CODES"), "asthma_diagnosis_codes")
  expect_equal(Ithaca:::ithStripConceptSetSuffix("Asthma_DIAGNOSIS_CODES"), "asthma")
  expect_equal(Ithaca:::ithStripConceptSetSuffix("Asthma_condition_codes"), "asthma")
  expect_equal(Ithaca:::ithStripConceptSetSuffix("outpatient_visits_visit_codes"), "outpatient_visits_visit")
  expect_equal(Ithaca:::ithNormalizeConceptSetCoreName("Tiotropium_MEDICATION_CODES"), "tiotropium")

  families <- vapply(
    c("a_procedure_codes", "a_diagnosis_codes", "a_condition_codes", "a_medication_codes", "a_drug_codes",
      "a_measurement_codes", "a_lab_codes", "a_visit_codes", "a_observation_codes", "a_device_codes", "plain"),
    Ithaca:::ithInferConceptSetFamily, character(1), USE.NAMES = FALSE
  )
  expect_equal(families, c("procedure", rep("conditionDiagnosis", 2), rep("medicationDrug", 2), rep("labMeasurement", 2),
                           "visit", "observation", "device", "unknown"))
})

test_that("only the same concept-set family is compatible, with unknown treated as compatible", {
  compatible <- Ithaca:::ithIsConceptSetFamilyCompatible

  expect_true(compatible("conditionDiagnosis", "conditionDiagnosis"))
  expect_false(compatible("conditionDiagnosis", "medicationDrug"))
  expect_true(compatible("unknown", "medicationDrug"))
  expect_true(compatible("visit", "unknown"))
  expect_true(compatible(NA_character_, "visit"))
})

test_that("name similarity rewards shared tokens and small edits", {
  similarity <- Ithaca:::ithComputeFuzzyNameSimilarity

  expect_equal(similarity("asthma", "asthma"), 1)
  expect_equal(similarity("", "asthma"), 0)
  expect_gt(similarity("asthmaa", "asthma"), 0.8)
  expect_gt(similarity("chronic_obstructive_lung", "chronic_obstructive_disease"), 0.5)
  expect_lt(similarity("asthma", "femur_fracture"), 0.4)
  expect_setequal(
    Ithaca:::ithGenerateFuzzyNameVariants("a_condition_codes"),
    c("a_condition_codes", "a_diagnosis_codes")
  )
})

test_that("an exact name match ignores case and condition versus diagnosis wording, without embeddings", {
  library <- makeEmbeddingLibrary(Asthma_DIAGNOSIS_CODES = c(1, 0, 0))

  result <- matchSets("asthma_condition_codes", library)

  expect_equal(result$matches$libCsName, "Asthma_DIAGNOSIS_CODES")
  expect_equal(result$matches$coSim, 1)
  expect_equal(result$diagnostics$matchMethod, "exact")
})

test_that("a name from another concept-set family is never an exact match", {
  library <- makeEmbeddingLibrary(
    Asthma_DIAGNOSIS_CODES = c(1, 0, 0),
    Metformin_MEDICATION_CODES = vecWithCosine(0.2)
  )

  result <- matchSets("asthma_medication_codes", library, embeddings = list(asthma = c(1, 0, 0)))

  expect_false("Asthma_DIAGNOSIS_CODES" %in% result$matches$libCsName)
  expect_equal(result$diagnostics$bestCandidate, "Metformin_MEDICATION_CODES")
})

test_that("a close embedding matches when the names differ", {
  library <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.97))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)))

  expect_equal(result$matches$libCsName, copdLibraryName)
  expect_equal(result$diagnostics$matchMethod, "embedding")
  expect_equal(result$diagnostics$matchedEmbeddingSim, 0.97)
})

test_that("a distant embedding does not match and the best candidate is reported", {
  library <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.5))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)))

  expect_equal(nrow(result$matches), 0L)
  expect_equal(result$diagnostics$matchMethod, "none")
  expect_equal(result$diagnostics$bestCandidate, copdLibraryName)
  expect_match(result$diagnostics$note, "below embedding/fuzzy thresholds")
})

test_that("a near-identical name matches by fuzzy similarity alone", {
  library <- makeEmbeddingLibrary(Asthma_DIAGNOSIS_CODES = vecWithCosine(0.1))

  result <- matchSets("asthmaa_diagnosis_codes", library, embeddings = list(asthmaa = c(1, 0, 0)), fuzzyThreshold = 0.8)

  expect_equal(result$matches$libCsName, "Asthma_DIAGNOSIS_CODES")
  expect_equal(result$diagnostics$matchMethod, "fuzzy")
})

test_that("embedding and fuzzy agreement is reported as both", {
  library <- makeEmbeddingLibrary(Asthma_DIAGNOSIS_CODES = vecWithCosine(0.96))

  result <- matchSets("asthmaa_diagnosis_codes", library, embeddings = list(asthmaa = c(1, 0, 0)), fuzzyThreshold = 0.8)

  expect_equal(result$diagnostics$matchMethod, "embedding+fuzzy")
})

test_that("the LLM is asked only about ambiguous scores and its choice can override the ranking", {
  library <- makeEmbeddingLibrary(
    Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.95),
    Asthma_DIAGNOSIS_CODES = vecWithCosine(0.3)
  )
  llm <- makeFakeLlm(structured = adjudicationResponse("Asthma_DIAGNOSIS_CODES"))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE)

  expect_equal(llm$state$kinds, "adjudication")
  expect_equal(result$matches$libCsName, "Asthma_DIAGNOSIS_CODES")
  expect_equal(result$diagnostics$matchMethod, "llmAdjudicated")
  expect_match(llm$state$prompts[[1]], "copd_diagnosis_codes", fixed = TRUE)
})

test_that("an LLM no-match decision with enough confidence rejects an embedding match", {
  library <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.95))
  llm <- makeFakeLlm(structured = adjudicationResponse("NO_MATCH"))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE)

  expect_equal(nrow(result$matches), 0L)
  expect_match(result$diagnostics$note, "LLM adjudication rejected all candidates")
})

test_that("an LLM answer below the confidence threshold is ignored", {
  library <- makeEmbeddingLibrary(
    Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.95),
    Asthma_DIAGNOSIS_CODES = vecWithCosine(0.3)
  )
  llm <- makeFakeLlm(structured = adjudicationResponse("Asthma_DIAGNOSIS_CODES", confidence = "0.3"))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE)

  expect_equal(result$matches$libCsName, copdLibraryName)
  expect_equal(result$diagnostics$matchMethod, "embedding")
  expect_match(result$diagnostics$note, "below confidence threshold")
})

test_that("an LLM choice outside the candidate list falls back to the similarity ranking", {
  library <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.95))
  llm <- makeFakeLlm(structured = adjudicationResponse("Invented_DIAGNOSIS_CODES"))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE)

  expect_equal(result$matches$libCsName, copdLibraryName)
  expect_match(result$diagnostics$note, "invalid candidate or below confidence threshold")
})

test_that("an LLM failure falls back to the similarity ranking", {
  library <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.95))
  llm <- makeFakeLlm(structured = function(kind, prompt, call) stop("model unavailable"))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE)

  expect_equal(result$matches$libCsName, copdLibraryName)
  expect_match(result$diagnostics$note, "LLM adjudication fallback")
})

test_that("clear scores and disabled adjudication never call the LLM", {
  far <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.5))
  gray <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.95))
  llm <- makeFakeLlm(structured = adjudicationResponse("Asthma_DIAGNOSIS_CODES"))

  matchSets(copdTarget, far, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE)
  matchSets(copdTarget, gray, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = FALSE)

  expect_equal(llm$state$structuredCalls, 0L)
})

test_that("two nearly tied candidates are adjudicated even when both score high", {
  library <- makeEmbeddingLibrary(
    Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.99),
    Copd_DIAGNOSIS_CODES_VARIANT = vecWithCosine(0.985)
  )
  llm <- makeFakeLlm(structured = adjudicationResponse("Copd_DIAGNOSIS_CODES_VARIANT"))

  result <- matchSets(copdTarget, library, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE)

  expect_equal(llm$state$structuredCalls, 1L)
  expect_equal(result$matches$libCsName, "Copd_DIAGNOSIS_CODES_VARIANT")
})

test_that("the ambiguity window can be set explicitly", {
  library <- makeEmbeddingLibrary(Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.6))
  llm <- makeFakeLlm(structured = adjudicationResponse(copdLibraryName))

  result <- matchSets(
    copdTarget, library, embeddings = list(copd = c(1, 0, 0)), llm = llm, adjudicate = TRUE,
    llmAmbiguityMinScore = 0.5, llmAmbiguityMaxScore = 0.7
  )

  expect_equal(llm$state$structuredCalls, 1L)
  expect_equal(result$diagnostics$matchMethod, "llmAdjudicated")
})

test_that("each needed name gets one diagnostics row and its best library match", {
  library <- makeEmbeddingLibrary(
    Asthma_DIAGNOSIS_CODES = c(1, 0, 0),
    Chronic_obstructive_pulmonary_disease_DIAGNOSIS_CODES = vecWithCosine(0.97)
  )

  result <- matchSets(c("copd_diagnosis_codes", "Asthma_diagnosis_codes", "copd_diagnosis_codes"), library,
                      embeddings = list(copd = c(1, 0, 0)))

  expect_equal(nrow(result$diagnostics), 2L)
  expect_setequal(result$matches$csName, c("copd_diagnosis_codes", "asthma_diagnosis_codes"))
  expect_equal(result$matches$libCsName[result$matches$csName == "copd_diagnosis_codes"], copdLibraryName)
})

test_that("an empty or incomplete library is reported rather than failing", {
  empty <- data.frame(conceptSetName = character(), embed = I(list()), stringsAsFactors = FALSE)
  expect_match(matchSets(copdTarget, empty)$diagnostics$note, "Library is empty")

  noEmbeddings <- data.frame(conceptSetName = "Other_DIAGNOSIS_CODES", stringsAsFactors = FALSE)
  expect_match(matchSets(copdTarget, noEmbeddings)$diagnostics$note, "Library has no embed column")

  library <- makeEmbeddingLibrary(Asthma_DIAGNOSIS_CODES = c(1, 0, 0))
  expect_match(matchSets(copdTarget, library)$diagnostics$note, "Target embedding unavailable")
  expect_equal(nrow(Ithaca:::ithMatchConceptSets(character(), library, verbose = FALSE)), 0L)
})

test_that("without diagnostics only the matches are returned", {
  library <- makeEmbeddingLibrary(Asthma_DIAGNOSIS_CODES = c(1, 0, 0))

  matches <- Ithaca:::ithMatchConceptSets("asthma_diagnosis_codes", library, verbose = FALSE)

  expect_equal(names(matches), c("csName", "libCsName", "coSim"))
})

test_that("Hecate synonyms are only searched for names the library could not resolve", {
  library <- makeEmbeddingLibrary(
    Asthma_DIAGNOSIS_CODES = c(0, 1, 0),
    Chronic_obstructive_lung_disease_DIAGNOSIS_CODES = vecWithCosine(0.1)
  )
  searched <- character()
  testthat::local_mocked_bindings(
    ithGetHecateCandidateDetails = function(targetName, limit = 5, llmClient = NULL) {
      searched <<- c(searched, targetName)
      list(
        searchTerm = "copd", candidates = "Chronic obstructive lung disease",
        rawCandidates = "Chronic obstructive lung disease", rejectedCandidates = character(), usedFallback = FALSE
      )
    },
    .package = "Ithaca"
  )
  llm <- makeFakeLlm()

  result <- matchSets(
    c("asthma_diagnosis_codes", copdTarget), library, embeddings = list(copd = c(1, 0, 0)),
    llm = llm, adjudicate = TRUE, includeHecateCandidates = TRUE
  )

  expect_equal(searched, copdTarget)
  expect_equal(nrow(result$matches), 2L)
  expect_equal(result$diagnostics$matchMethod[result$diagnostics$csName == "asthma_diagnosis_codes"], "exact")
  expect_equal(result$diagnostics$matchMethod[result$diagnostics$csName == copdTarget], "fuzzy")
  expect_match(result$diagnostics$hecateTopCandidates[result$diagnostics$csName == copdTarget], "Chronic obstructive lung disease")
})

test_that("Hecate candidates are filtered by name overlap and fall back when none overlap", {
  searchedDomains <- list()
  rows <- data.frame(conceptName = c("Chronic obstructive lung disease", "Fracture of femur"), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    ithVectorSearchStandard = function(term, domains = NULL, conceptClasses = NULL, limit = 10, ...) {
      searchedDomains[[length(searchedDomains) + 1L]] <<- domains
      rows
    },
    .package = "Ithaca"
  )

  details <- Ithaca:::ithGetHecateCandidateDetails("chronic_obstructive_lung_disease_diagnosis_codes")

  expect_equal(details$candidates, "Chronic obstructive lung disease")
  expect_equal(details$rejectedCandidates, "Fracture of femur")
  expect_false(details$usedFallback)
  expect_equal(searchedDomains[[1]], "Condition")

  rows <- data.frame(conceptName = c("Fracture of femur", "Appendicitis"), stringsAsFactors = FALSE)
  fallback <- Ithaca:::ithGetHecateCandidateDetails("chronic_obstructive_lung_disease_diagnosis_codes")
  expect_true(fallback$usedFallback)
  expect_equal(fallback$candidates, c("Fracture of femur", "Appendicitis"))
})

test_that("Hecate searches use LLM synonyms and tolerate search failures", {
  terms <- character()
  testthat::local_mocked_bindings(
    ithVectorSearchStandard = function(term, ...) {
      terms <<- c(terms, term)
      if (term == "copd") stop("search service down")
      data.frame(conceptName = "Chronic obstructive pulmonary disease", stringsAsFactors = FALSE)
    },
    .package = "Ithaca"
  )
  llm <- makeFakeLlm(text = function(prompt) "copd; chronic obstructive pulmonary disease")

  details <- Ithaca:::ithGetHecateCandidateDetails("copd_diagnosis_codes", llmClient = llm)

  expect_true(all(c("copd", "chronic obstructive pulmonary disease") %in% terms))
  expect_equal(details$candidates, "Chronic obstructive pulmonary disease")

  empty <- Ithaca:::ithGetHecateCandidateDetails("copd_diagnosis_codes")
  expect_equal(length(Ithaca:::ithGetHecateCandidateDetails("copd_diagnosis_codes")$candidates), 0L)
  expect_equal(empty$searchTerm, "copd")
})

test_that("a failing synonym request falls back to the plain search term", {
  llm <- makeFakeLlm(text = function(prompt) stop("llm down"))

  expect_equal(Ithaca:::ithGetHecateSearchTerms("copd", llm), "copd")
  expect_equal(Ithaca:::ithGetHecateSearchTerms("copd", NULL), "copd")
})
