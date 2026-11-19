rubricSpec <- function(name = "COPD",
                       dx = "Two outpatient codes within -60 to +60 of index.",
                       extraPoints = 0L,
                       information = "") {
  list(testCondition = name, dxCriteria = dx, dxAdditionalPoints = extraPoints,
       additionalInformation = information, rubricName = name)
}

# The responder is called with the rubric request number (1, 2, ...) and returns the LLM's rubric.
rubricLlm <- function(responder = function(n) makeFakeRubricResponse(paste0("R", n)),
                      text = function(prompt) "select 1 as fake_sql",
                      other = NULL) {
  requests <- 0L
  makeFakeLlm(
    structured = function(kind, prompt, call) {
      if (kind == "rubric") {
        requests <<- requests + 1L
        return(responder(requests))
      }
      if (!is.null(other)) {
        return(other(kind, prompt, call))
      }
      stop("unexpected structured call: ", kind)
    },
    text = text
  )
}

createRubrics <- function(outputDirectory, llm, specs = list(rubricSpec()), rubricTemplates = "Template A",
                          quiet = TRUE, ..., env = parent.frame()) {
  testthat::local_mocked_bindings(ithGetLlmClientFromConfig = function(...) llm, .package = "Ithaca", .env = env)
  withr::local_options(ithaca.yesCombinationReport = FALSE, .local_envir = env)
  run <- function() Ithaca:::ithCreateRubrics(
    conditionSpecs = specs, outputDirectory = outputDirectory, rubricTemplates = rubricTemplates,
    llmClient = llm, verbose = FALSE, ...
  )
  if (quiet) suppressMessages(run()) else run()
}

conditionPath <- function(outputDirectory, file, conditionName = "COPD") {
  file.path(outputDirectory, conditionName, file)
}

readConciseRubric <- function(outputDirectory, version = 1L, conditionName = "COPD") {
  utils::read.csv(
    conditionPath(outputDirectory, paste0(conditionName, "_conciseRubric_V", version, ".csv"), conditionName),
    stringsAsFactors = FALSE
  )
}
