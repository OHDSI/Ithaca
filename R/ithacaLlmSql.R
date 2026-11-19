ithGetCredential <- function(environmentVariable, required = TRUE) {
  value <- trimws(Sys.getenv(environmentVariable, unset = ""))
  if (nzchar(value)) {
    return(value)
  }

  if (required) {
    stop("Missing credential. Set environment variable ", environmentVariable, ".")
  }

  ""
}

ithDefaultLlmSettings <- function(role) {
  if (identical(role, "reasoning")) {
    list(
      model = "o3",
      endpointEnvVar = "GENAI_O3_ENDPOINT",
      apiKeyEnvVar = "GENAI_API_GPT4_KEY",
      apiVersion = "2024-12-01-preview"
    )
  } else {
    list(
      model = "gpt-4o",
      endpointEnvVar = "GENAI_GPT4O_ENDPOINT",
      apiKeyEnvVar = "GENAI_API_GPT4_KEY",
      apiVersion = "2023-03-15-preview",
      apiArgs = list(temperature = 0, top_p = 1, seed = 1234)
    )
  }
}

# Builds an Azure OpenAI client from the config's llmReasoning / llmNonReasoning block.
# Unset fields fall back to the built-in defaults for that role.
ithGetLlmClientFromConfig <- function(config, role = c("reasoning", "nonReasoning")) {
  role <- match.arg(role)
  settings <- modifyList(
    ithDefaultLlmSettings(role),
    ithCoalesce(if (role == "reasoning") config$llmReasoning else config$llmNonReasoning, list())
  )

  chatFn <- ellmer::chat_azure_openai
  chatFormals <- names(formals(chatFn))
  endpointEnvVar <- as.character(settings$endpointEnvVar[[1]])
  apiKeyEnvVar <- as.character(settings$apiKeyEnvVar[[1]])

  args <- list(
    endpoint = gsub(
      "/openai/deployments.*",
      "",
      ithGetCredential(endpointEnvVar)
    ),
    model = as.character(settings$model[[1]]),
    credentials = function() ithGetCredential(apiKeyEnvVar)
  )

  apiVersion <- as.character(settings$apiVersion[[1]])
  if ("apiVersion" %in% chatFormals) {
    args$apiVersion <- apiVersion
  } else if ("api_version" %in% chatFormals) {
    args$api_version <- apiVersion
  }
  if (length(settings$apiArgs) > 0) {
    args$api_args <- settings$apiArgs
  }

  do.call(chatFn, args)
}

ithResetLlmTurns <- function(llmClient) {
  resetMethod <- tryCatch(llmClient$set_turns, error = function(e) NULL)
  if (is.function(resetMethod)) {
    tryCatch(resetMethod(list()), error = function(e) NULL)
  }
  invisible(NULL)
}

ithQueryLlm <- function(llmClient,
                          prompt,
                          output = c("dataFrame", "list", "text"),
                          ellmerTypeObject = NULL,
                          retries = 5,
                          silent = TRUE) {
  output <- match.arg(output)
  attempt <- 0L
  lastErrorMessage <- NULL

  while (attempt < retries) {
    attempt <- attempt + 1L
    ithResetLlmTurns(llmClient)
    result <- tryCatch({
      if (output == "text") {
        return(llmClient$chat(prompt, echo = "none"))
      }

      if (is.null(ellmerTypeObject)) {
        stop("ellmerTypeObject is required for structured responses")
      }

      text <- llmClient$chat_structured(
        prompt,
        echo = "none",
        type = ellmerTypeObject
      )
      parsed <- jsonlite::fromJSON(jsonlite::toJSON(text), simplifyVector = TRUE)

      if (output == "dataFrame") {
        return(data.frame(parsed, stringsAsFactors = FALSE))
      }

      parsed
    }, error = function(e) {
      lastErrorMessage <<- conditionMessage(e)
      if (!silent) {
        message("LLM attempt ", attempt, " failed: ", lastErrorMessage)
      }
      NULL
    })

    if (!is.null(result)) {
      return(result)
    }
  }

  detail <- if (is.null(lastErrorMessage)) {
    "No underlying error message was captured."
  } else {
    paste0("Last error: ", lastErrorMessage)
  }

  stop(
    "LLM query failed after ", retries, " attempts. ", detail,
    " Check LLM configuration/credentials (for example GENAI_* environment variables) and network access."
  )
}

ithCreateSqlStatement <- function(criteria, conceptSets, llmClient, promptDir = NULL) {
  syntaxRules <- ithReadPrompt("sqlSyntax.txt", promptDir = promptDir)
  template <- ithReadPrompt("createSqlFromRule.txt", promptDir = promptDir)

  prompt <- gsub("<CRITERION>", criteria, template, fixed = TRUE)
  prompt <- gsub("<CONCEPT_SET_NAMES>", conceptSets, prompt, fixed = TRUE)
  prompt <- gsub("<OHDSI_SYNTAX>", syntaxRules, prompt, fixed = TRUE)

  sql <- ithQueryLlm(llmClient, prompt, output = "text")

  ithEnsureSpecialtyProviderFilter(
    criteria = criteria,
    conceptSets = conceptSets,
    sql = sql,
    llmClient = llmClient,
    promptDir = promptDir
  )
}

ithCorrectSqlStatement <- function(sql, llmClient, error = NULL, promptDir = NULL) {
  syntaxRules <- ithReadPrompt("sqlSyntax.txt", promptDir = promptDir)
  templateFile <- if (is.null(error)) "correctSqlSyntax.txt" else "correctSqlError.txt"
  template <- ithReadPrompt(templateFile, promptDir = promptDir)

  prompt <- gsub("<SQL_QUERY>", sql, template, fixed = TRUE)
  if (!is.null(error)) {
    prompt <- gsub("<QUERY_ERROR>", error, prompt, fixed = TRUE)
  }
  prompt <- gsub("<OHDSI_SYNTAX>", syntaxRules, prompt, fixed = TRUE)

  ellmerTypeObject <- ellmer::type_object(
    originalQuery = ellmer::type_string(),
    changesMade = ellmer::type_string(),
    correctedQuery = ellmer::type_string(),
    unableToCorrect = ellmer::type_enum(values = c("YES", "NO"))
  )

  ithQueryLlm(
    llmClient = llmClient,
    prompt = prompt,
    output = "list",
    ellmerTypeObject = ellmerTypeObject
  )
}

ithCriteriaRequiresSpecialtyProviderFilter <- function(criteria) {
  criteriaText <- tolower(paste(criteria, collapse = " "))
  grepl(
    "\\b(cardiolog|dermatolog|rheumatolog|neurolog|oncolog|hospice|palliative|specialt|specialist)\\b",
    criteriaText,
    perl = TRUE
  )
}

ithExtractConceptSetTokens <- function(sql, columnName) {
  pattern <- paste0("(?is)", columnName, "\\s+in\\s*\\(([^)]*)\\)")
  matches <- regmatches(sql, gregexpr(pattern, sql, perl = TRUE))[[1]]
  if (length(matches) == 0 || identical(matches, character(0))) {
    return(character(0))
  }

  tokens <- character(0)
  for (matchText in matches) {
    tokenMatch <- regmatches(matchText, gregexpr("@[A-Za-z0-9_]+", matchText, perl = TRUE))[[1]]
    if (length(tokenMatch) > 0 && !identical(tokenMatch, character(0))) {
      tokens <- c(tokens, tokenMatch)
    }
  }

  unique(tokens)
}

ithSqlHasProviderSpecialtyFilter <- function(sql) {
  hasProviderJoin <- grepl(
    "@databaseSchema\\.provider|\\bprovider\\b",
    sql,
    ignore.case = TRUE,
    perl = TRUE
  )
  hasSpecialtyFilter <- grepl(
    "specialty_concept_id",
    sql,
    ignore.case = TRUE,
    perl = TRUE
  )

  visitTokens <- ithExtractConceptSetTokens(sql, "visit_concept_id")
  specialtyTokens <- ithExtractConceptSetTokens(sql, "specialty_concept_id")
  hasDistinctTokenUsage <- TRUE
  if (length(visitTokens) > 0 && length(specialtyTokens) > 0) {
    hasDistinctTokenUsage <- !setequal(visitTokens, specialtyTokens)
  }

  list(
    hasProviderJoin = hasProviderJoin,
    hasSpecialtyFilter = hasSpecialtyFilter,
    hasDistinctTokenUsage = hasDistinctTokenUsage,
    isComplete = hasProviderJoin && hasSpecialtyFilter && hasDistinctTokenUsage
  )
}

ithEnsureSpecialtyProviderFilter <- function(criteria,
                                             conceptSets,
                                             sql,
                                             llmClient,
                                             promptDir = NULL,
                                             maxCorrectionAttempts = 2L) {
  if (!ithCriteriaRequiresSpecialtyProviderFilter(criteria)) {
    return(sql)
  }

  currentSql <- sql
  coverage <- ithSqlHasProviderSpecialtyFilter(currentSql)
  attempts <- 0L

  while (!coverage$isComplete && attempts < maxCorrectionAttempts) {
    attempts <- attempts + 1L
    missingParts <- c(
      if (!coverage$hasProviderJoin) "provider join",
      if (!coverage$hasSpecialtyFilter) "p.specialty_concept_id filter",
      if (!coverage$hasDistinctTokenUsage) "distinct concept-set usage between visit_concept_id and specialty_concept_id"
    )

    errorText <- paste0(
      "Criterion requires specialty provider filtering, but SQL is missing: ",
      paste(missingParts, collapse = " and "),
      ". Add a provider join and specialty filter. Criteria: ", criteria,
      ". Concept sets: ", conceptSets,
      "."
    )

    corrected <- ithCorrectSqlStatement(
      sql = currentSql,
      llmClient = llmClient,
      error = errorText,
      promptDir = promptDir
    )

    correctedSql <- corrected$correctedQuery
    if (is.null(correctedSql) || !nzchar(trimws(correctedSql))) {
      break
    }

    currentSql <- correctedSql
    coverage <- ithSqlHasProviderSpecialtyFilter(currentSql)
  }

  if (!coverage$isComplete) {
    warning(
      "Specialty criterion SQL is missing provider specialty constraints after correction attempts. ",
      "Criteria: ", criteria,
      call. = FALSE
    )
  }

  currentSql
}

#' Query an LLM client with optional structured output
#'
#' @param llmClient ellmer chat client.
#' @param prompt Prompt text.
#' @param systemPrompt Optional system prompt text.
#' @param silent Logical; suppress retry messages.
#' @param output Output mode (`"data frame"`, `"list"`, or `"text"`).
#' @param array Deprecated compatibility argument.
#' @param ellmerTypeObject Optional ellmer type object for structured output.
#'
#' @return Parsed model response.
#' @examples
#' \dontrun{
#' llm <- getLlmo3Client()
#' txt <- queryLLM(llmClient = llm, prompt = "Return OK", output = "text")
#' }
#' @export
queryLLM <- function(llmClient,
                     prompt,
                     systemPrompt = NULL,
                     silent = TRUE,
                     output = "data frame",
                     array = FALSE,
                     ellmerTypeObject = NULL) {
  if (!is.null(systemPrompt) && nzchar(systemPrompt)) {
    prompt <- paste(systemPrompt, prompt, sep = "\n\n")
  }

  mappedOutput <- switch(
    output,
    "text" = "text",
    "data frame" = "dataFrame",
    "list"
  )

  ithQueryLlm(
    llmClient = llmClient,
    prompt = prompt,
    output = mappedOutput,
    ellmerTypeObject = ellmerTypeObject,
    silent = silent
  )
}

#' Create OHDSI SQL from rubric criteria
#'
#' @param criteria Rule criteria text.
#' @param conceptSets Concept set names for plugin replacement.
#' @param llmClient ellmer chat client.
#'
#' @return SQL text.
#' @examples
#' \dontrun{
#' sql <- createSqlStatement(
#'   criteria = "2 outpatient diagnoses within 60 days",
#'   conceptSets = "gerd_diagnosis",
#'   llmClient = getLlmo3Client()
#' )
#' }
#' @export
createSqlStatement <- function(criteria, conceptSets, llmClient) {
  ithCreateSqlStatement(
    criteria = criteria,
    conceptSets = conceptSets,
    llmClient = llmClient
  )
}

#' Correct malformed SQL using LLM prompts
#'
#' @param sql SQL text to correct.
#' @param error Optional SQL error message.
#'
#' @return List containing original query, changes, and corrected query.
#' @examples
#' \dontrun{
#' fixed <- correctSqlStatement(
#'   sql = "select * from cdm.condition_occurrence where person_id == 1",
#'   error = "Parse error near =="
#' )
#' }
#' @export
correctSqlStatement <- function(sql, error = NULL) {
  ithCorrectSqlStatement(
    sql = sql,
    llmClient = ithGetLlmClientFromConfig(list(), "reasoning"),
    error = error
  )
}

#' Get configured `o3` LLM client
#'
#' @return ellmer chat client.
#' @examples
#' \dontrun{
#' llm <- getLlmo3Client()
#' }
#' @export
getLlmo3Client <- function() {
  ithGetLlmClientFromConfig(list(), "reasoning")
}

#' Get configured `gpt-4o` LLM client
#'
#' @return ellmer chat client.
#' @examples
#' \dontrun{
#' llm <- getLlm4oClient()
#' }
#' @export
getLlm4oClient <- function() {
  ithGetLlmClientFromConfig(list(), "nonReasoning")
}
