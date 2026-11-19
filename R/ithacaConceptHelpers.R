ithVectorSearch <- function(term,
                              domains,
                              conceptClasses = NULL,
                              vocabularyId,
                              standardConcept,
                              limit = 10,
                              maxRetries = 3,
                              waitTime = 2) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    vocabulary_id = vocabularyId,
    standard_concept = standardConcept,
    limit = limit
  )

  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }

  url <- "https://hecate.pantheon-hds.com/api/search"
  for (attempt in seq_len(maxRetries)) {
    response <- tryCatch(httr::GET(url, query = params), error = function(e) NULL)
    if (!is.null(response) && httr::status_code(response) == 200) {
      data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      if (length(data) == 0) {
        return(data.frame())
      }
      scoreData <- data[, c("conceptName", "score")]
      rows <- dplyr::bind_rows(data$concepts)
      merged <- merge(rows, scoreData)
      return(SqlRender::snakeCaseToCamelCaseNames(merged))
    }
    if (attempt < maxRetries) {
      Sys.sleep(waitTime)
    }
  }

  stop("All attempts failed for term: ", term)
}

ithVectorSearchAtc <- function(term,
                                  domains,
                                  vocabularyId,
                                  limit = 10,
                                  maxRetries = 3,
                                  waitTime = 2) {
  ithVectorSearch(
    term = term,
    domains = domains,
    conceptClasses = NULL,
    vocabularyId = vocabularyId,
    standardConcept = "C",
    limit = limit,
    maxRetries = maxRetries,
    waitTime = waitTime
  )
}

ithCreateJsonForConceptsPlusDescendants <- function(conceptIds, database) {
  sql <- "
SELECT concept_id,
  concept_name,
       domain_id,
  vocabulary_id,
       concept_class_id,
       standard_concept
FROM @vocab_schema.concept
WHERE concept_id IN (@conceptIds)
  AND (invalidReason IS NULL OR invalidReason = '')
"

  sql <- SqlRender::render(
    sql,
    vocab_schema = database$cdmDatabaseSchema,
    conceptIds = paste(conceptIds, collapse = ",")
  )

  conn <- DatabaseConnector::connect(database$connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  meta <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)

  if (nrow(meta) == 0) {
    return(list(expression = list(items = list())))
  }

  items <- lapply(seq_len(nrow(meta)), function(i) {
    list(
      concept = list(
        CONCEPT_ID = meta$conceptId[[i]],
        CONCEPT_NAME = meta$conceptName[[i]],
        DOMAIN_ID = meta$domainId[[i]],
        VOCABULARY_ID = meta$vocabularyId[[i]],
        CONCEPT_CLASS_ID = meta$conceptClassId[[i]],
        STANDARD_CONCEPT = meta$standardConcept[[i]],
        INVALID_REASON = ""
      ),
      isExcluded = FALSE,
      includeDescendants = TRUE,
      includeMapped = FALSE
    )
  })

  list(expression = list(items = items))
}

ithGetConceptSet <- function(conceptName,
                                conceptIds,
                                domains,
                                conceptClasses = "",
                                baseDirectory,
                                database,
                                quickRun = FALSE,
                                fullString,
                                exclusions = "TEST",
                                additionalInformation = "",
                                clinicalContext = "",
                                bucketSize = 20,
                                llmClient = ithGetLlmClientFromConfig(list(), "nonReasoning")) {
  sql <- paste0(
    "SELECT COUNT(DISTINCT c.concept_id) AS concept_count ",
    "FROM ", database$cdmDatabaseSchema, ".concept_ancestor ca ",
    "JOIN ", database$cdmDatabaseSchema, ".concept c ON ca.descendant_concept_id = c.concept_id ",
    "WHERE ca.ancestor_concept_id IN (", paste(conceptIds, collapse = ","), ") ",
    "AND c.domain_id IN (", paste0("'", domains, "'", collapse = ","), ") ",
    "AND c.concept_class_id IN (", paste0("'", conceptClasses, "'", collapse = ","), ") ",
    "AND c.standard_concept = 'S'"
  )

  connection <- DatabaseConnector::connect(database$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)
  recordCount <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)$conceptCount[[1]]

  if (recordCount <= 2000) {
    excludedConditions <- exclusions
    if (identical(exclusions, "TEST")) {
      exclusionResult <- ithCheckForExclusions(conceptName, llmClient = ithGetLlmClientFromConfig(list(), "reasoning"))
      excludedConditions <- if (exclusionResult$yesNo[[1]] == "YES") exclusionResult$exclusions[[1]] else "none"
    }

    phenelopeAdditionalInformation <- if (is.null(additionalInformation)) "" else as.character(additionalInformation[[1]])
    if (!is.null(excludedConditions) && !identical(excludedConditions, "none")) {
      phenelopeAdditionalInformation <- paste(
        phenelopeAdditionalInformation,
        paste0("Exclusions: ", excludedConditions),
        sep = if (nzchar(phenelopeAdditionalInformation)) "\n" else ""
      )
    }

    return(tryCatch({
      conceptSetJson <- NULL
      lastError <- NULL
      for (attempt in 1:2) {
        conceptSetJson <- tryCatch(
          Phenelope::createConceptSet(
            name = conceptName,
            seedConceptIds = conceptIds,
            clinicalDefinition = phenelopeAdditionalInformation,
            llmClient = llmClient,
            connectionDetails = database$connectionDetails,
            vocabDatabaseSchema = database$cdmDatabaseSchema,
            conceptAdjudicator = Phenelope::DefaultConceptAdjudicator$new(batchSize = 100, nForQuickScreen = 1000),
            conceptRecommender = Phenelope::HecateConceptRecommender$new(minCount = 0),
            batchSize = 100,
            cacheFolder = file.path(baseDirectory, fullString)
          ),
          error = function(e) {
            lastError <<- e
            NULL
          }
        )
        if (!is.null(conceptSetJson)) {
          break
        }
        if (attempt == 1) {
          ithMaybeMessage(TRUE, "Phenelope failed for '", conceptName, "'; retrying once.")
        }
      }
      if (is.null(conceptSetJson)) {
        stop(lastError)
      }
      jsonlite::fromJSON(conceptSetJson, simplifyVector = FALSE)
    }, error = function(e) {
      ithMaybeMessage(
        TRUE,
        "Phenelope failed while creating concept set '",
        conceptName,
        "'. Using placeholder conceptId 36433327. Error: ",
        conditionMessage(e)
      )
      ithPlaceholderConceptSet()
    }))

    # conceptSet <- RubricConcepts::createConceptSet(
    #   conceptName = conceptName,
    #   originalConceptList = conceptIds,
    #   llmClient = llmClient,
    #   excludedConditions = excludedConditions,
    #   connectionDetails = database$connectionDetails,
    #   cdmDatabaseSchema = database$cdmDatabaseSchema,
    #   outputDirectory = file.path(baseDirectory, fullString),
    #   additionalInformation = additionalInformation,
    #   minCount = 0,
    #   belowMinimumCountApproach = "TEST ALL",
    #   quickRun = quickRun,
    #   condenseConceptSet = FALSE,
    #   bucketSize = bucketSize,
    #   clinicalContext = clinicalContext,
    #   tries = 1,
    #   successes = 1
    # )
    #
    # finalConceptSet <- conceptSet[[2]]
    # finalConceptSet <- finalConceptSet[finalConceptSet$items$concept$DOMAIN_ID %in% domains]
    # return(finalConceptSet)
  }

  ithCreateJsonForConceptsPlusDescendants(conceptIds, database)$expression
}

#' Vector search OMOP concepts
#'
#' @param term Search text.
#' @param domains Domain filters.
#' @param conceptClasses Optional concept class filters.
#' @param vocabularyId Vocabulary identifier.
#' @param standardConcept Standard concept flag.
#' @param limit Maximum rows returned.
#' @param maxRetries Retry attempts.
#' @param waitTime Delay between retries in seconds.
#'
#' @return Data frame of matching concepts.
#' @examples
#' \dontrun{
#' results <- vectorSearch(
#'   term = "heart failure",
#'   domains = c("Condition"),
#'   conceptClasses = c("Clinical Finding"),
#'   vocabularyId = "SNOMED",
#'   standardConcept = "S"
#' )
#' }
#' @export
vectorSearch <- function(term,
                         domains,
                         conceptClasses,
                         vocabularyId,
                         standardConcept,
                         limit = 10,
                         maxRetries = 3,
                         waitTime = 2) {
  ithVectorSearch(
    term = term,
    domains = domains,
    conceptClasses = conceptClasses,
    vocabularyId = vocabularyId,
    standardConcept = standardConcept,
    limit = limit,
    maxRetries = maxRetries,
    waitTime = waitTime
  )
}

#' Vector search ATC concepts
#'
#' @param term Search text.
#' @param domains Domain filters.
#' @param vocabularyId Vocabulary identifier.
#' @param limit Maximum rows returned.
#' @param maxRetries Retry attempts.
#' @param waitTime Delay between retries in seconds.
#'
#' @return Data frame of matching concepts.
#' @examples
#' \dontrun{
#' results <- vectorSearchAtc(term = "metformin", domains = c("Drug"), vocabularyId = "ATC")
#' }
#' @export
vectorSearchAtc <- function(term,
                            domains,
                            vocabularyId,
                            limit = 10,
                            maxRetries = 3,
                            waitTime = 2) {
  ithVectorSearchAtc(
    term = term,
    domains = domains,
    vocabularyId = vocabularyId,
    limit = limit,
    maxRetries = maxRetries,
    waitTime = waitTime
  )
}

#' Build a concept set expression from concept IDs and descendants
#'
#' @param conceptIds Concept IDs.
#' @param database Database specification.
#'
#' @return List containing concept set expression items.
#' @examples
#' \dontrun{
#' expr <- createJsonforConceptsPlusDescendants(c(201826, 316139), database = CCAE_OHDA_DBSpec)
#' }
#' @export
createJsonforConceptsPlusDescendants <- function(conceptIds, database) {
  ithCreateJsonForConceptsPlusDescendants(conceptIds, database)
}

#' Create a concept set from seed concepts
#'
#' @param conceptName Concept set target name.
#' @param conceptIds Seed concept IDs.
#' @param domains Domain filters.
#' @param conceptClasses Concept class filters.
#' @param baseDirectory Output directory.
#' @param database Database specification.
#' @param quickRun Logical; enable quick run mode.
#' @param fullString Folder suffix used for output.
#' @param exclusions Exclusion mode or text.
#' @param additionalInformation Additional clinical context text.
#' @param clinicalContext Clinical context text.
#' @param bucketSize Bucket size for concept generation workflow.
#'
#' @return Concept set expression/list.
#' @examples
#' \dontrun{
#' cs <- getConceptSet(
#'   conceptName = "heart failure",
#'   conceptIds = c(316139),
#'   domains = c("Condition"),
#'   baseDirectory = "P:/llm/rubrics",
#'   database = CCAE_OHDA_DBSpec,
#'   fullString = "heart_failure"
#' )
#' }
#' @export
getConceptSet <- function(conceptName,
                          conceptIds,
                          domains,
                          conceptClasses = "",
                          baseDirectory,
                          database,
                          quickRun = FALSE,
                          fullString,
                          exclusions = "TEST",
                          additionalInformation = "",
                          clinicalContext = "",
                          bucketSize = 20) {
  ithGetConceptSet(
    conceptName = conceptName,
    conceptIds = conceptIds,
    domains = domains,
    conceptClasses = conceptClasses,
    baseDirectory = baseDirectory,
    database = database,
    quickRun = quickRun,
    fullString = fullString,
    exclusions = exclusions,
    additionalInformation = additionalInformation,
    clinicalContext = clinicalContext,
    bucketSize = bucketSize
  )
}
