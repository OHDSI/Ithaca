# Probe: for each rule of a condition's concise rubric, ask an LLM for one query on observational
# data in the OMOP common data model that would support or refute the rule's criteria and its score.
#
# One LLM call per rule, with the whole rubric as context. Each query builds two populations from the
# CDM itself (no cohort table needed) and tests the rule on them:
#   positives: people with the condition, index = the first date rule ER2 (definitive evidence) is met;
#   negatives: people without it, index = their first code from <condition>_RELEVANT_DIAGNOSIS_CODES
#     (the differential diagnoses, as used by AE1), and never a code from <condition>_DIAGNOSIS_CODES.
# The rule's hit rate in each population supports or refutes its criteria and its score. Each query is
# OHDSI SQL returning aggregate counts only, with why it was developed and how its results will be
# read. Nothing is written to the rubric files: results are returned and, with saveResults, saved as
# unversioned files under <outputDirectory>/rubricEvidenceProbe/.
#
# ER1, ER2 and the fixed AE1 are skipped unless includeFixed = TRUE (their points are intentional).
#
# Example:
#   pkgload::load_all("E:/Ithaca")
#   source("extras/rubricEvidenceProbe.R")
#   probe <- probeRubricEvidence("Major Depressive Disorder", maxRules = 3)
#   probe$queries    # one row per rule: the SQL and its explanation
#   probe$rules      # one row per rule: status and timing
#   probeRubricEvidence("Seizure", dryRun = TRUE)$prompts[[1]]   # prompts only, no LLM call

probeEvidenceRulePrompt <- function(conditionName, rubricRows, ruleRow, dxCriteria, additionalInformation) {
  orNone <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x[[1]]) || !nzchar(trimws(x[[1]]))) "<none supplied>" else as.character(x[[1]])
  }
  rubricJson <- as.character(jsonlite::toJSON(rubricRows, auto_unbox = TRUE, pretty = TRUE, na = "null"))
  ruleJson <- as.character(jsonlite::toJSON(ruleRow, auto_unbox = TRUE, pretty = TRUE, na = "null"))

  paste0(
    "You are a senior clinician and clinical informaticist. A computable phenotype rubric for '", conditionName, "' ",
    "will be scored against OMOP-mapped claims/EHR data. Each rule adds its score when satisfied (evidenceRules), ",
    "usually subtracts points (alternativeExplanations), or forces a negative decision (hard_exclusion, -100). ",
    "A patient is positive when total points reach rubricYesScore and negative below rubricNoScore.\n\n",
    "Diagnostic criteria supplied by the user (authoritative):\n", orNone(dxCriteria), "\n\n",
    "Additional information supplied at rubric creation (authoritative):\n", orNone(additionalInformation), "\n\n",
    "Whole rubric, for context:\n", rubricJson, "\n\n",
    "TARGET RULE:\n", ruleJson, "\n\n"
  )
}

probeDataQueryType <- function() {
  ellmer::type_object(
    sql = ellmer::type_string(),
    whyDeveloped = ellmer::type_string(),
    howUsedForCriteria = ellmer::type_string(),
    howUsedForScore = ellmer::type_string(),
    resultColumns = ellmer::type_string(),
    limitations = ellmer::type_string()
  )
}

# ER2's definition and the two concept sets that define the populations, from the rubric and the
# fixed AE1 naming; stops when the rubric has no ER2.
probePopulations <- function(conditionName, rubricRows) {
  er2 <- rubricRows[toupper(trimws(as.character(rubricRows$ruleId))) == "ER2", , drop = FALSE]
  if (nrow(er2) == 0L) stop("The rubric has no ER2 rule to define the positive population", call. = FALSE)
  list(
    er2 = er2[1, intersect(c("ruleId", "type", "score", "criteria", "neededConceptSets"), names(er2)), drop = FALSE],
    conditionSet = Ithaca:::ithAe1ConditionConceptSetName(conditionName),
    relevantSet = Ithaca:::ithAe1ConceptSetName(conditionName)
  )
}

probeDataQueryPrompt <- function(conditionName, rubricRows, ruleRow, dxCriteria, additionalInformation, populations) {
  context <- probeEvidenceRulePrompt(conditionName, rubricRows, ruleRow, dxCriteria, additionalInformation)
  er2Json <- as.character(jsonlite::toJSON(populations$er2, auto_unbox = TRUE, pretty = TRUE, na = "null"))
  paste0(
    context,
    "Task: write ONE query on observational data in the OMOP common data model that tests this rule, on two populations ",
    "built from the CDM itself, so that its results support or refute the rule's criteria and its score.\n\n",
    "The two populations (the same for every rule):\n",
    "- POSITIVE: people with the condition. Their index date is the FIRST date on which rule ER2 (the definitive ",
    "evidence rule, below) is met. Implement ER2's own criteria (setting, counts, windows) as written and take each ",
    "person's earliest qualifying date.\n",
    "- NEGATIVE: people without the condition who have a differential diagnosis. Their index date is the FIRST date of any ",
    "code in @", populations$relevantSet, " (the differential diagnoses). Exclude anyone who has any code in @",
    populations$conditionSet, " at any time.\n",
    "ER2 (positive definition):\n", er2Json, "\n\n",
    "Test the TARGET RULE on both populations: for every person, check whether the rule's criteria are met using the ",
    "windows relative to THAT person's index date, exactly as the rule is written. Do not use the target rule's own ",
    "hits to define either population.\n\n",
    "Query requirements:\n",
    "- OHDSI SQL (SQL Server syntax that SqlRender translates). The CDM is in @databaseSchema (person, ",
    "observation_period, visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence, measurement, ",
    "observation, death). There is no cohort table: build both populations in CTEs.\n",
    "- Concept sets are placeholders that resolve to concept ID lists. Use them exactly as written, inside IN (...), ",
    "for example condition_concept_id IN (@", populations$conditionSet, "). Use the placeholders in the rule's ",
    "neededConceptSets, in ER2's neededConceptSets, and @", populations$conditionSet, " and @", populations$relevantSet, ". ",
    "Do not invent other placeholders; use another concept_id only if you are certain of it.\n",
    "- Return aggregate counts or proportions only, never patient-level rows.\n",
    "- The SQL must be exactly one statement that begins with WITH or SELECT. No SET, USE, DECLARE, CREATE or temp ",
    "tables, no prose or notes outside SQL comments, and nothing before the first WITH or SELECT except comments.\n",
    "- Return one row per population (POSITIVE, NEGATIVE) with at least: population, persons, persons_meeting_rule, ",
    "proportion_meeting_rule. Also include the likelihood ratio (proportion in POSITIVE divided by proportion in NEGATIVE, ",
    "guarded against division by zero) in a column repeated on both rows.\n",
    "- If the target rule is an alternative explanation or hard exclusion, the proportion meeting it in POSITIVE is the ",
    "share of definite cases it would wrongly push down or exclude.\n\n",
    "Always explain, in plain language:\n",
    "- whyDeveloped: why this query was developed, how you implemented ER2 and the first-occurrence index dates, and ",
    "what it tests about this rule.\n",
    "- howUsedForCriteria: how the result columns will be read to support or refute the criteria wording, with the ",
    "concrete result pattern or threshold that would count as support and the pattern that would count as refutation.\n",
    "- howUsedForScore: how the results will be read to support or refute the score: how the likelihood ratio and the ",
    "two proportions relate to the rule's points and to the yes threshold of 10 (a rule that can reach YES alone should be ",
    "almost absent in NEGATIVE; a rule with a likelihood ratio near 1 supports no positive points; a negative-point rule ",
    "should be far more common in NEGATIVE than in POSITIVE), and the pattern that would suggest a higher or lower score.\n",
    "- resultColumns: each column the query returns and what it means.\n",
    "- limitations: what the query cannot show (ER2 defining the positives, differential-diagnosis negatives being ",
    "harder than typical non-cases, index dates from first diagnosis rather than a clinical event, claims data gaps)."
  )
}

# Returns list(rule, query) for one rule; an LLM failure becomes status "error", not a stop. SQL that
# the runner would reject is sent back once with the reason.
probeQueryForRule <- function(prompt, ruleRow, llmClient, verbose) {
  startedAt <- Sys.time()
  status <- "ok"
  errorMessage <- ""
  ask <- function(text) {
    tryCatch(
      Ithaca:::ithQueryLlm(
        llmClient = llmClient, prompt = text, output = "list",
        ellmerTypeObject = probeDataQueryType(), silent = !isTRUE(verbose)
      ),
      error = function(e) {
        status <<- "error"
        errorMessage <<- conditionMessage(e)
        NULL
      }
    )
  }
  result <- ask(prompt)
  if (!is.null(result)) {
    reason <- probeQueryIsReadOnly(paste(as.character(result$sql), collapse = " "))
    if (nzchar(reason)) {
      Ithaca:::ithMaybeMessage(verbose, "    SQL rejected (", reason, "); asking for a corrected query")
      retry <- ask(paste0(
        prompt, "\n\nYour previous query was rejected before running because ", reason, ". Previous SQL:\n",
        paste(as.character(result$sql), collapse = " "),
        "\n\nReturn the corrected query with the same explanations: exactly one statement that begins with WITH or ",
        "SELECT, with nothing before it except SQL comments."
      ))
      if (!is.null(retry)) result <- retry
    }
  }
  text <- function(x) if (is.null(x) || length(x) == 0L) "" else paste(as.character(x), collapse = " ")

  query <- NULL
  if (!is.null(result)) {
    query <- data.frame(
      rubric = as.character(ruleRow$rubric),
      ruleId = as.character(ruleRow$ruleId),
      type = as.character(ruleRow$type),
      score = as.character(ruleRow$score),
      criteria = as.character(ruleRow$criteria),
      sql = text(result$sql),
      whyDeveloped = text(result$whyDeveloped),
      howUsedForCriteria = text(result$howUsedForCriteria),
      howUsedForScore = text(result$howUsedForScore),
      resultColumns = text(result$resultColumns),
      limitations = text(result$limitations),
      stringsAsFactors = FALSE
    )
    explained <- nzchar(query$whyDeveloped) && nzchar(query$howUsedForCriteria) && nzchar(query$howUsedForScore)
    if (!nzchar(query$sql) || !explained) {
      status <- "incomplete"
      errorMessage <- "the data query is missing its SQL or part of its explanation"
    }
  }

  list(
    rule = data.frame(
      rubric = as.character(ruleRow$rubric),
      ruleId = as.character(ruleRow$ruleId),
      status = status,
      message = substr(gsub("[\r\n]+", " ", errorMessage), 1, 300),
      seconds = round(as.numeric(difftime(Sys.time(), startedAt, units = "secs")), 1),
      stringsAsFactors = FALSE
    ),
    query = query
  )
}

#' Probe an LLM-written data query for each rule of a concise rubric
#'
#' @param conditionName Condition/rubric name (`rubricName` or `testCondition` in the config).
#' @param outputDirectory Root output directory; `NULL` uses the config's.
#' @param configPath Path to config.yaml (used for anything not supplied).
#' @param conciseRubricPath Optional concise rubric CSV; `NULL` uses the latest in the condition folder.
#' @param dxCriteria,additionalInformation Optional overrides for the condition's resolved text.
#' @param ruleIds Optional rule IDs to check; default is every rule.
#' @param maxRules Optional cap on the number of rules (first ones in rubric order).
#' @param includeFixed When `TRUE`, also check ER1, ER2 and the fixed AE1.
#' @param llmClient LLM client; `NULL` uses the config's `llmReasoning` settings.
#' @param dryRun When `TRUE`, build the prompts and make no LLM calls.
#' @param saveResults When `TRUE`, save `rules` and `queries` CSVs, and each query as a commented
#'   `.sql` file, under `<outputDirectory>/rubricEvidenceProbe/`.
#' @param verbose Print progress.
#' @return A list with `rules` (status per rule), `queries` (SQL with `whyDeveloped`,
#'   `howUsedForCriteria`, `howUsedForScore`, `resultColumns`, `limitations`), `prompts` (named by
#'   rubric/rule) and `savedTo`.
probeRubricEvidence <- function(conditionName,
                                outputDirectory = NULL,
                                configPath = "config.yaml",
                                conciseRubricPath = NULL,
                                dxCriteria = NULL,
                                additionalInformation = NULL,
                                ruleIds = NULL,
                                maxRules = NULL,
                                includeFixed = FALSE,
                                llmClient = NULL,
                                dryRun = FALSE,
                                saveResults = FALSE,
                                verbose = TRUE) {
  if (is.null(outputDirectory) || is.null(dxCriteria) || is.null(additionalInformation) ||
      (is.null(llmClient) && !isTRUE(dryRun))) {
    config <- Ithaca:::ithReadIthacaConditionConfig(configPath)
    spec <- Ithaca:::ithFindConfigConditionSpec(config, conditionName)
    if (is.null(outputDirectory)) outputDirectory <- as.character(config$outputDirectory[[1]])
    if (is.null(dxCriteria)) dxCriteria <- as.character(Ithaca:::ithCoalesce(spec$dxCriteria, "")[[1]])
    if (is.null(additionalInformation)) additionalInformation <- as.character(Ithaca:::ithCoalesce(spec$additionalInformation, "")[[1]])
    if (is.null(llmClient) && !isTRUE(dryRun)) llmClient <- Ithaca:::ithGetLlmClientFromConfig(config, "reasoning")
  }

  conditionDir <- file.path(outputDirectory, conditionName)
  if (is.null(conciseRubricPath)) {
    conciseRubric <- Ithaca:::ithLoadLatest(conditionDir, paste0(conditionName, "_conciseRubric.csv"))
    if (is.null(conciseRubric)) stop("No concise rubric found for condition: ", conditionName)
  } else {
    conciseRubric <- utils::read.csv(conciseRubricPath, stringsAsFactors = FALSE)
  }
  Ithaca:::ithValidateEditedConciseRubric(conciseRubric, conditionName = conditionName)

  keepColumns <- intersect(
    c("rubric", "rubricName", "rubricYesScore", "rubricNoScore", "ruleId", "type", "score", "criteria", "rationale", "neededConceptSets"),
    names(conciseRubric)
  )
  rulesOut <- list()
  queriesOut <- list()
  prompts <- list()

  for (rubricId in unique(as.character(conciseRubric$rubric))) {
    rubricRows <- conciseRubric[as.character(conciseRubric$rubric) == rubricId, keepColumns, drop = FALSE]
    ids <- toupper(trimws(as.character(rubricRows$ruleId)))
    isFixed <- ids %in% c("ER1", "ER2") |
      (ids == "AE1" & grepl("_relevant_diagnosis_codes(,|$)", tolower(gsub("\\s+", "", as.character(rubricRows$neededConceptSets)))))
    rubricRows$fixed <- isFixed
    populations <- probePopulations(conditionName, rubricRows)

    selected <- which(rep(TRUE, nrow(rubricRows)))
    if (!isTRUE(includeFixed)) selected <- selected[!isFixed[selected]]
    if (!is.null(ruleIds)) selected <- selected[ids[selected] %in% toupper(ruleIds)]
    if (!is.null(maxRules)) selected <- utils::head(selected, as.integer(maxRules))
    Ithaca:::ithMaybeMessage(verbose, "Rubric ", rubricId, ": ", length(selected), " rule(s) to check for '", conditionName, "'")

    for (row in selected) {
      ruleRow <- rubricRows[row, keepColumns, drop = FALSE]
      prompt <- probeDataQueryPrompt(conditionName, rubricRows, ruleRow, dxCriteria, additionalInformation, populations)
      prompts[[paste0(rubricId, "/", ruleRow$ruleId)]] <- prompt
      if (isTRUE(dryRun)) next

      Ithaca:::ithMaybeMessage(verbose, "  [", format(Sys.time(), "%H:%M:%S"), "] ", rubricId, "/", ruleRow$ruleId, " (score ", ruleRow$score, ")")
      one <- probeQueryForRule(prompt, ruleRow, llmClient, verbose)
      rulesOut[[length(rulesOut) + 1L]] <- one$rule
      queriesOut[[length(queriesOut) + 1L]] <- one$query
      Ithaca:::ithMaybeMessage(
        verbose, "    ", one$rule$status, if (nzchar(one$rule$message)) paste0(" - ", one$rule$message) else "",
        " (", one$rule$seconds, "s)"
      )
    }
  }

  rules <- if (length(rulesOut) > 0L) do.call(rbind, rulesOut) else NULL
  queries <- Filter(Negate(is.null), queriesOut)
  queries <- if (length(queries) > 0L) do.call(rbind, queries) else NULL

  savedTo <- NULL
  if (isTRUE(saveResults) && !is.null(rules)) {
    folder <- file.path(outputDirectory, "rubricEvidenceProbe")
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    savedTo <- c(
      rules = file.path(folder, paste0(conditionName, "_ruleStatus_", stamp, ".csv")),
      queries = file.path(folder, paste0(conditionName, "_dataQueries_", stamp, ".csv"))
    )
    utils::write.csv(rules, savedTo[["rules"]], row.names = FALSE)
    if (!is.null(queries)) {
      utils::write.csv(queries, savedTo[["queries"]], row.names = FALSE)
      sqlFolder <- file.path(folder, paste0(conditionName, "_dataQueries_", stamp))
      dir.create(sqlFolder, showWarnings = FALSE)
      for (row in seq_len(nrow(queries))) {
        comment <- function(label, value) paste0("-- ", label, ": ", gsub("[\r\n]+", " ", value))
        writeLines(
          c(
            comment("Rule", paste0(queries$rubric[[row]], "/", queries$ruleId[[row]], " (score ", queries$score[[row]], "): ", queries$criteria[[row]])),
            comment("Why developed", queries$whyDeveloped[[row]]),
            comment("How used for the criteria", queries$howUsedForCriteria[[row]]),
            comment("How used for the score", queries$howUsedForScore[[row]]),
            comment("Result columns", queries$resultColumns[[row]]),
            comment("Limitations", queries$limitations[[row]]),
            "", queries$sql[[row]]
          ),
          file.path(sqlFolder, paste0(queries$rubric[[row]], "_", queries$ruleId[[row]], ".sql"))
        )
      }
    }
    Ithaca:::ithMaybeMessage(verbose, "Saved: ", paste(savedTo, collapse = " ; "))
  }

  list(rules = rules, queries = queries, prompts = prompts, savedTo = savedTo)
}

probeStripSqlComments <- function(sql) {
  gsub("(?s)--[^\n]*|/\\*.*?\\*/", " ", as.character(sql), perl = TRUE)
}

# The SQL is LLM-written, so only a single read-only SELECT/WITH statement is run. Returns "" when
# acceptable, otherwise the reason it is rejected.
probeQueryIsReadOnly <- function(sql) {
  text <- probeStripSqlComments(sql)
  text <- trimws(sub(";\\s*$", "", trimws(text)))
  if (!grepl("^(select|with)\\b", text, ignore.case = TRUE)) {
    return("it does not start with SELECT or WITH")
  }
  if (grepl(";", text, fixed = TRUE)) {
    return("it has more than one statement")
  }
  banned <- regmatches(text, regexpr("\\b(insert|update|delete|drop|create|alter|truncate|merge|grant|revoke|call|exec|execute|into)\\b", text, ignore.case = TRUE))
  if (length(banned) > 0L) {
    return(paste0("it uses ", toupper(banned[[1]])))
  }
  ""
}

#' Run the queries written by probeRubricEvidence against one database
#'
#' Renders each query for the database (the CDM schema, and the concept-set placeholders resolved from
#' the condition's matched concept sets and the concept set library, including its
#' `<condition>_DIAGNOSIS_CODES` and `<condition>_RELEVANT_DIAGNOSIS_CODES` sets), translates it to the
#' database's dialect, and runs it. The queries build their own populations from the CDM, so no cohort
#' table is used. Only single read-only SELECT/WITH statements are run. A query that fails is recorded
#' and the rest still run.
#'
#' @param conditionName Condition/rubric name the queries were written for.
#' @param queries Data frame from `probeRubricEvidence()$queries`, a path to a saved queries CSV, or
#'   `NULL` for the latest `<conditionName>_dataQueries_*.csv` under
#'   `<outputDirectory>/rubricEvidenceProbe/`.
#' @param databaseId Database to run against; it must have a CDM schema in the config or in `cdm`.
#' @param cdm Named list of database id -> CDM schema name, used when `databaseId` is not in the config's
#'   `general.cdm` (OptumDod is commented out there).
#' @param configPath Path to config.yaml.
#' @param outputDirectory Root output directory; `NULL` uses the config's.
#' @param saveResults When `TRUE`, save a status CSV and one result CSV per query in
#'   `<outputDirectory>/rubricEvidenceProbe/`.
#' @param verbose Print progress.
#' @return A list with `results` (one row per query: status, rows, seconds, message, and the query's
#'   explanation columns), `data` (the result table of each query, named `rubric/ruleId`) and `savedTo`.
probeRunRubricQueries <- function(conditionName,
                                  queries = NULL,
                                  databaseId = "OptumDod",
                                  cdm = list(OptumDod = "cdm_optum_extended_dod_v4155"),
                                  configPath = "config.yaml",
                                  outputDirectory = NULL,
                                  saveResults = TRUE,
                                  verbose = TRUE) {
  config <- suppressMessages(Ithaca:::ithReadIthacaWorkflowConfig(configPath))
  if (is.null(outputDirectory)) outputDirectory <- as.character(config$outputDirectory[[1]])
  folder <- file.path(outputDirectory, "rubricEvidenceProbe")

  if (is.null(queries)) {
    files <- sort(list.files(folder, pattern = "_dataQueries_[0-9]+\\.csv$", full.names = TRUE))
    files <- files[startsWith(basename(files), paste0(conditionName, "_dataQueries_"))]
    if (length(files) == 0L) stop("No saved queries found for '", conditionName, "' in ", folder)
    queries <- files[[length(files)]]
    Ithaca:::ithMaybeMessage(verbose, "Reading queries from ", queries)
  }
  if (is.character(queries)) queries <- utils::read.csv(queries, stringsAsFactors = FALSE)
  missingColumns <- setdiff(c("rubric", "ruleId", "sql"), names(queries))
  if (length(missingColumns) > 0L) stop("queries is missing column(s): ", paste(missingColumns, collapse = ", "))

  cdmMap <- if (databaseId %in% names(config$cdm)) config$cdm[databaseId] else cdm[databaseId]
  if (is.null(cdmMap[[1]])) stop("No CDM schema for database '", databaseId, "' in the config or in cdm")
  config$cdm <- cdmMap
  database <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = globalenv())[[databaseId]]

  conceptSets <- tryCatch(list(
    library = Ithaca:::ithLoadConceptSetLibraryForScoring(
      outputDirectory = outputDirectory, seedLibraryPath = config$seedLibraryPath, libraryVersion = config$conceptLibraryVersion
    )[[1]],
    matched = Ithaca:::ithLoadLatest(file.path(outputDirectory, conditionName), "matchedConceptSets.rds")
  ), error = function(e) list(error = conditionMessage(e)))

  Ithaca:::ithMaybeMessage(
    verbose, "Running ", nrow(queries), " query(ies) on ", databaseId, " (", database$cdmDatabaseSchema, ")"
  )
  connection <- suppressMessages(DatabaseConnector::connect(database$connectionDetails))
  on.exit(suppressWarnings(try(DatabaseConnector::disconnect(connection), silent = TRUE)), add = TRUE)

  runOne <- function(sql) {
    reason <- probeQueryIsReadOnly(sql)
    if (nzchar(reason)) stop("query not run: ", reason, call. = FALSE)
    sql <- probeStripSqlComments(sql)
    plugins <- unique(stringr::str_extract_all(sql, "\\B@[A-Za-z0-9_]+")[[1]])
    plugins <- plugins[plugins != "@databaseSchema"]
    plugins <- plugins[order(-nchar(plugins))]
    if (length(plugins) > 0L && !is.null(conceptSets$error)) {
      stop("concept sets could not be loaded: ", conceptSets$error, call. = FALSE)
    }
    rendered <- SqlRender::render(sql, databaseSchema = database$cdmDatabaseSchema)
    for (plugin in plugins) {
      detail <- Ithaca:::ithResolveConceptSetPluginDetails(
        substr(plugin, 2, nchar(plugin)), matchedConceptSets = conceptSets$matched,
        conceptSetJsonLibrary = conceptSets$library, database = database
      )
      rendered <- gsub(plugin, detail$value, rendered, fixed = TRUE)
    }
    translated <- SqlRender::translate(rendered, database$connectionDetails$dbms)
    DatabaseConnector::querySql(connection, translated, snakeCaseToCamelCase = TRUE, integer64AsNumeric = FALSE)
  }

  explanation <- intersect(
    c("whyDeveloped", "howUsedForCriteria", "howUsedForScore", "resultColumns", "limitations"), names(queries)
  )
  rows <- list()
  data <- list()
  for (index in seq_len(nrow(queries))) {
    label <- paste0(queries$rubric[[index]], "/", queries$ruleId[[index]])
    startedAt <- Sys.time()
    status <- "ok"
    errorMessage <- ""
    result <- tryCatch(runOne(queries$sql[[index]]), error = function(e) {
      status <<- "error"
      errorMessage <<- conditionMessage(e)
      NULL
    })
    seconds <- round(as.numeric(difftime(Sys.time(), startedAt, units = "secs")), 1)
    if (!is.null(result)) data[[label]] <- result
    rows[[index]] <- cbind(
      data.frame(
        rubric = queries$rubric[[index]], ruleId = queries$ruleId[[index]], databaseId = databaseId, status = status,
        rows = if (is.null(result)) NA_integer_ else nrow(result), seconds = seconds,
        message = substr(gsub("[\r\n]+", " ", errorMessage), 1, 300), stringsAsFactors = FALSE
      ),
      queries[index, explanation, drop = FALSE]
    )
    Ithaca:::ithMaybeMessage(
      verbose, "  ", label, ": ", status, if (is.null(result)) "" else paste0(", ", nrow(result), " row(s)"), " (", seconds, "s)",
      if (nzchar(errorMessage)) paste0(" - ", substr(errorMessage, 1, 150)) else ""
    )
  }
  results <- do.call(rbind, rows)

  savedTo <- NULL
  if (isTRUE(saveResults)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    resultsFolder <- file.path(folder, paste0(conditionName, "_queryResults_", databaseId, "_", stamp))
    dir.create(resultsFolder, showWarnings = FALSE)
    for (label in names(data)) {
      utils::write.csv(data[[label]], file.path(resultsFolder, paste0(gsub("/", "_", label, fixed = TRUE), ".csv")), row.names = FALSE)
    }
    savedTo <- file.path(folder, paste0(conditionName, "_queryRunStatus_", databaseId, "_", stamp, ".csv"))
    utils::write.csv(results, savedTo, row.names = FALSE)
    Ithaca:::ithMaybeMessage(verbose, "Saved: ", savedTo, " ; ", resultsFolder)
  }

  list(results = results, data = data, savedTo = savedTo)
}
