library(jsonlite)
library(SqlRender)
library(DatabaseConnector)
library(Helios)
library(dplyr)
library(data.table)
library(stringr)
library(Phenelope)

options(scipen = 999)

codeFolder <- "PhenotypeLibrary"
dataFolder <- "ProjectData/PheValuator"
drive <- "p"

llmClient <- ellmer::chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_gpt4o_endpoint")),
  api_version = "2023-03-15-preview",
  model = "gpt-4o",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
llmClientNonReasoning <- llmClient

#set up your LLM client - create the LLM client for one model below
llmClient <- ellmer::chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
llmClientReasoning <- llmClient

currentCcaeVersion <- 3909
dbConnectionString <- paste("jdbc:databricks://",
                            Sys.getenv("DATABRICKS_HOST"),
                            ":443/default;transportMode=http;ssl=1;AuthMech=3;httpPath=",
                            Sys.getenv("DATABRICKS_HTTP_PATH"),
                            sep='')

ccaeConnection <- list(cdmDatabaseSchema = paste0("merative_ccae.cdm_merative_ccae_v", currentCcaeVersion),
                       connectionDetails = DatabaseConnector::createConnectionDetails(dbms = "spark",
                                                                                      connectionString =  dbConnectionString,
                                                                                      user = keyring::key_get(service = "dBuserName"),
                                                                                      password = keyring::key_get(service = "dBpassword")))
database <- list(ccaeConnection)

tempEmulationSchema <- xxxxx

######## acute liver failure
caseCohortId <- 25268
caseFirstOccurrenceOnly <- FALSE
lowerAgeLimit <- 18
startDate <- '20100101'
visitType <- c(9202, 9201, 9203, 262)
baseSampleSize <- 2000000

cohortTable <- "rubric_evaluation_cohorts"
for(dbUp in 1:length(database)) {
  #############################
  #generate case cohort
  generateCohorts(cohortList = c(caseCohortId), dbList = list(database[[dbUp]]), drive = drive)
  #############################

  ParallelLogger::logInfo("Using Cohort Id ", caseCohortId, " to designate cases.")
  sqlFilename <- "createEvaluationCohort.sql"

  if (inclusionEvaluationCohortId != 0) {
    ParallelLogger::logInfo("Creating evaluation cohort subjects using visits from cohort Id: ", inclusionEvaluationCohortId)
  }
  if (exclusionEvaluationCohortId != 0) {
    ParallelLogger::logInfo("Creating evaluation cohort subjects excluding visits from cohort Id: ", exclusionEvaluationCohortId)
  }

  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = sqlFilename,
    packageName = "Phenelope",
    dbms = connectionDetails$dbms,
    cdm_database_schema = database$cdmDatabaseSchema,
    cohort_database_schema = database$workDatabaseSchema,
    cohort_database_table = cohortTable,
    tempEmulationSchema = tempEmulationSchema,
    caseCohortId = caseCohortId,
    caseFirstOccurrenceOnly = caseFirstOccurrenceOnly,
    work_database_schema = database$workDatabaseSchema,
    ageLimit = lowerAgeLimit,
    # upperAgeLimit = upperAgeLimit,
    # gender = gender,
    # race = race,
    # ethnicity = ethnicity,
    startDate = startDate,
    # endDate = endDate,
    baseSampleSize = format(baseSampleSize, scientific = FALSE),
    visitType = c(visitType)
  )
  ParallelLogger::logInfo("Creating evaluation cohort on server from sql")
  DatabaseConnector::executeSql(connection = connection, sql)
  tempTableCreated <- TRUE
}

