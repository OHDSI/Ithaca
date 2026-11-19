library(stringi)
library(DatabaseConnector)
library(SqlRender)

source("./tempInsertBulkTable.R")

database <- list(DOD_OHDA_DBSpec)

train <- read.csv("P:/llm/FinalCohortReferenceSet_Training.csv")
train <- read.csv("P:/llm/FinalCohortReferenceSet_Test.csv")

conditionList <- list(list(condition = "multipleMyeloma", rubricName = "Multiple Myeloma", cohortId = 15726),
                      list(condition = "bladderCancer", rubricName = "Bladder cancer", cohortId = 23784),
                      list(condition = "atrialFibrillation", rubricName = "Atrial fibrillation", cohortId = 23896),
                      list(condition = "majorDepressiveDisorder", rubricName = "Major Depressive Disorder", cohortId = 24046),
                      list(condition = "neutropenia", rubricName = "Neutropenia", cohortId = 24047),
                      list(condition = "urinaryTractInfection", rubricName = "Urinary tract infection", cohortId = 24048),
                      list(condition = "teStroke", rubricName = "Ischemic stroke", cohortId = 24049),
                      list(condition = "acuteLiverInjury", rubricName = "acute Liver Injury", cohortId = 24050),
                      list(condition = "seizure", rubricName = "Seizure", cohortId = 24051),
                      list(condition = "atopicDermatitis", rubricName = "Moderate to Severe Atopic dermatitis", cohortId = 24052))

connection <- connect(database$connectionDetails)

Sys.setenv(
  "AZR_STORAGE_ACCOUNT" = "episharedcentralbackup",
  "AZR_ACCOUNT_KEY" = Sys.getenv("AZR_ACCOUNT_KEY"),
  "AZR_CONTAINER_NAME" = "bulkloading")

tempTable <- gsub("[.]", "_", paste0("pv_table_", database[[1]]$cdmDatabaseSchema)) #this will hold all the cohorts for all the runs

{
  condUp <- train[toupper(train$phenotype) == toupper("Moderate to Severe Atopic dermatitis"),c("phenotype", "personId" , "cohortStartDate")]
  names(condUp) <- c("cohortDefinitionId", "subjectId", "cohortStartDate")
  condUp$cohortDefinitionId <- 99999
  condUp$cohortEndDate <- condUp$cohortStartDate
  condUp$subjectId <- as.double(condUp$subjectId)
  condUp$cohortDefinitionId <- as.integer(condUp$cohortDefinitionId)

  upload <- bulkLoadCohortExistingTable(database = database[[1]], cohortFile = condUp, cohortTableName = tempTable)

  # connection <- connect(database[[1]]$connectionDetails)
  # insertTable(
  #   connection = connection,
  #   databaseSchema = database[[1]]$workDatabaseSchema,
  #   tableName = preTempTable,
  #   data = condUp,
  #   dropTableIfExists = T,
  #   createTable = T,
  #   tempTable = FALSE,
  #   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  #   bulkLoad = T,
  #   progressBar = T,
  #   camelCaseToSnakeCase = T
  # )
}

testCond <- train[toupper(train$phenotype) == toupper("Moderate to Severe Atopic dermatitis"),c("phenotype", "personId" , "cohortStartDate")]
