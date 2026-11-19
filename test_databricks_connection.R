library(Ithaca)
library(DatabaseConnector)

configPath <- "config.yaml"
databaseId <- "OptumDod"  # change if needed

config <- Ithaca:::ithReadIthacaWorkflowConfig(configPath)
databases <- Ithaca:::ithResolveDatabasesFromConfig(config = config, envir = .GlobalEnv)

if (!(databaseId %in% names(databases))) {
  stop("Database ID not found in config: ", databaseId)
}

db <- databases[[databaseId]]
cat("Testing database:", databaseId, "\n")
cat("CDM schema:", db$cdmDatabaseSchema, "\n")
cat("Scratch schema:", db$scratchSchema, "\n")

conn <- DatabaseConnector::connect(db$connectionDetails)
on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

ping <- DatabaseConnector::querySql(conn, "SELECT current_timestamp() AS ts")
print(ping)

personSql <- paste0("SELECT COUNT(*) AS n FROM ", db$cdmDatabaseSchema, ".person")
personCount <- DatabaseConnector::querySql(conn, personSql)
print(personCount)

cat("Connection and CDM access look good.\n")
