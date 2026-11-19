library(DBI)
library(duckdb)

{
  src <- "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
  dst <- "P:/llm/rubrics/rubricConceptSetLibrary - Copy.duckdb"

  file.copy(from = src, to = dst, overwrite = TRUE)

  duckConnection <- dbConnect(
    duckdb::duckdb(),
    dbdir = dst,
    read_only = TRUE
  )

  on.exit(dbDisconnect(duckConnection, shutdown = TRUE), add = TRUE)
}

dbGetQuery(
  duckConnection,
  "
  SELECT
    conceptSetName,
    cleanName
  FROM conceptSetCurrent
  where lower(cleanName) like lower('%surgical%')
  "
)

a <- dbGetQuery(
  connection,
  "
  SELECT
    *
  FROM conceptSetCurrent
  where cleanName = 'hydroxychloroquine medication codes'
  "
)
