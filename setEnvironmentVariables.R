# Fill in the values below, then run: source("setEnvironmentVariables.R")
# Do not commit real keys or tokens; keep a filled-in copy outside version control.

Sys.setenv(
  # LLM client
  GENAI_O3_ENDPOINT = "",
  GENAI_GPT4O_ENDPOINT = "",
  GENAI_API_GPT4_KEY = "",

  # Embeddings
  GENAI_EMBED_ENDPOINT = "",
  GENAI_API_EMBED_KEY = "",

  # Databricks (required)
  DATABRICKS_TOKEN = "",
  DATABRICKS_HOST = "",
  DATABRICKS_HTTP_PATH = "",

  # Databricks (optional; leave "" to use config.yaml or the defaults)
  DATABRICKS_USER = "",
  DATABRICKS_CATALOG = "",
  DATABRICKS_SCRATCH_SCHEMA = ""
)

required <- c(
  "GENAI_O3_ENDPOINT", "GENAI_GPT4O_ENDPOINT", "GENAI_API_GPT4_KEY",
  "GENAI_EMBED_ENDPOINT", "GENAI_API_EMBED_KEY",
  "DATABRICKS_TOKEN", "DATABRICKS_HOST", "DATABRICKS_HTTP_PATH"
)
missing <- required[!nzchar(Sys.getenv(required))]
if (length(missing) > 0L) {
  message("Still empty: ", paste(missing, collapse = ", "))
}
