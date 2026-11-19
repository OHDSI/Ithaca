#use this to get concept set information from the library by name
queryConceptSetLibrary(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetName = "%anxiety%",
  smallList = TRUE
)

exportConceptSetJson(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetName = "generalized_anxiety_disorder_relevant_diagnosis_codes",
  copyToClipboard = TRUE,
  writeFile = FALSE
)

#use this to remove a concept set from the concept set library by name
deleteConceptSetFromLibraryByCleanName(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  cleanName = "mental_health_treatment_PROCEDURE_CODES",
  dryRun = FALSE
)

#use to find concept sets with the single code (36433327) for an empty concept set
listConceptSetsWithConceptId(
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb"
)

#use this to insert a single concept set to the concept set library
#NOTE: cleanName and conceptSetName can be explicitly specified
insertConceptSetFromJson(
  cleanName = "levothyroxine brand medication codes",
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetJsonPath = "P:/rubricConceptSets/levothyroxine_brand_medication_codes/ConceptSetExpression.json"
)

insertConceptSetFromJson(
  cleanName = "mental health condition diagnosis codes",
  conceptSetName = "mental_health_condition_diagnosis_codes",
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetJsonPath = "P:/llm/rubricsJhu/suicidal ideation/mental_health_condition_diagnosis_codes/mental_health_condition_diagnosis_codes.json"
)

insertConceptSetFromJson(
  conceptSetName = "self_inflicted_superficial_injury_DIAGNOSIS_CODES",
  libraryPath = "P:/llm/rubrics/rubricConceptSetLibrary.duckdb",
  conceptSetJsonPath = "P:/llm/rubricsJhu/suicidal ideation/self_inflicted_injury_diagnosis_codes/self_inflicted_injury_diagnosis_codes.json"
)
