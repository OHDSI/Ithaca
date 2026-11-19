llmClient <- getLlmo3Client()

conditionList <- list(
  list(
    testCondition = "Bipolar II disease",
    dxCriteria = "1 inpatient/ED dx OR 2 outpatient dx",
    additionalInformation = "",
    rubricName = "Bipolar II disease",
    cohortId = 24228
  )
)

conditionList <- list(
  list(
    testCondition = "ST-elevation myocardial infarction",
    dxCriteria = "1 inpatient/ED dx",
    additionalInformation = "",
    rubricName = "ST-elevation myocardial infarction",
    cohortId = 24228
  )
)

conditionList <- list(
  list(
    testCondition = "Ischemic stroke",
    dxCriteria = "1 inpatient/ED dx",
    additionalInformation = "",
    rubricName = "Ischemic stroke",
    cohortId = 24228
  )
)

afib <- list(testCondition = "Atrial fibrillation",
             dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
             rubricName = "Atrial fibrillation",
             additionalInformation = "Atrial fibrillation is a supraventricular tachyarrhythmia defined by electrocardiographic evidence of disorganized atrial electrical activity resulting in ineffective atrial contraction, characterized by the absence of discrete P waves replaced by fibrillatory activity and an irregularly irregular ventricular response, sustained for at least 30 seconds on continuous monitoring or a standard ECG tracing. This definition encompasses all temporal subtypes, including paroxysmal, persistent, long-standing persistent, and permanent atrial fibrillation, without restriction by symptom status. It is strictly limited to true atrial fibrillation and excludes other irregular or rapid cardiac rhythms that may mimic its presentation, including atrial flutter, atrial tachycardia, multifocal atrial tachycardia, premature atrial or ventricular complexes, sinus arrhythmia, and ventricular tachyarrhythmias.",
             cohortId = 24228)

bladderCancer <- list(testCondition = "Bladder cancer",
                      dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                      rubricName = "Bladder cancer",
                      additionalInformation = "Bladder cancer is a malignant neoplasm arising in the urinary bladder, most commonly from the urothelial lining, and is defined by histopathologic confirmation of malignancy in bladder tissue obtained through cystoscopic biopsy or transurethral resection of bladder tumor (TURBT). It includes the full clinical and pathologic spectrum of disease, from non–muscle invasive bladder cancer, including carcinoma in situ, to muscle-invasive and metastatic bladder cancer, and encompasses all malignant histologic subtypes originating in the bladder, including urothelial carcinoma, squamous cell carcinoma, adenocarcinoma, small cell carcinoma, and sarcoma. The definition excludes benign or inflammatory bladder lesions and non-bladder primary urothelial malignancies, such as upper tract urothelial carcinoma arising in the renal pelvis or ureter.",
                      cohortId = 24228)

Neutropenia <- list(testCondition = "Neutropenia",
                    dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                    rubricName = "Neutropenia",
                    additionalInformation = "Neutropenia is a laboratory-defined hematologic abnormality characterized by a reduced absolute neutrophil count in peripheral blood, defined in adults as an ANC below 1,500 cells/µL, confirmed on repeat complete blood count with differential to exclude spurious or artifactual results. It includes both acute/transient and chronic forms of neutropenia, without restriction by duration, and encompasses all etiologic contexts, including acquired and congenital neutropenia, whether occurring in isolation or in association with other hematologic abnormalities. In the absence of immediate laboratory data, a presumptive clinical suspicion of neutropenia is warranted in patients presenting with signs of infection—particularly unexplained fever (temperature >= 38.0°C), chills, or mucocutaneous ulcers—who possess high-risk clinical contexts. These contexts include recent chemotherapy, immunosuppressive therapy, or a history of congenital or cyclic bone marrow disorders. ",
                    cohortId = 24228)

urinaryTractInfection <- list(testCondition = "Urinary tract infection",
                              dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                              rubricName = "Urinary tract infection",
                              additionalInformation = "Urinary tract infection (UTI) is an infectious condition involving any part of the urinary tract, including the bladder, urethra, ureters, and kidneys, and encompasses lower and upper tract infections such as cystitis, urethritis, ureteritis, and pyelonephritis. It is most commonly caused by bacterial pathogens, particularly Escherichia coli, although fungal, viral, or parasitic infections may rarely occur, especially in immunocompromised individuals. Within this construct, symptomatic UTI is defined by the presence of urinary tract symptoms—such as dysuria, frequency, urgency, suprapubic pain, flank pain, or systemic features consistent with upper tract involvement—together with objective evidence of urinary inflammation or infection, including pyuria on urinalysis and, when available, significant bacteriuria on urine culture. Asymptomatic bacteriuria is included as a related subtype defined by significant bacteriuria in the absence of urinary symptoms. Fever is not required for the diagnosis of UTI but may indicate upper urinary tract involvement. This definition excludes noninfectious and non-urinary infectious conditions that may mimic UTI, including interstitial cystitis, sexually transmitted infections, nephrolithiasis, and other alternative causes of urinary symptoms.",
                              cohortId = 24228)

Seizure <- list(testCondition = "Seizure",
                dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                rubricName = "Seizure",
                additionalInformation = "A seizure is a clinically defined, paroxysmal episode of neurologic dysfunction resulting from abnormal, excessive, and synchronous neuronal activity in the brain, characterized by a sudden and stereotyped alteration in consciousness, behavior, motor activity, sensation, or autonomic function that is distinct from the individual’s baseline state. The diagnosis is based primarily on clinical history and event semiology and may be supported by ancillary findings such as postictal features or electroencephalographic abnormalities when available, but does not require confirmatory testing. This definition encompasses the full spectrum of seizure types, including focal, generalized, and unknown-onset seizures with motor or non-motor manifestations, and includes both provoked and unprovoked events. It excludes non-epileptic causes of transient neurologic symptoms, including syncope, metabolic or toxic disturbances, psychogenic nonepileptic events, and other physiologic conditions that mimic seizure activity.",
                cohortId = 24228)

acuteLiverInjury <- list(testCondition = "acute Liver Injury",
                         dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                         rubricName = "acute Liver Injury",
                         additionalInformation = "Acute liver injury (ALI) is a clinically significant, symptomatic condition characterized by a rapid onset (over days to weeks) of hepatocellular injury and/or cholestasis, resulting in abnormal liver biochemistry accompanied by clinical manifestations that prompt medical evaluation or care. It reflects a true acute non-traumatic pathological process in the liver, with evidence of hepatocellular dysfunction and/or impaired bile flow, and may occur in individuals without pre-existing liver disease or as an acute deterioration superimposed on chronic liver disease (acute-on-chronic liver injury), provided there is a clear deviation from baseline. The definition excludes non-hepatic causes of liver enzyme abnormalities, mechanical or physical hepatic trauma, and does not include stable chronic liver disease in the absence of an acute change.",
                         cohortId = 24228)

modSevAtopicDermatitis <- list(testCondition = "Moderate to Severe Atopic dermatitis",
                               dxCriteria = "1) One Condition diagnosis code in an inpatient or emergency department setting, 2) OR two Condition diagnosis codes separated by 1-60 days in an outpatient setting separated by will count for a 40-50% of a score needed for YES.
             Examples: if YES requires 5 points, the condition codes should count for 2 points, if YES requires 6 points it should count for 3 points.",
                               rubricName = "Moderate to Severe Atopic dermatitis",
                               additionalInformation = "Moderate-to-severe atopic dermatitis is a clinically diagnosed, chronic, relapsing inflammatory skin disorder representing a higher-severity subset of atopic dermatitis, characterized by substantial disease burden defined by either objective measures of extent and lesion severity (e.g., Investigator’s Global Assessment ≥3, elevated Eczema Area and Severity Index, or significant body surface area involvement) or by marked symptom burden and functional impairment, including intense pruritus, sleep disturbance, and reduced quality of life. This severity state requires inadequate control with optimized topical therapies, reflecting the need for therapeutic escalation. The condition is assessed using a multidimensional framework incorporating clinician-reported measures and patient-reported outcomes, and excludes mild atopic dermatitis as well as other dermatologic or systemic conditions that may mimic its presentation, such as psoriasis, contact dermatitis, seborrheic dermatitis, scabies, and cutaneous T-cell lymphoma.",
                               cohortId = 24228)



conditionList <- list(afib, bladderCancer, Neutropenia, urinaryTractInfection, Seizure,
                      acuteLiverInjury, modSevAtopicDermatitis)

getRubrics(
  conditionList = conditionList,
  outputDirectory = "P:/llm/rubricsPears",
  includeLabs = FALSE,
  llmClient = llmClient,
  verbose = TRUE
)

#edit the csv file (if changes are required) then run this
applyConciseRubricEdits(conditionName = "Rheumatoid arthritis",
                        outputDirectory = "P:/llm/largeScalePhenotyping2",
                        editedConciseRubricPath = "P:/llm/largeScalePhenotyping2/Rheumatoid arthritis/Rheumatoid arthritis_conciseRubric_V1 - Edit.csv",
                        updateSummary = TRUE,
                        verbose = TRUE)

#edit the csv file for all conditions (if changes are required) then run this
# for(conditionUp in 2:length(conditionList)) {
for(conditionUp in 7:7) {
  applyConciseRubricEdits(conditionName = conditionList[[conditionUp]]$testCondition,
                          outputDirectory = "P:/llm/rubricsPears",
                          editedConciseRubricPath = paste0("P:/llm/rubricsPears/",
                                                           conditionList[[conditionUp]]$testCondition, "/",
                                                           conditionList[[conditionUp]]$testCondition, "_conciseRubric_V1 - Edit.csv"),
                          updateSummary = TRUE,
                          verbose = TRUE)
}

###### using config and condition csv
library(Ithaca)

# Read + normalize config (handles defaults/common/contextual/run and CSV conditionSpecs)
cfg <- Ithaca:::ithReadIthacaWorkflowConfig("config.yaml")

# Build reasoning client from config (falls back to built-in o3 defaults)
llmClient <- Ithaca:::ithGetLlmClientFromConfig(cfg, "reasoning")

# Execute rubric generation only
getRubrics(
  conditionList = cfg$conditionSpecs,
  outputDirectory = as.character(cfg$outputDirectory[[1]]),
  rubricTemplates = cfg$rubricTemplates,
  includeLabs = isTRUE(cfg$includeLabs),
  promptDir = cfg$promptDir,
  llmClient = llmClient,
  useLibraryHints = isTRUE(cfg$useLibraryHints),
  libraryHintSimilarityThreshold = as.numeric(Ithaca:::ithCoalesce(cfg$libraryHintSimilarityThreshold, 0.94)[[1]]),
  libraryHintTopCandidates = as.integer(Ithaca:::ithCoalesce(cfg$libraryHintTopCandidates, 5)[[1]]),
  useExistingRubric = isTRUE(cfg$useExistingRubric),
  verbose = isTRUE(Ithaca:::ithCoalesce(cfg$verbose, TRUE))
)

