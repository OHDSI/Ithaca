source("./functions.R")
source("./extras/llmTestingHeader.R")

systemicLupusErythematosus <- list(conceptList = c(257628),
                                   condition = "Systemic lupus erythematosus",
                                   excludedConditions = "none",
                                   excludeCauses = F,
                                   belowMinimumCountApproach = "TEST ALL",
                                   minCount = 0,
                                   conceptSetName = "[LLM Testing] Systemic lupus erythematosus",
                                   outputFolder = "p:/shared/llm/llm testing/Systemic lupus erythematosus",
                                   additionalInformation = c("p:/shared/llm/llm testing/Systemic lupus erythematosus/Systemic lupus erythematosus.docx",
                                                             file.path("./docs", "chronic.docx")),
                                   tries = 1,
                                   successes = 1)

dermatomyositis <- list(conceptList = c(80182),
                        condition = "Dermatomyositis",
                        excludedConditions = "none",
                        excludeCauses = F,
                        belowMinimumCountApproach = "TEST ALL",
                        minCount = 0,
                        conceptSetName = "[LLM Testing] Dermatomyositis",
                        outputFolder = "p:/shared/llm/llm testing/Dermatomyositis",
                        additionalInformation = c("p:/shared/llm/llm testing/Dermatomyositis/Dermatomyositis.docx",
                                                  file.path("./docs", "chronic.docx")),
                        tries = 1,
                        successes = 1)

hiv <- list(conceptList = c(439727),
            condition = "Human immunodeficiency virus infection",
            excludedConditions = "none",
            excludeCauses = F,
            belowMinimumCountApproach = "TEST ALL",
            minCount = 0,
            conceptSetName = "[LLM Testing] Human immunodeficiency virus infection",
            outputFolder = "p:/shared/llm/llm testing/Human immunodeficiency virus infection",
            additionalInformation = c("p:/shared/llm/llm testing/Human immunodeficiency virus infection/Human immunodeficiency virus infection.docx",
                                      file.path("./docs", "chronic.docx")),
            tries = 1,
            successes = 1)

influenza <- list(conceptList = c(4266367),
                  condition = "Influenza",
                  excludedConditions = "none",
                  excludeCauses = F,
                  belowMinimumCountApproach = "TEST ALL",
                  minCount = 0,
                  conceptSetName = "[LLM Testing] Influenza",
                  outputFolder = "p:/shared/llm/llm testing/Influenza",
                  additionalInformation = c("p:/shared/llm/llm testing/Human immunodeficiency virus infection/Human immunodeficiency virus infection.docx",
                                            file.path("./docs", "acute.docx")),
                  tries = 1,
                  successes = 1)

tuberculosis <- list(conceptList = c(434557),
                     condition = "Tuberculosis",
                     excludedConditions = "none",
                     excludeCauses = F,
                     belowMinimumCountApproach = "TEST ALL",
                     minCount = 0,
                     conceptSetName = "[LLM Testing] Tuberculosis",
                     outputFolder = "p:/shared/llm/llm testing/Tuberculosis",
                     additionalInformation = c("p:/shared/llm/llm testing/Tuberculosis/Tuberculosis.docx",
                                               file.path("./docs", "chronic.docx")),
                     tries = 1,
                     successes = 1)

diabeticRetinopathy  <- list(conceptList = c(4174977),
                             condition = "Diabetic retinopathy ",
                             excludedConditions = "none",
                             excludeCauses = F,
                             belowMinimumCountApproach = "TEST ALL",
                             minCount = 0,
                             conceptSetName = "[LLM Testing] Diabetic retinopathy",
                             outputFolder = "p:/shared/llm/llm testing/Diabetic retinopathy",
                             additionalInformation = c("p:/shared/llm/llm testing/Diabetic retinopathy/Diabetic retinopathy.docx",
                                                       file.path("./docs", "chronic.docx")),
                             tries = 1,
                             successes = 1)

macularDegeneration  <- list(conceptList = c(374028),
                             condition = "Age-related macular degeneration",
                             excludedConditions = "none",
                             excludeCauses = F,
                             belowMinimumCountApproach = "TEST ALL",
                             minCount = 0,
                             conceptSetName = "[LLM Testing] Age-related macular degeneration",
                             outputFolder = "p:/shared/llm/llm testing/Age-related macular degeneration",
                             additionalInformation = c("p:/shared/llm/llm testing/Age-related macular degeneration/Age-related macular degeneration.docx",
                                                       file.path("./docs", "chronic.docx")),
                             tries = 1,
                             successes = 1)

diabeticMacularEdema  <- list(conceptList = c(380097),
                              condition = "Diabetic macular edema",
                              excludedConditions = "none",
                              excludeCauses = F,
                              belowMinimumCountApproach = "TEST ALL",
                              minCount = 0,
                              conceptSetName = "[LLM Testing] Diabetic macular edema",
                              outputFolder = "p:/shared/llm/llm testing/Diabetic macular edema",
                              additionalInformation = c("p:/shared/llm/llm testing/Diabetic macular edema/Diabetic macular edema.docx",
                                                        file.path("./docs", "acute.docx")),
                              tries = 1,
                              successes = 1)

rheumatoidArthritis  <- list(conceptList = c(80809),
                             condition = "Rheumatoid Arthritis",
                             excludedConditions = "none",
                             excludeCauses = F,
                             belowMinimumCountApproach = "TEST ALL",
                             minCount = 0,
                             conceptSetName = "[LLM Testing] Rheumatoid Arthritis",
                             outputFolder = "p:/shared/llm/llm testing/Rheumatoid Arthritis",
                             additionalInformation = c("p:/shared/llm/llm testing/Rheumatoid Arthritis/Rheumatoid Arthritis.docx",
                                                       file.path("./docs", "chronic.docx")),
                             tries = 1,
                             successes = 1)

psoriaticArthritis  <- list(conceptList = c(40319772),
                            condition = "Psoriatic Arthritis",
                            excludedConditions = "none",
                            excludeCauses = F,
                            belowMinimumCountApproach = "TEST ALL",
                            minCount = 0,
                            conceptSetName = "[LLM Testing] Psoriatic Arthritis",
                            outputFolder = "p:/shared/llm/llm testing/Psoriatic Arthritis",
                            additionalInformation = c("p:/shared/llm/llm testing/Psoriatic Arthritis/Psoriatic Arthritis.docx",
                                                      file.path("./docs", "chronic.docx")),
                            tries = 1,
                            successes = 1)

sarcoidosis  <- list(conceptList = c(438688),
                     condition = "Sarcoidosis",
                     excludedConditions = "none",
                     excludeCauses = F,
                     belowMinimumCountApproach = "TEST ALL",
                     minCount = 0,
                     conceptSetName = "[LLM Testing] Sarcoidosis",
                     outputFolder = "p:/shared/llm/llm testing/Sarcoidosis",
                     additionalInformation = c("p:/shared/llm/llm testing/Sarcoidosis/Sarcoidosis.docx",
                                               file.path("./docs", "chronic.docx")),
                     tries = 1,
                     successes = 1)

interstitialLungDisease   <- list(conceptList = c(4119786),
                                  condition = "Interstitial Lung Disease",
                                  excludedConditions = "none",
                                  excludeCauses = F,
                                  belowMinimumCountApproach = "TEST ALL",
                                  minCount = 0,
                                  conceptSetName = "[LLM Testing] Interstitial Lung Disease",
                                  outputFolder = "p:/shared/llm/llm testing/Interstitial Lung Disease",
                                  additionalInformation = c("p:/shared/llm/llm testing/Interstitial Lung Disease/Interstitial Lung Disease.docx",
                                                            file.path("./docs", "chronic.docx")),
                                  tries = 1,
                                  successes = 1)

conditionList <- list(systemicLupusErythematosus, systemicSclerosis, dermatomyositis, hiv, influenza, tuberculosis, diabeticRetinopathy,
                      macularDegeneration, diabeticMacularEdema, rheumatoidArthritis, psoriaticArthritis, sarcoidosis, interstitialLungDisease)

pulmonaryFibrosis <- list(conceptList = c(4197819),
                          condition = "pulmonary fibrosis",
                          excludedConditions = "none",
                          excludeCauses = F,
                          belowMinimumCountApproach = "TEST ALL",
                          minCount = 0,
                          conceptSetName = "[LLM Testing] pulmonary fibrosis",
                          outputFolder = "p:/shared/llm/llm testing/pulmonary fibrosis",
                          additionalInformation = c("p:/shared/llm/llm testing/pulmonary fibrosis/pulmonary fibrosis.docx",
                                                    file.path("./docs", "chronic.docx")),
                          tries = 1,
                          successes = 1)

ovarianCancer <- list(conceptList = c(200051),
                      condition = "Primary malignant neoplasm of ovary",
                      excludedConditions = "none",
                      excludeCauses = F,
                      belowMinimumCountApproach = "TEST ALL",
                      minCount = 0,
                      conceptSetName = "[LLM Testing] Primary malignant neoplasm of ovary",
                      outputFolder = "p:/shared/llm/llm testing/Primary malignant neoplasm of ovary",
                      additionalInformation = c("p:/shared/llm/llm testing/Primary malignant neoplasm of ovary/Primary malignant neoplasm of ovary.docx",
                                                file.path("./docs", "chronic.docx")),
                      tries = 1,
                      successes = 1)

conditionList <- list(pulmonaryFibrosis, ovarianCancer)

deepVeinThrombosis <- list(conceptList = c(4133004),
                           condition = "Deep vein thrombosis",
                           excludedConditions = "none",
                           excludeCauses = F,
                           belowMinimumCountApproach = "TEST ALL",
                           minCount = 0,
                           conceptSetName = "[LLM Testing] Deep vein thrombosis",
                           outputFolder = "p:/shared/llm/llm testing/Deep vein thrombosis",
                           additionalInformation = c("p:/shared/llm/llm testing/Deep vein thrombosis/Deep vein thrombosis.docx",
                                                     file.path("./docs", "chronic.docx")),
                           tries = 1,
                           successes = 1)

aorticDissection <- list(conceptList = c(4133004),
                         condition = "Aortic dissection",
                         excludedConditions = "none",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST ALL",
                         minCount = 0,
                         conceptSetName = "[LLM Testing] Aortic dissection",
                         outputFolder = "p:/shared/llm/llm testing/Aortic dissection",
                         additionalInformation = c("p:/shared/llm/llm testing/Aortic dissection/Aortic dissection.docx",
                                                   file.path("./docs", "acute.docx")),

                         tries = 1,
                         successes = 1)

rightHeartFailure <- list(conceptList = c(4273632),
                          condition = "Right heart failure",
                          excludedConditions = "none",
                          excludeCauses = F,
                          belowMinimumCountApproach = "TEST ALL",
                          minCount = 0,
                          conceptSetName = "[LLM Testing] Right heart failure",
                          outputFolder = "p:/shared/llm/llm testing/Right heart failure",
                          additionalInformation = c("p:/shared/llm/llm testing/Right heart failure/Right heart failure.docx",
                                                    file.path("./docs", "chronic.docx")),
                          tries = 1,
                          successes = 1)

conditionList <- list(deepVeinThrombosis, aorticDissection, rightHeartFailure)

systemicSclerosis <- list(conceptList = c(134442),
                          condition = "Systemic sclerosis",
                          excludedConditions = "none",
                          excludeCauses = F,
                          belowMinimumCountApproach = "TEST ALL",
                          minCount = 0,
                          conceptSetName = "[LLM Testing] Systemic sclerosis",
                          outputFolder = "p:/shared/llm/llm testing/Systemic sclerosis",
                          additionalInformation = c("p:/shared/llm/llm testing/Systemic sclerosis/Systemic sclerosis.docx",
                                                    file.path("./docs", "chronic.docx")),
                          tries = 1,
                          successes = 1)

syphilis <- list(conceptList = c(436033),
                 condition = "Syphilis",
                 excludedConditions = "none",
                 excludeCauses = F,
                 belowMinimumCountApproach = "TEST ALL",
                 minCount = 0,
                 conceptSetName = "[LLM Testing] Syphilis",
                 outputFolder = "p:/shared/llm/llm testing/Syphilis",
                 additionalInformation = c("p:/shared/llm/llm testing/Syphilis/Syphilis.docx",
                                           file.path("./docs", "chronic.docx")),
                 tries = 1,
                 successes = 1)

acuteRespiratoryDistressSyndrome <- list(conceptList = c(436033),
                                         condition = "Acute Respiratory Distress Syndrome",
                                         excludedConditions = "none",
                                         excludeCauses = F,
                                         belowMinimumCountApproach = "TEST ALL",
                                         minCount = 0,
                                         conceptSetName = "[LLM Testing] Acute Respiratory Distress Syndrome",
                                         outputFolder = "p:/shared/llm/llm testing/Acute Respiratory Distress Syndrome",
                                         additionalInformation = c("p:/shared/llm/llm testing/Acute Respiratory Distress Syndrome/Acute Respiratory Distress Syndrome.docx",
                                                                   file.path("./docs", "acute.docx")),
                                         tries = 1,
                                         successes = 1)

sepsis <- list(conceptList = c(132797),
               condition = "Sepsis",
               excludedConditions = "none",
               excludeCauses = F,
               belowMinimumCountApproach = "TEST ALL",
               minCount = 0,
               conceptSetName = "[LLM Testing] Sepsis",
               outputFolder = "p:/shared/llm/llm testing/Sepsis",
               additionalInformation = c("p:/shared/llm/llm testing/Sepsis/Sepsis.docx",
                                         file.path("./docs", "acute.docx")),
               tries = 1,
               successes = 1)


pulmonaryEmbolism <- list(conceptList = c(440417),
                          condition = "Pulmonary embolism",
                          excludedConditions = "none",
                          excludeCauses = F,
                          belowMinimumCountApproach = "TEST ALL",
                          minCount = 0,
                          conceptSetName = "[LLM Testing] Pulmonary embolism",
                          outputFolder = "p:/shared/llm/llm testing/Pulmonary embolism",
                          additionalInformation = c("p:/shared/llm/llm testing/Pulmonary embolism/Pulmonary embolism.docx",
                                                    file.path("./docs", "acute.docx")),
                          tries = 1,
                          successes = 1)

unstableAngina <- list(conceptList = c(315296),
                       condition = "Unstable Angina",
                       excludedConditions = "none",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST ALL",
                       minCount = 0,
                       conceptSetName = "[LLM Testing] Unstable Angina",
                       outputFolder = "p:/shared/llm/llm testing/Unstable Angina",
                       additionalInformation = c("p:/shared/llm/llm testing/Unstable Angina/Unstable Angina.docx",
                                                 file.path("./docs", "acute.docx")),
                       tries = 1,
                       successes = 1)

systemicLupusErythematosus <- list(conceptList = c(257628),
                                   condition = "Systemic lupus erythematosus",
                                   excludedConditions = "none",
                                   excludeCauses = F,
                                   belowMinimumCountApproach = "TEST ALL",
                                   minCount = 0,
                                   conceptSetName = "[LLM Testing] Systemic lupus erythematosus",
                                   outputFolder = "p:/shared/llm/llm testing/Systemic lupus erythematosus",
                                   additionalInformation = c("p:/shared/llm/llm testing/Systemic lupus erythematosus/Systemic lupus erythematosus.docx",
                                                             file.path("./docs", "chronic.docx")),
                                   tries = 1,
                                   successes = 1)

diabeticRetinopathy <- list(conceptList = c(4174977),
                            condition = "Diabetic retinopathy",
                            excludedConditions = "none",
                            excludeCauses = F,
                            belowMinimumCountApproach = "TEST ALL",
                            minCount = 0,
                            conceptSetName = "[LLM Testing] Diabetic retinopathy",
                            outputFolder = "p:/shared/llm/llm testing/Diabetic retinopathy",
                            additionalInformation = c("p:/shared/llm/llm testing/Diabetic retinopathy/Diabetic retinopathy.docx",
                                                      file.path("./docs", "chronic.docx")),
                            tries = 1,
                            successes = 1)

ageRelatedMacularDegeneration <- list(conceptList = c(374028),
                                      condition = "Age-related macular degeneration including Exudative, Nonexudative, and Drusen stages",
                                      excludedConditions = "none",
                                      excludeCauses = F,
                                      belowMinimumCountApproach = "TEST ALL",
                                      minCount = 0,
                                      conceptSetName = "[LLM Testing] Age-related macular degeneration",
                                      outputFolder = "p:/shared/llm/llm testing/Age-related macular degeneration",
                                      additionalInformation = c("p:/shared/llm/llm testing/Age-related macular degeneration/Age-related macular degeneration.docx",
                                                                file.path("./docs", "chronic.docx")),
                                      tries = 1,
                                      successes = 1)

uveitis <- list(conceptList = c(4028363),
                condition = "Uveitis including sarcoid uveitis, syphilis uveitis, VKH, HSV anterior uveitis",
                excludedConditions = "none",
                excludeCauses = F,
                belowMinimumCountApproach = "TEST ALL",
                minCount = 0,
                conceptSetName = "[LLM Testing] Uveitis",
                outputFolder = "p:/shared/llm/llm testing/Uveitis",
                additionalInformation = c("p:/shared/llm/llm testing/Uveitis/Uveitis.docx",
                                          file.path("./docs", "acute.docx")),
                tries = 1,
                successes = 1)

hypertrophicObstructiveCardiomyopathy <- list(conceptList = c(316428),
                                              condition = "Hypertrophic obstructive cardiomyopathy",
                                              excludedConditions = "none",
                                              excludeCauses = F,
                                              belowMinimumCountApproach = "TEST ALL",
                                              minCount = 0,
                                              conceptSetName = "[LLM Testing] Hypertrophic obstructive cardiomyopathy",
                                              outputFolder = "p:/shared/llm/llm testing/Hypertrophic obstructive cardiomyopathy",
                                              additionalInformation = c("p:/shared/llm/llm testing/Hypertrophic obstructive cardiomyopathy/Hypertrophic obstructive cardiomyopathy.docx",
                                                                        file.path("./docs", "chronic.docx")),
                                              tries = 1,
                                              successes = 1)

sepsis <- list(conceptList = c(132797),
               condition = "Sepsis",
               excludedConditions = "none",
               excludeCauses = F,
               belowMinimumCountApproach = "TEST ALL",
               minCount = 0,
               conceptSetName = "[LLM Testing] Sepsis",
               outputFolder = "p:/shared/llm/llm testing/Sepsis",
               additionalInformation = c("p:/shared/llm/llm testing/Sepsis/Sepsis.docx",
                                         file.path("./docs", "acute.docx")),
               tries = 1,
               successes = 1)

headNeckCancer <- list(conceptList = c(4114222),
                       condition = "Head and Neck Cancer",
                       excludedConditions = "none",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] Head and Neck Cancer 3",
                       outputFolder = "p:/shared/llm/Epi1324/Head and Neck Cancer 3",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

colorectalCancer <- list(conceptList = c(37168850),
                         condition = "Colorectal cancer",
                         excludedConditions = "none",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST PHOEBE",
                         minCount = 1,
                         conceptSetName = "[LLM Testing] Colorectal cancer 2",
                         outputFolder = "p:/shared/llm/Epi1324/Colorectal cancer 2",
                         clinicalContext = "in the general population",
                         additionalInformation = "",
                         tries = 1,
                         successes = 1)

hematemesis <- list(conceptList = c(26727,4145805),
                    condition = "Hematemesis",
                    excludedConditions = "none",
                    excludeCauses = F,
                    belowMinimumCountApproach = "TEST PHOEBE",
                    minCount = 1,
                    conceptSetName = "[LLM Testing] Hematemesis",
                    outputFolder = "p:/shared/llm/EPI1327/Hematemesis",
                    clinicalContext = "in the general population",
                    additionalInformation = "",
                    tries = 1,
                    successes = 1)

pepticUlcer <- list(conceptList = c(4027663,4232840),
                    condition = "Peptic ulcer",
                    excludedConditions = "none",
                    excludeCauses = F,
                    belowMinimumCountApproach = "TEST PHOEBE",
                    minCount = 1,
                    conceptSetName = "[LLM Testing] Peptic ulcer",
                    outputFolder = "p:/shared/llm/EPI1327/Peptic ulcer",
                    clinicalContext = "in the general population",
                    additionalInformation = "",
                    tries = 1,
                    successes = 1)

melena <- list(conceptList = c(4103703),
               condition = "Melena ",
               excludedConditions = "none",
               excludeCauses = F,
               belowMinimumCountApproach = "TEST PHOEBE",
               minCount = 1,
               conceptSetName = "[LLM Testing] Melena",
               outputFolder = "p:/shared/llm/EPI1327/Melena",
               clinicalContext = "in the general population",
               additionalInformation = "",
               tries = 1,
               successes = 1)

haematochezia <- list(conceptList = c(443530,4026112),
                      condition = "Haematochezia",
                      excludedConditions = "none",
                      excludeCauses = F,
                      belowMinimumCountApproach = "TEST PHOEBE",
                      minCount = 1,
                      conceptSetName = "[LLM Testing] Haematochezia",
                      outputFolder = "p:/shared/llm/EPI1327/Haematochezia",
                      clinicalContext = "in the general population",
                      additionalInformation = "",
                      tries = 1,
                      successes = 1)

gastrointestinalUlcer <- list(conceptList = c(4247120),
                              condition = "Gastrointestinal ulcer",
                              excludedConditions = "none",
                              excludeCauses = F,
                              belowMinimumCountApproach = "TEST PHOEBE",
                              minCount = 1,
                              conceptSetName = "[LLM Testing] Gastrointestinal ulcer",
                              outputFolder = "p:/shared/llm/EPI1327/Gastrointestinal ulcer",
                              clinicalContext = "in the general population",
                              additionalInformation = "",
                              tries = 1,
                              successes = 1)

gastrointestinalPerforation <- list(conceptList = c(4202064),
                                    condition = "Gastrointestinal perforation",
                                    excludedConditions = "none",
                                    excludeCauses = F,
                                    belowMinimumCountApproach = "TEST PHOEBE",
                                    minCount = 1,
                                    conceptSetName = "[LLM Testing] Gastrointestinal perforation",
                                    outputFolder = "p:/shared/llm/EPI1327/Gastrointestinal perforation",
                                    clinicalContext = "in the general population",
                                    additionalInformation = "",
                                    tries = 1,
                                    successes = 1)

malloryWeiss <- list(conceptList = c(316457,4145716),
                     condition = "Mallory-Weiss syndrome",
                     excludedConditions = "none",
                     excludeCauses = F,
                     belowMinimumCountApproach = "TEST PHOEBE",
                     minCount = 1,
                     conceptSetName = "[LLM Testing] Mallory-Weiss syndrome",
                     outputFolder = "p:/shared/llm/test/Mallory-Weiss syndrome",
                     clinicalContext = "in the general population",
                     additionalInformation = "",
                     tries = 1,
                     successes = 1)

gastritis  <- list(conceptList = c(201340),
                   condition = "Gastritis",
                   excludedConditions = "none",
                   excludeCauses = F,
                   belowMinimumCountApproach = "TEST PHOEBE",
                   minCount = 1,
                   conceptSetName = "[LLM Testing] Gastritis",
                   outputFolder = "p:/shared/llm/EPI1327/Gastritis",
                   clinicalContext = "in the general population",
                   additionalInformation = "",
                   tries = 1,
                   successes = 1)

chronicGastritis  <- list(conceptList = c(4225273),
                          condition = "Chronic Gastritis",
                          excludedConditions = "none",
                          excludeCauses = F,
                          belowMinimumCountApproach = "TEST PHOEBE",
                          minCount = 1,
                          conceptSetName = "[LLM Testing] Chronic Gastritis",
                          outputFolder = "p:/shared/llm/EPI1327/Chronic Gastritis",
                          clinicalContext = "in the general population",
                          additionalInformation = "",
                          tries = 1,
                          successes = 1)

peritonitis  <- list(conceptList = c(196152),
                     condition = "peritonitis",
                     excludedConditions = "none",
                     excludeCauses = F,
                     belowMinimumCountApproach = "TEST PHOEBE",
                     minCount = 1,
                     conceptSetName = "[LLM Testing] peritonitis",
                     outputFolder = "p:/shared/llm/EPI1327/peritonitis",
                     clinicalContext = "in the general population",
                     additionalInformation = "",
                     tries = 1,
                     successes = 1)

analAbscess  <- list(conceptList = c(45772085),
                     condition = "Anal abscess",
                     excludedConditions = "none",
                     excludeCauses = F,
                     belowMinimumCountApproach = "TEST PHOEBE",
                     minCount = 1,
                     conceptSetName = "[LLM Testing] Anal abscess",
                     outputFolder = "p:/shared/llm/EPI1327/Anal abscess",
                     clinicalContext = "in the general population",
                     additionalInformation = "",
                     tries = 1,
                     successes = 1)

colitis  <- list(conceptList = c(4272488),
                 condition = "Colitis",
                 excludedConditions = "none",
                 excludeCauses = F,
                 belowMinimumCountApproach = "TEST PHOEBE",
                 minCount = 1,
                 conceptSetName = "[LLM Testing] Colitis",
                 outputFolder = "p:/shared/llm/EPI1327/Colitis",
                 clinicalContext = "in the general population",
                 additionalInformation = "",
                 tries = 1,
                 successes = 1)

headNeckCancer <- list(conceptList = c(4114222),
                       condition = "Head and Neck Cancer",
                       excludedConditions = "cancer of bones, cancer of esophagus, Cancer of brain, meninges, and cranial nerves, cancer of eye,
                       cancer of skin, hematological or lymphoproliferative cancers, metastases to the head and neck region,
                       cancer of thyroid and parathyroid gland, in situ cancers of head and neck",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] Head and Neck Cancer 5",
                       outputFolder = "p:/shared/llm/Epi1324/Head and Neck Cancer 5",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

gastricCancer1 <- list(conceptList = c(443387),
                       condition = "Gastric cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] Gastric cancer 1 (443387)",
                       outputFolder = "p:/shared/llm/Patrick/Gastric cancer 1 (443387)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

gastricCancer2 <- list(conceptList = c(4131611),
                       condition = "Gastric cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] Gastric cancer 2 (4131611)",
                       outputFolder = "p:/shared/llm/Patrick/Gastric cancer 2 (4131611)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

gastricCancer3 <- list(conceptList = c(196044),
                       condition = "Gastric cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] Gastric cancer 3 (196044)",
                       outputFolder = "p:/shared/llm/Patrick/Gastric cancer 3 (196044)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

stomachCancer1 <- list(conceptList = c(443387),
                       condition = "stomach cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] stomach cancer 1 (443387)",
                       outputFolder = "p:/shared/llm/Patrick/stomach cancer 1 (443387)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

stomachCancer2 <- list(conceptList = c(4131611),
                       condition = "stomach cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] stomach cancer 2 (4131611)",
                       outputFolder = "p:/shared/llm/Patrick/stomach cancer 2 (4131611)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

stomachCancer3 <- list(conceptList = c(196044),
                       condition = "stomach cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] stomach cancer 3 (196044)",
                       outputFolder = "p:/shared/llm/Patrick/stomach cancer 3 (196044)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

neoplasmStomach1 <- list(conceptList = c(443387),
                         condition = "Neoplasm of the stomach",
                         excludedConditions = "",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST PHOEBE",
                         minCount = 1,
                         conceptSetName = "[LLM Testing] Neoplasm of the stomach 1 (443387)",
                         outputFolder = "p:/shared/llm/Patrick/Neoplasm of the stomach 1 (443387)",
                         clinicalContext = "in the general population",
                         additionalInformation = "",
                         tries = 1,
                         successes = 1)

neoplasmStomach2 <- list(conceptList = c(4131611),
                         condition = "Neoplasm of the stomach",
                         excludedConditions = "",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST PHOEBE",
                         minCount = 1,
                         conceptSetName = "[LLM Testing] Neoplasm of the stomach 2 (4131611)",
                         outputFolder = "p:/shared/llm/Patrick/Neoplasm of the stomach 2 (4131611)",
                         clinicalContext = "in the general population",
                         additionalInformation = "",
                         tries = 1,
                         successes = 1)

neoplasmStomach3 <- list(conceptList = c(196044),
                         condition = "Neoplasm of the stomach",
                         excludedConditions = "",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST PHOEBE",
                         minCount = 1,
                         conceptSetName = "[LLM Testing] Neoplasm of the stomach 3 (196044)",
                         outputFolder = "p:/shared/llm/Patrick/Neoplasm of the stomach 3 (196044)",
                         clinicalContext = "in the general population",
                         additionalInformation = "",
                         tries = 1,
                         successes = 1)

compositeStomachCancer <- list(conceptList = c(196044, 443387, 4131611),
                               condition = "Neoplasm of the stomach, Stomach cancer, or Gastric cancer",
                               excludedConditions = "",
                               excludeCauses = F,
                               belowMinimumCountApproach = "TEST PHOEBE",
                               minCount = 1,
                               conceptSetName = "[LLM Testing] Neoplasm of the stomach, Stomach cancer, or Gastric cancer",
                               outputFolder = "p:/shared/llm/Patrick/Neoplasm of the stomach, Stomach cancer, or Gastric cancer",
                               clinicalContext = "in the general population",
                               additionalInformation = "",
                               tries = 1,
                               successes = 1)

gastricCancer4 <- list(conceptList = c(196044, 443387, 4131611),
                       condition = "Gastric cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] Gastric cancer 4 (196044, 443387, 4131611)",
                       outputFolder = "p:/shared/llm/Patrick/Gastric cancer 4 (196044, 443387, 4131611)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

stomachCancer4 <- list(conceptList = c(196044, 443387, 4131611),
                       condition = "stomach cancer",
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing] stomach cancer 4 (196044, 443387, 4131611)",
                       outputFolder = "p:/shared/llm/Patrick/stomach cancer 4 (196044, 443387, 4131611)",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

neoplasmStomach4 <- list(conceptList = c(196044, 443387, 4131611),
                         condition = "Neoplasm of the stomach",
                         excludedConditions = "",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST PHOEBE",
                         minCount = 1,
                         conceptSetName = "[LLM Testing] Neoplasm of the stomach 4 (196044, 443387, 4131611)",
                         outputFolder = "p:/shared/llm/Patrick/Neoplasm of the stomach 4 (196044, 443387, 4131611)",
                         clinicalContext = "in the general population",
                         additionalInformation = "",
                         tries = 1,
                         successes = 1)

takotsuboCardiomyopathy <- list(conceptList = c(40479589),
                                condition = "Takotsubo cardiomyopathy (Stress cardiomyopathy)",
                                excludedConditions = "acute myocardial infarction, Myocarditis, dilated cardiomyopathy, hypertrophic cardiomyopathy",
                                excludeCauses = F,
                                belowMinimumCountApproach = "TEST PHOEBE",
                                minCount = 1,
                                conceptSetName = "[LLM Testing o3] Takotsubo cardiomyopathy",
                                outputFolder = "p:/shared/llm/testO3/Takotsubo cardiomyopathy",
                                clinicalContext = "in the general population",
                                additionalInformation = "",
                                tries = 1,
                                successes = 1)

aplasticAnemia <- list(conceptList = c(137829),
                       condition = "Aplastic anemia",
                       closestConditionConcept = 137829,
                       excludedConditions = "",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 0,
                       conceptSetName = "[LLM Testing] Aplastic anemia",
                       outputFolder = "p:/shared/llm/azza/Aplastic anemia",
                       clinicalContext = "in the general population",
                       additionalInformation = "",
                       tries = 1,
                       successes = 1)

gastricCancer1 <- list(conceptList = c(443387),
                       condition = "Gastric cancer",
                       excludedConditions = "benign cancers and tumors",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing o3] Gastric cancer 1 (443387)",
                       outputFolder = "p:/shared/llm/llm testing o3/Gastric cancer 1 (443387)",
                       clinicalContext = "in the general population",
                       additionalInformation = "ALWAYS analyze gastric cancer as those including:  1) gastric lymphomas,  2) junctional cancers, 3) neuroendocrine neoplasms, 4) sarcomas.",
                       tries = 1,
                       successes = 1)

stomachCancer1 <- list(conceptList = c(443387),
                       condition = "stomach cancer",
                       excludedConditions = "benign cancers and tumors",
                       excludeCauses = F,
                       belowMinimumCountApproach = "TEST PHOEBE",
                       minCount = 1,
                       conceptSetName = "[LLM Testing o3] stomach cancer 1 (443387)",
                       outputFolder = "p:/shared/llm/llm testing o3/stomach cancer 1 (443387)",
                       clinicalContext = "in the general population",
                       additionalInformation = "ALWAYS analyze gastric cancer as those including:  1) gastric lymphomas,  2) junctional cancers, 3) neuroendocrine neoplasms, 4) sarcomas.",
                       tries = 1,
                       successes = 1)

deepVeinThrombosis <- list(conceptList = c(4133004),
                           condition = "Deep vein thrombosis",
                           excludedConditions = "Distal calf DVT (peroneal/posterior/anterior tibial, muscular veins);
                           Superficial vein thrombosis (e.g., great saphenous vein thrombosis);
                           Chronic post-thrombotic changes; Lymphedema; Venous insufficiency; Cellulitis;
                           Baker cyst; Upper-extremity DVT; Pulmonary embolism (PE)",
                           excludeCauses = F,
                           belowMinimumCountApproach = "TEST ALL",
                           minCount = 0,
                           conceptSetName = "[LLM Testing o3] Deep vein thrombosis",
                           outputFolder = "p:/shared/llm/llm testing o3/Deep vein thrombosis",
                           additionalInformation = "",
                           clinicalContext = "in the general population",
                           tries = 1,
                           successes = 1)

systemicLupusErythematosus <- list(conceptList = c(257628),
                                   condition = "Systemic lupus erythematosus",
                                   excludedConditions = " - **Cutaneous lupus erythematosus (discoid lupus, subacute cutaneous lupus)**
- **Drug-induced lupus**
- **Neonatal lupus**
- **Antiphospholipid syndrome (APS) without SLE**
- **Undifferentiated or mixed connective tissue disease (UCTD)**
- **Systemic sclerosis (scleroderma)**
- **Dermatomyositis/polymyositis**
- **Sjögren’s syndrome**
- **Rheumatoid arthritis (RA)**
- **Organ-specific diagnoses without explicit SLE linkage (e.g., nephritis, serositis, cytopenias, CNS vasculitis)**
- **Non-SLE lupus (e.g., lupus vulgaris)**
- **Fibromyalgia** ",
                                   excludeCauses = F,
                                   belowMinimumCountApproach = "TEST ALL",
                                   minCount = 0,
                                   conceptSetName = "[LLM Testing] Systemic lupus erythematosus",
                                   outputFolder = "p:/shared/llm/llm testing/Systemic lupus erythematosus",
                                   additionalInformation = "",
                                   clinicalContext = "in the general population",
                                   tries = 1,
                                   successes = 1)

diabeticMacularEdema  <- list(conceptList = c(380097),
                              condition = "Diabetic macular edema",
                              excludedConditions = "
- **Neovascular age-related macular degeneration (nAMD):** Wet AMD, Exudative AMD
- **Retinal vein occlusions (CRVO/BRVO) with macular edema:** Central Retinal Vein Occlusion, Branch Retinal Vein Occlusion
- **Polypoidal choroidal vasculopathy (PCV):** Idiopathic Polypoidal Choroidal Vasculopathy
- **Myopic choroidal neovascularization (mCNV):** Pathologic Myopia with CNV
- **Post-operative cystoid macular edema (Irvine–Gass):** Post-surgical CME
- **Uveitic cystoid macular edema:** Inflammatory CME
- **Unspecified macular edema without diabetic linkage:** Idiopathic Macular Edema
- **Diabetic retinopathy without macular edema:** Non-ME Diabetic Retinopathy ",
                              excludeCauses = F,
                              belowMinimumCountApproach = "TEST ALL",
                              minCount = 0,
                              conceptSetName = "[LLM Testing o3] Diabetic macular edema",
                              outputFolder = "p:/shared/llm/llm testing o3/Diabetic macular edema",
                              additionalInformation = "",
                              clinicalContext = "in the general population",
                              tries = 1,
                              successes = 1)

posteriorUveitis <- list(conceptList = c(4183040),
                condition = "Posterior Uveitis",
                excludedConditions = "- **Infectious posterior uveitides:** Toxoplasma posterior uveitis, herpes simplex virus (HSV), varicella-zoster virus (VZV), cytomegalovirus (CMV), tuberculosis, syphilis.
- **Intraocular lymphoma and other masquerade syndromes:** Primary vitreoretinal lymphoma, metastatic malignancies, and other non-inflammatory mimics.
- **Anterior uveitis, episcleritis/scleritis, and non-inflammatory mimics:** Conditions limited to the anterior segment or sclera without posterior segment involvement.
- **Organ-specific findings without explicit uveitis diagnosis:** Cystoid macular edema (CME), retinal vasculitis, or other isolated findings not associated with a uveitis diagnosis.  ",
                excludeCauses = F,
                belowMinimumCountApproach = "TEST ALL",
                minCount = 0,
                conceptSetName = "[LLM Testing o3] Posterior Uveitis",
                outputFolder = "p:/shared/llm/llm testing o3/Posterior Uveitis",
                additionalInformation = "",
                clinicalContext = "in the general population",
                tries = 1,
                successes = 1)

rheumatoidArthritis  <- list(conceptList = c(80809),
                             condition = "Rheumatoid Arthritis",
                             excludedConditions = "
- **Psoriatic Arthritis:** PsA, Psoriatic Joint Disease
- **Ankylosing Spondylitis and Other Spondyloarthropathies:** AS, Axial Spondyloarthritis
- **Reactive and Enteropathic Arthritis:** Reiter’s Syndrome, Arthritis Associated with IBD
- **Systemic Lupus Erythematosus:** SLE, Lupus
- **Systemic Sclerosis:** Scleroderma
- **Sjögren’s Syndrome:** Dry Eye Syndrome
- **Dermatomyositis/Polymyositis:** Inflammatory Myopathies
- **Undifferentiated/Mixed Connective Tissue Disease:** UCTD, MCTD
- **Gout:** Hyperuricemia, Crystal-Induced Arthritis
- **Calcium Pyrophosphate Disease (Pseudogout/CPPD):** CPPD, Chondrocalcinosis
- **Polymyalgia Rheumatica:** PMR
- **Osteoarthritis:** OA, Degenerative Joint Disease
- **Any Other Arthralgia or Unspecified Arthritis:** Joint Pain, Non-Specific Arthritis",
                             excludeCauses = F,
                             belowMinimumCountApproach = "TEST ALL",
                             minCount = 0,
                             conceptSetName = "[LLM Testing o3] Rheumatoid Arthritis",
                             outputFolder = "p:/shared/llm/llm testing o3/Rheumatoid Arthritis",
                             additionalInformation = "",
                             clinicalContext = "in the general population",
                             tries = 1,
                             successes = 1)

systemicSclerosis <- list(conceptList = c(134442),
                          condition = "Systemic sclerosis",
                          excludedConditions = "- **Localized Scleroderma:** Morphea (generalized/plaque/guttate), linear scleroderma, en coup de sabre.
- **Mimics/Fibrosing Conditions:** Eosinophilic fasciitis, scleredema, scleromyxedema, nephrogenic systemic fibrosis.
- **Induced Scleroderma:** Drug-induced (e.g., bleomycin, taxanes) or environmental/occupational (e.g., silica, vinyl chloride, toxic oil).
- **Other/Overlap Conditions:** GVHD with sclerodermatous features, Mixed Connective Tissue Disease (MCTD), Undifferentiated Connective Tissue Disease (UCTD), isolated Raynaud’s phenomenon, acrosclerosis, or sclerodactyly.  ",
                          excludeCauses = F,
                          belowMinimumCountApproach = "TEST ALL",
                          minCount = 0,
                          conceptSetName = "[LLM Testing o3] Systemic sclerosis",
                          outputFolder = "p:/shared/llm/llm testing o3/Systemic sclerosis",
                          additionalInformation = "",
                          clinicalContext = "in the general population",
                          tries = 1,
                          successes = 1)

conditionList <- list(systemicSclerosis, rheumatoidArthritis, gastricCancer1, stomachCancer1, deepVeinThrombosis,
                      systemicLupusErythematosus, diabeticMacularEdema, posteriorUveitis)
################################################################################################
#multiple or single run
for(conditionUp in 1:length(conditionList)) {
  finalSet <- executeLlmConceptCreate(conceptName = conditionList[[conditionUp]]$condition,
                                      originalConceptList = conditionList[[conditionUp]]$conceptList,
                                      excludedConditions = conditionList[[conditionUp]]$excludedConditions,
                                      tries = conditionList[[conditionUp]]$tries,
                                      successes = conditionList[[conditionUp]]$successes,
                                      database = database,
                                      vocabulary = database$vocabulary,
                                      minCount = conditionList[[conditionUp]]$minCount,
                                      belowMinimumCountApproach = conditionList[[conditionUp]]$belowMinimumCountApproach,
                                      conceptSetName = conditionList[[conditionUp]]$conceptSetName,
                                      baseUrl = "https://epi.jnj.com:8443/WebAPI",
                                      additionalInformation = conditionList[[conditionUp]]$additionalInformation,
                                      clinicalContext = conditionList[[conditionUp]]$clinicalContext,
                                      vocabularies = c('SNOMED', 'HCPCS', 'CPT4'),
                                      postConceptSet = T, #should the concept set be posted to ATLAS
                                      outputDirectory = conditionList[[conditionUp]]$outputFolder,
                                      quickRun = F) #set to F to go through all the steps for an "official" run (i.e., not testing)
}

source("./functions.R")

gastricCancer <- list(conceptList = c(rep(4208017, times = 10), rep(196044, times = 5), rep(200974, times = 10)),
                      condition = "gastric Cancer",
                      excludedConditions = "none",
                      excludeCauses = F,
                      conceptSetName = "[LLM Test] gastricCancer",
                      outputFolder = "p:/shared/llm/test",
                      clinicalContext = "in the general population",
                      additionalInformation = "ALWAYS analyze gastric cancer as those including:  1) gastric lymphomas,  2) junctional cancers, 3) neuroendocrine neoplasms, 4) sarcomas.",
                      tries = 1,
                      successes = 1)

stomachCancer <- list(conceptList = c(rep(4208017, times = 10), rep(196044, times = 5), rep(200974, times = 10)),
                      condition = "stomach Cancer",
                      excludedConditions = "none",
                      excludeCauses = F,
                      conceptSetName = "[LLM Test] stomachCancer",
                      outputFolder = "p:/shared/llm/test",
                      clinicalContext = "in the general population",
                      additionalInformation = "ALWAYS analyze stomach cancer as those including:  1) gastric lymphomas,  2) junctional cancers, 3) neuroendocrine neoplasms, 4) sarcomas.",
                      tries = 1,
                      successes = 1)

rheumatoidArthritis <- list(conceptList = c(rep(4116447, times = 1), rep(1076675, times = 1),
                                            rep(4103516, times = 1), rep(42539550, times = 1),
                                            rep(4300202, times = 1), rep(4296152, times = 1),
                                            rep(4311391, times = 1), rep(4299308, times = 1)),
                      condition = "rheumatoid Arthritis",
                      excludedConditions = " - **Psoriatic Arthritis:** PsA, Psoriatic Joint Disease - **Ankylosing Spondylitis and Other Spondyloarthropathies:** AS, Axial Spondyloarthritis - **Reactive and Enteropathic Arthritis:** Reiter’s Syndrome, Arthritis Associated with IBD - **Systemic Lupus Erythematosus:** SLE, Lupus - **Systemic Sclerosis:** Scleroderma - **Sjögren’s Syndrome:** Dry Eye Syndrome - **Dermatomyositis/Polymyositis:** Inflammatory Myopathies - **Undifferentiated/Mixed Connective Tissue Disease:** UCTD, MCTD - **Gout:** Hyperuricemia, Crystal-Induced Arthritis - **Calcium Pyrophosphate Disease (Pseudogout/CPPD):** CPPD, Chondrocalcinosis - **Polymyalgia Rheumatica:** PMR - **Osteoarthritis:** OA, Degenerative Joint Disease - **Any Other Arthralgia or Unspecified Arthritis:** Joint Pain, Non-Specific Arthritis",
                      excludeCauses = F,
                      conceptSetName = "[LLM Test] rheumatoid Arthritis",
                      clinicalContext = "in the general population",
                      outputFolder = "p:/shared/llm/test",
                      tries = 1,
                      successes = 1)

rheumatoidArthritis <- list(conceptList = c(rep(4116447, times = 2), rep(1076675, times = 2)),
                            condition = "rheumatoid Arthritis",
                            excludedConditions = " - **Psoriatic Arthritis:** PsA, Psoriatic Joint Disease - **Ankylosing Spondylitis and Other Spondyloarthropathies:** AS, Axial Spondyloarthritis - **Reactive and Enteropathic Arthritis:** Reiter’s Syndrome, Arthritis Associated with IBD - **Systemic Lupus Erythematosus:** SLE, Lupus - **Systemic Sclerosis:** Scleroderma - **Sjögren’s Syndrome:** Dry Eye Syndrome - **Dermatomyositis/Polymyositis:** Inflammatory Myopathies - **Undifferentiated/Mixed Connective Tissue Disease:** UCTD, MCTD - **Gout:** Hyperuricemia, Crystal-Induced Arthritis - **Calcium Pyrophosphate Disease (Pseudogout/CPPD):** CPPD, Chondrocalcinosis - **Polymyalgia Rheumatica:** PMR - **Osteoarthritis:** OA, Degenerative Joint Disease - **Any Other Arthralgia or Unspecified Arthritis:** Joint Pain, Non-Specific Arthritis",
                            excludeCauses = F,
                            conceptSetName = "[LLM Test] rheumatoid Arthritis",
                            clinicalContext = "in the general population",
                            outputFolder = "p:/shared/llm/test",
                            tries = 1,
                            successes = 1)

systemicSclerosis <- list(conceptList = c(rep(4296514, times = 5), rep(40352976, times = 5)),
                          condition = "Systemic sclerosis",
                          excludedConditions = "- **Localized Scleroderma:** Morphea (generalized/plaque/guttate), linear scleroderma, en coup de sabre.
- **Mimics/Fibrosing Conditions:** Eosinophilic fasciitis, scleredema, scleromyxedema, nephrogenic systemic fibrosis.
- **Induced Scleroderma:** Drug-induced (e.g., bleomycin, taxanes) or environmental/occupational (e.g., silica, vinyl chloride, toxic oil).
- **Other/Overlap Conditions:** GVHD with sclerodermatous features, Mixed Connective Tissue Disease (MCTD), Undifferentiated Connective Tissue Disease (UCTD), isolated Raynaud’s phenomenon, acrosclerosis, or sclerodactyly.  ",
                          excludeCauses = F,
                          belowMinimumCountApproach = "TEST ALL",
                          minCount = 0,
                          conceptSetName = "[LLM Testing o3] Systemic sclerosis",
                          outputFolder = "p:/shared/llm/test",
                          additionalInformation = "",
                          clinicalContext = "in the general population",
                          tries = 1,
                          successes = 1)

posteriorUveitis <- list(conceptList = c(rep(4090114, times = 10)),
                         condition = "Posterior Uveitis",
                         excludedConditions = "- **Infectious posterior uveitides:** Toxoplasma posterior uveitis, herpes simplex virus (HSV), varicella-zoster virus (VZV), cytomegalovirus (CMV), tuberculosis, syphilis.
- **Intraocular lymphoma and other masquerade syndromes:** Primary vitreoretinal lymphoma, metastatic malignancies, and other non-inflammatory mimics.
- **Anterior uveitis, episcleritis/scleritis, and non-inflammatory mimics:** Conditions limited to the anterior segment or sclera without posterior segment involvement.
- **Organ-specific findings without explicit uveitis diagnosis:** Cystoid macular edema (CME), retinal vasculitis, or other isolated findings not associated with a uveitis diagnosis.  ",
                         excludeCauses = F,
                         belowMinimumCountApproach = "TEST ALL",
                         minCount = 0,
                         conceptSetName = "[LLM Testing o3] Posterior Uveitis",
                         outputFolder = "p:/shared/llm/llm testing o3/Posterior Uveitis",
                         additionalInformation = "",
                         clinicalContext = "in the general population",
                         tries = 1,
                         successes = 1)

conditionList <- list(stomachCancer, gastricCancer)
# conditionList <- list(gastricCancer)

for(conditionUp in 1:length(conditionList)) {
  finalSet <- executeLlmConceptCreate(conceptName = conditionList[[conditionUp]]$condition,
                                      originalConceptList = conditionList[[conditionUp]]$conceptList,
                                      excludedConditions = conditionList[[conditionUp]]$excludedConditions,
                                      tries = conditionList[[conditionUp]]$tries,
                                      successes = conditionList[[conditionUp]]$successes,
                                      database = database,
                                      vocabulary = database$vocabulary,
                                      minCount = 0,
                                      belowMinimumCountApproach = "TEST ALL",
                                      conceptSetName = conditionList[[conditionUp]]$conceptSetName,
                                      clinicalContext = conditionList[[conditionUp]]$clinicalContext,
                                      additionalInformation = conditionList[[conditionUp]]$additionalInformation,
                                      baseUrl = "https://epi.jnj.com:8443/WebAPI",
                                      postConceptSet = F,
                                      model = "o3", #"GPT-4o", #
                                      outputDirectory = "p:/shared/llm/test", #change to a directory on your machine
                                      quickRun = T) #set to T to NOT go through all the steps for an "official" run (i.e., for testing)
}
