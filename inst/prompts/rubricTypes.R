rubricListInput <- list()

rubricListInput[[1]] <- "RUBRIC — COMPREHENSIVE MULTI-DOMAIN CLAIMS LABEL VALIDATION

<RUBRIC_VARIATION_NAME>:

Comprehensive_MultiDomainClaimsValidation

<RUBRIC_ID>

R1

<RUBRIC_SPECIFICS>:

- This is the SINGLE comprehensive rubric that will be used for phenotype label validation.

- Because there is only ONE rubric, it must provide broad and detailed coverage of all clinically meaningful claims-based evidence that can support or contradict the phenotype.

- Generate SUBSTANTIALLY MORE rules than would be appropriate for one member of a multi-rubric ensemble.

- DO NOT limit the number of rules merely to keep the rubric concise.

- The rubric should optimize for high positive predictive value while still allowing multiple legitimate clinical pathways to YES.

- Borderline, contradictory, poorly corroborated, or clinically ambiguous cases should default to INSUFFICIENT EVIDENCE rather than YES.

COMPREHENSIVE EVIDENCE COVERAGE:

- Build extensive rules spanning ALL clinically applicable evidence categories:

  peri_index,
  post_index,
  treatment,
  utilization,
  complications.

- Include MULTIPLE independent rules within every clinically applicable category.

- Do not select only one or two representative signals from a category.

- Include all major claims-observable manifestations of the phenotype that are sufficiently specific to contribute meaningful validation evidence.

PERI-INDEX EVIDENCE:

- Create multiple rules representing clinically plausible management around the index event.

- Consider:
  acute medications,
  urgent treatment,
  procedures,
  emergency department care,
  inpatient care,
  specialist evaluation,
  early follow-up,
  and other phenotype-consistent management patterns.

- Explicitly model clinically meaningful combinations and temporal sequences where appropriate.

- For acute phenotypes, peri-index management should generally receive substantial weight.

- For chronic phenotypes, peri-index evidence may support validation but should not necessarily be required if a convincing longitudinal pattern exists.

POST-INDEX CONTINUITY:

- Create extensive rules representing disease-consistent continuity after index.

- Consider:
  repeat outpatient care,
  specialty care,
  repeat procedures,
  monitoring,
  medication persistence,
  refills,
  treatment modification,
  escalation,
  combination therapy,
  and complication management.

- Repeated events should be explicitly defined using numbers of occurrences, spacing requirements, and time windows.

- Chronic phenotypes should strongly reward persistent and temporally distributed evidence rather than isolated episodes.

TREATMENT AND MEDICATION TRAJECTORIES:

- Include all major phenotype-relevant medication and supply patterns that can be detected reliably in claims.

- Explicitly consider:
  treatment initiation,
  refill persistence,
  repeated fills,
  combination therapy,
  add-on therapy,
  treatment escalation,
  treatment switching,
  disease-specific supplies,
  and characteristic medication sequences.

- Distinguish highly phenotype-specific treatments from medications with many alternative indications.

- Highly disease-specific treatment patterns may receive strong weight.

- Nonspecific medications should receive weaker weight unless corroborated by other evidence.

- Medication evidence alone must NEVER establish YES.

UTILIZATION AND PROCEDURAL SIGNATURES:

- Include multiple rules for relevant:
  procedures,
  interventions,
  inpatient care,
  emergency department care,
  specialty care,
  repeated outpatient care,
  and characteristic utilization patterns.

- Distinguish phenotype-specific interventions from generic diagnostic procedures.

- Generic diagnostic testing should receive limited weight unless accompanied by disease-consistent downstream management.

- Utilization or procedure evidence alone must NEVER establish YES.

COMPLICATIONS AND DOWNSTREAM CONSEQUENCES:

- Include clinically plausible downstream complications when they provide useful evidence for the phenotype.

- Complication rules should generally require phenotype-consistent treatment, utilization, or management rather than relying only on the complication diagnosis.

- Explicitly consider competing etiologies because many complications are nonspecific.

- Give strong weight only when the complication and its management form a pattern that is meaningfully supportive of the phenotype.

MULTI-DOMAIN CORROBORATION:

- YES must require evidence from at least TWO DISTINCT evidence categories.

- Multiple rules from the SAME evidence category do NOT satisfy the multi-domain requirement.

- Prefer YES decisions supported by THREE or more independent signals when sufficient information exists.

- Do not allow a large number of weak rules within one category to compensate for lack of corroboration from another category.

- A medication alone, diagnosis pattern alone, procedure alone, utilization pattern alone, or complication alone must never establish YES.

STRONG-EVIDENCE REQUIREMENT:

- Whenever clinically feasible, YES should require at least ONE strong-tier evidence rule in addition to the requirement for evidence from at least two distinct categories.

- If the phenotype has no claims-observable signal appropriate for a strong-tier rule, keep the fixed YES threshold of 10 and require broader moderate evidence (more corroborating rules) to reach it.

- Numerous weak signals must not substitute for meaningful corroborating evidence.

TEMPORAL COHERENCE:

- Evaluate whether events form a clinically coherent temporal pattern rather than merely co-occurring somewhere in the observation window.

- Give additional evidentiary importance to plausible sequences such as:

  presentation -> evaluation -> treatment -> follow-up,

  treatment initiation -> continuation -> monitoring,

  hospitalization -> procedure -> post-discharge management,

  diagnosis pattern -> phenotype-specific treatment -> longitudinal follow-up.

- Avoid rewarding temporally disconnected events that are unlikely to represent one coherent disease course.

- Acute phenotypes should emphasize the expected peri-index sequence.

- Chronic phenotypes should emphasize persistence and continuity across the available follow-up period.

MULTIPLE PATHWAYS TO YES:

- Do NOT assume all true patients receive the same medication, procedure, specialty care, or site of care.

- Generate multiple legitimate pathways by which the phenotype could be supported.

- Include alternative accepted treatment and management patterns that are observable in administrative claims.

- Avoid making YES dependent on one specific medication, procedure, specialist, or utilization setting unless that element is genuinely necessary for the phenotype.

DIAGNOSIS CODES:

- Phenotype diagnosis codes are moderate evidence only.

- Phenotype diagnosis-code evidence appears only in the two fixed rules: ER1 (>=1 diagnosis within -5 to +5 days of index, 3 points) and ER2 (the supplied DX criteria, at the points given in the main prompt). Do not create any other phenotype diagnosis-code evidence rule; number other evidence rules from ER3.

- Diagnosis-code evidence must never independently establish YES.

- Diagnosis codes should primarily support other claims evidence rather than dominate scoring.

ALTERNATIVE EXPLANATIONS:

- Generate an EXTENSIVE and COMPREHENSIVE collection of alternative-explanation rules.

- Do not limit competing explanations to only the two or three most common differential diagnoses.

- Consider all major claims-observable alternatives that could plausibly explain the evidence used to validate the phenotype.

- Include:
  differential diagnoses,
  competing etiologies,
  alternative medication indications,
  alternative procedure indications,
  acute mimics,
  transient conditions,
  rule-out evaluation patterns,
  competing chronic diseases,
  unrelated major utilization events,
  and alternative causes of downstream complications.

- When clinically appropriate, create separate rules for distinct differential diagnoses instead of combining many unrelated conditions into one rule.

- A competing diagnosis should receive a major penalty only when the record contains a coherent competing pattern.

- An isolated competing diagnosis code should generally NOT be sufficient to negate otherwise strong phenotype evidence.

- Strong competing patterns should receive substantial negative points and/or cap the maximum decision at INSUFFICIENT.

- Particularly compelling competing explanations may cap the decision at NO when clinically justified.

HARD EXCLUSIONS:

- Hard exclusions should be relatively uncommon.

- Reserve them for situations where available claims evidence strongly indicates:
  the assigned phenotype is incompatible with the observed clinical context,
  another circumstance clearly invalidates the label,
  or reliable validation is impossible.

- Do not create hard exclusions merely because another diagnosis is possible.

- If uncertainty remains between the phenotype and an alternative explanation, generally use an alternative-explanation penalty or cap rather than a hard exclusion.

RULE GRANULARITY:

- Prefer MANY simple, explicit, independently evaluable rules over a small number of highly complex rules containing many unrelated OR conditions.

- Each rule should represent one clinically meaningful claims pattern.

- Do not combine unrelated treatments, procedures, differential diagnoses, or complications merely to reduce rule count.

- Do not create redundant rules that represent essentially the same clinical signal.

DOUBLE-COUNTING:

- Avoid creating multiple rules that routinely trigger from the exact same clinical event.

- Do not artificially inflate scores by representing the same evidence several different ways.

- If two rules are related, their criteria should be sufficiently distinct that satisfying both represents genuinely additional evidence.

- Multiple administrative claims arising from one clinical encounter should not automatically count as independent clinical signals unless explicitly and clinically justified.

POINT VALUES:

- Assign point values according to the strength, specificity, and independence of each claims pattern.

- In general:
  strong evidence = highest positive points,
  moderate evidence = intermediate positive points,
  weak evidence = small positive points.

- Highly phenotype-specific treatment/procedure patterns should generally receive the greatest weight.

- Common medications, generic testing, isolated diagnoses, and nonspecific utilization should receive lower weight.

- Do not allow accumulation of numerous weak rules to overwhelm the absence of convincing strong or moderate corroboration.

NEGATIVE POINT VALUES:

- Alternative-explanation penalties should reflect how strongly the competing pattern decreases the probability that the assigned phenotype explains the record.

- Highly specific competing patterns should receive large negative point values.

- Moderately plausible competing explanations should receive smaller penalties.

- Use decision caps when a competing explanation creates substantial unresolved ambiguity even when the numerical score remains high.

DECISION THRESHOLDS:

- The thresholds are fixed: YES = 10 points, NO = 2 points. Normalize all other rule points to this scale.

- YES should require:
  1) achievement of the YES score threshold,
  2) evidence from at least TWO distinct categories,
  3) satisfaction of any strong-evidence requirement,
  4) no applicable hard exclusion,
  5) no alternative-explanation cap preventing YES.

- NO should represent records with convincing evidence against the assigned label or compelling alternative explanations when supported by the rubric.

- INSUFFICIENT EVIDENCE should be used when:
  evidence is sparse,
  evidence is contradictory,
  required cross-category corroboration is absent,
  evidence is predominantly weak,
  competing explanations remain unresolved,
  or the score falls between the YES and NO thresholds.

- Favor INSUFFICIENT EVIDENCE over forcing either YES or NO when claims data cannot reliably distinguish between plausible explanations.

AUTOMATED IMPLEMENTATION:

- Every rule must be sufficiently explicit to translate directly into deterministic R logic.

- Avoid vague terms such as:
  frequent,
  recurrent,
  persistent,
  shortly after,
  around index,
  prolonged,
  substantial,
  or repeated

  unless the rule provides an explicit numeric definition.

- Explicitly specify whenever applicable:
  number of occurrences,
  date ranges,
  minimum spacing,
  maximum spacing,
  presence requirements,
  absence requirements,
  temporal relationship between events,
  visit setting,
  and required corroborating events.

OVERALL OBJECTIVE:

- Create one comprehensive, clinically coherent, conservative claims-validation rubric capable of independently adjudicating the phenotype.

- The rubric should capture the useful information previously obtained by using multiple specialized rubric variations within a SINGLE unified scoring system.

- The rubric should provide broad sensitivity to legitimate clinical pathways while maintaining strict requirements for cross-domain corroboration and protection against false-positive validation.

- Generate enough independently evaluable rules to represent the full range of meaningful positive evidence, negative evidence, alternative explanations, and exclusions observable using the available claims data.
"
