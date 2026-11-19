# Ithaca vNext Revision Specification

## Purpose

This document defines the target-state rewrite specification for the next major Ithaca revision.
It captures requirements confirmed to date, architecture direction, non-goals, and migration/rollout expectations.

This is intentionally implementation-oriented so future chat sessions and contributors can execute against a stable contract.

## Scope and intent

- Full rewrite of workflow internals around a DuckDB-centered architecture.
- Preserve user-facing workflow outcomes where possible (rubric creation, concept set generation, scoring outputs).
- Improve reproducibility, auditability, and operational resilience.
- Keep design compatible with current OHDSI/CohortGenerator constraints used in this repository.

## Confirmed requirements (locked)

1. Canonical concept-set storage is DuckDB:
   - `P:/llm/rubrics/rubricConceptSetLibrary.duckdb`
2. Big-bang switch for concept-set library backend (no RDS dual-support period).
3. Scoring uses **all current sets** (not approval-gated).
4. Provenance fields are required on concept-set writes:
   - `runId`, `user`, `timestamp`, `condition`, `cohort`, `sourceFile`
5. Promotion model (`dev -> shared -> prod`) is deferred to a later phase.
6. Every scoring run must pin an explicit concept library snapshot/version.
7. Write model is single-user for now (multi-writer support deferred).
8. Concept matching must support ambiguity-gated LLM adjudication over top candidates.

## Current-state baseline (as of this spec)

- Concept-set library definitions and embeddings are stored in DuckDB with revision history.
- Scoring requires `conceptLibraryVersion` pinning.
- LLM adjudication for ambiguous concept matching is implemented with:
  - feature flag,
  - ambiguity trigger (gray zone / top-2 gap),
  - confidence gating,
  - fallback to similarity-only when unavailable.
- Adjudication prompt is externalized:
  - `inst/prompts/conceptSetMatchAdjudication.txt`

## Target architecture (vNext rewrite)

### 1) Storage model

DuckDB should be the system of record for:

- Concept set definitions
- Embeddings
- Revision history / snapshot lineage
- Match/adjudication diagnostics
- Run-level provenance metadata

### 2) Read model

Scoring and matching should resolve concept sets by:

- Explicit `libraryVersion` pin,
- Deterministic snapshot read semantics,
- No implicit “latest” in production scoring paths.

### 3) Write model

- Immediate write after each concept-set creation/update event.
- Transactional per-unit updates.
- Clear error behavior and rollback on failure.
- Single-writer assumptions documented and enforced.

### 4) Matching model

Two-stage retrieval + adjudication:

1. Candidate retrieval by embedding/fuzzy similarity.
2. LLM final adjudication only under ambiguity triggers.

Fallback rules:

- If LLM unavailable/errors/low-confidence -> revert to deterministic similarity path.

### 5) Diagnostics and traceability

Every match decision should be attributable with:

- candidate set and scores,
- trigger reason (why LLM invoked),
- LLM decision/confidence/rationale,
- final decision method (exact, embedding, fuzzy, adjudicated, none/fallback),
- run metadata.

## Rewrite requirements by module

### Concept library module

- Provide explicit APIs for:
  - initialize/create library,
  - migrate legacy inputs,
  - upsert concept set,
  - load snapshot by version,
  - retrieve latest version metadata.
- Enforce schema migration/versioning strategy.

### Matching module

- Decouple candidate generation from adjudication.
- Centralize threshold policy and defaults.
- Emit machine-readable diagnostics for each attempted match.

### Workflow module

- Parameterize all adjudication controls from top-level workflow entrypoints.
- Keep defaults safe and explicit.
- Ensure workflow logs record pinned `conceptLibraryVersion`.

### Scoring module

- Require pinned concept-library snapshot.
- Fail fast with clear error if snapshot/version missing.

## Changes required in new version that are NOT fully covered by current version

This section highlights required vNext outcomes that go beyond the present implementation.

1. **Replace per-condition match file artifacts with DuckDB-native match audit tables**
   - Current: `matchedConceptSets.rds` plus diagnostics CSV artifacts.
   - vNext: write match decisions and diagnostics to DuckDB tables as system-of-record.
   - Optional artifact export can remain for convenience, but DB is canonical.

2. **Canonical run ledger in DuckDB**
   - Current: provenance captured primarily during concept-set upserts.
   - vNext: all workflow phases (rubric gen, matching, concept-set creation, scoring) should write run events into a common run ledger schema.

3. **Formal schema migration framework**
   - Current: schema is created/used directly.
   - vNext: declarative schema version + migration steps with forward compatibility checks.

4. **Deterministic replay support**
   - Current: partial reproducibility via `conceptLibraryVersion` pinning.
   - vNext: replay manifest should include library version, thresholds, adjudication settings, and model identifiers.

5. **First-class query/report helpers over DuckDB audit data**
   - Current: diagnostics are primarily file outputs.
   - vNext: provide package functions to query decisions, confidence distributions, fallback rates, and unresolved/missing sets directly from DB.

## Non-goals for the first vNext cut

- Multi-writer concurrent mutation support.
- Approval-gated concept-set lifecycle enforcement.
- Environment promotion automation.

## Data model expectations (high level)

Minimum entities:

- `conceptSetRevision`
- `conceptSetCurrent`
- `libraryVersionHistory`
- `matchDecisionAudit` (new canonical replacement for matchedConceptSets file flow)
- `runAuditLedger` (cross-phase run events)

## Compatibility and migration

- Big-bang cutover is acceptable for concept-library backend.
- One-time migration utilities must be available and documented.
- Post-cutover validation should include:
  - concept counts,
  - name uniqueness expectations,
  - score/match parity checks on selected retrospective runs.

## Operational policies

- Scoring must require explicit `conceptLibraryVersion`.
- LLM adjudication should be configurable and can be disabled.
- Fallback behavior must be deterministic and auditable.

## Recommended phased execution for rewrite

1. Formalize target schemas and migration framework.
2. Move match artifacts (`matchedConceptSets`) into DuckDB audit tables.
3. Add run ledger across major workflow stages.
4. Add DB-native diagnostics query/report functions.
5. Remove or de-emphasize file artifacts where redundant.

## Acceptance criteria for vNext

- End-to-end workflow runs successfully using DuckDB as canonical storage.
- Scoring reproducibility is guaranteed via pinned `libraryVersion`.
- Match decisions are auditable in DuckDB without requiring file artifact inspection.
- Legacy parity checks are documented and pass agreed thresholds.

## Additional recommendations (not yet required)

These are high-value enhancements to consider after core vNext requirements are complete.

1. **Golden-run regression harness** (`Priority: P1`, `Effort: M`)
   - Maintain fixed reference conditions/cohorts with expected outputs.
   - Run on every substantial change to quantify parity/delta vs baseline.

2. **Idempotent run + resume model** (`Priority: P1`, `Effort: L`)
   - Introduce explicit run-state transitions and resumable checkpoints.
   - Ensure retries/restarts do not duplicate writes or produce divergent outcomes.

3. **Embedding/adjudication cache policy** (`Priority: P2`, `Effort: M`)
   - Define deterministic cache keys (name/model/prompt hash/library version context).
   - Add cache invalidation/versioning rules to avoid stale-match behavior.

4. **Model-governance metadata** (`Priority: P1`, `Effort: S`)
   - Persist model identifiers, prompt hash/version, and adjudication policy values per decision.
   - Support explainability and forensic replay.

5. **Automated quality gates** (`Priority: P1`, `Effort: M`)
   - Add configurable run thresholds for:
     - fallback rate,
     - no-match rate,
     - low-confidence adjudication rate,
     - unresolved concept-set rate.
   - Fail or warn when quality falls outside policy.

6. **Version-diff reporting utilities** (`Priority: P2`, `Effort: M`)
   - Provide package-level reports for “what changed” across library versions:
     - concept-set JSON changes,
     - embedding/match decision shifts,
     - unresolved/newly created concept sets.

7. **PII-safe diagnostics mode** (`Priority: P3`, `Effort: S`)
   - Add optional redaction policy for text fields and paths in diagnostics/logs while preserving debuggability.

8. **Public API stability policy** (`Priority: P2`, `Effort: S`)
   - Define semantic versioning expectations for top-level callable functions.
   - Add deprecation windows and compatibility checks for entrypoints.

## Document governance

- Owner: Ithaca maintainers.
- Update this document whenever user-facing workflow, audit strategy, or storage contracts change.
- Keep `README.md` and `REFACTORING.md` consistent with any finalized decisions here.
