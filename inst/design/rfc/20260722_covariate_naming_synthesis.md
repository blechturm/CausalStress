# RFC Synthesis: DGP Contract Terminology Clarifications

**Status:** ACCEPTED — maintainer accepted 2026-07-24 (Max Thomasberger), after
corrections required by the independent synthesis review. Binding at authority
level 4 within its stated scope. The Constitution v2.0.1 amendment it authorizes
is applied by a separate maintainer ratification action (see "Ratification
Status").
**Date:** 2026-07-24 (revised same day to incorporate review corrections)
**Synthesizer:** Claude
**Independent review:** Codex — `20260722_covariate_naming_synthesis_review.md`
(verdict: CONCUR WITH CORRECTIONS)
**Consolidates:** seed `20260722_covariate_naming_seed.md` (author: Codex) and
response `20260722_covariate_naming_response.md` (reviewer: Claude)
**Proposed constitutional version:** 2.0.1 (patch)

## Governance Note (read first)

This synthesis was drafted by the same agent that wrote the adversarial response
(Claude). To compensate for that role concentration, the seed author (Codex)
performed an independent review of the synthesis
(`20260722_covariate_naming_synthesis_review.md`). That review returned **CONCUR
WITH CORRECTIONS**, confirming the **patch** grade, the uppercase `X1...Xk`
decision, the synthetic-only scope, and the `real-data DGP` prose normalization,
while requiring three corrections. **All three MAJOR corrections and the applicable
MINOR corrections have been applied to this revision; the "Corrections Applied"
section records each one.** The maintainer then accepted this corrected synthesis.

Ratification of the Constitution itself remains a maintainer action; this document
authorizes it but does not perform it.

## Recommended Decision (accepted)

Ratify **Constitution v2.0.1** as a **patch**, comprising two independent,
narrowly-scoped corrections, and treat the covariate-name validator as a
**release-blocking v0.2.0 correction** (not an unspecified later follow-through).

1. **Adopt Option A** — amend Article III §3.2.A so synthetic-DGP covariates are
   `X1...Xk` (uppercase `X`, consecutive one-based integer suffixes), matching the
   immutable outputs of all 24 released synthetic DGP versions. The §3.2.A wording
   is the minimal form required by the review (no reserved-column vocabulary is
   constitutionalized).
2. **Normalize prose** — replace the human-facing term `Real DGP` with
   `real-data DGP` at the three live normative sites (§1.3, §1.7, §3.2.B). The
   machine discriminator `type = "real"` is unchanged; the real-data capability
   stays unimplemented and unauthorized.
3. **Reject Option B** (registry-declared arbitrary names) for this cycle and
   **reject Option C** (rename outputs to lowercase) permanently.
4. **Require the covariate-name validator as a v0.2.0 release blocker.** The
   Constitution rule ratifies now; a narrow v0.2.0 correction packet must land the
   enforcing validator and tests **before the public v0.2.0 tag** (Article VII
   requires continuous automated enforcement).
5. **Defer** the real-data capability, arbitrary synthetic feature names, and any
   estimator-roster redesign to the future real-data RFC / `horizon.md`.

## Verified Basis (independently sourced)

Fully verified from source by both the response and the independent review:

- All 24 registered synthetic versions across 12 DGP IDs emit uppercase,
  consecutive `X1...Xk` (explicit `X1=..` tibbles for 11 IDs; `colnames(X) <-
  paste0("X", seq_len(p_hd))` for `synth_hd_sparse_plm`, p ∈ {50,100}). Latent `Z`
  in kangschafer never enters `df`. The review additionally executed all 24
  generators and confirmed the covariate names at runtime.
- The Constitution text is the sole mismatch: §3.2.A says lowercase `x1...xk`
  (`CAUSAL_STRESS_CONSTITUTION.md:160`).
- No covariate-name validator exists in `cs_check_dgp_synthetic()`
  (`R/cs-contracts.R:518-676`), `cs_validate_dgp_registry()`, or
  `cs_validate_dgp()`; the review confirmed a lowercase-`x1` DGP currently passes
  `cs_check_dgp_synthetic()`.
- Estimator covariate discovery is heterogeneous: only `est-lm-att.R:43` and
  `est-ipw-att.R:42` use `grep("^X")`; `est-grf-dr`, `est-bart`, `est-gengc`,
  `est-gengc-dr`, `est-tmle` are name-agnostic.
- The change is text-only; it touches no generator/RNG/oracle/fingerprint code, so
  no released dataset, truth table, fingerprint, estimator result, or campaign
  artifact moves.
- Zero `type = "real"` rows exist; real-data support is entirely prospective.

Full file/line evidence is in the response and review documents.

## Decision Grade: Why Patch, Not Major

**Grade: patch v2.0.1** — confirmed by the independent review.

The change is graded patch on this reasoning:

- The **intended scientific contract** of §3.2.A — a synthetic DGP returns
  `y,w,p,y0,y1`, covariates indexed `1..k` under one canonical name, and
  `meta$structural_te` — is preserved. Only the surface notation of the canonical
  covariate name (case) changes, aligning the text with the immutable outputs of
  all 24 released versions.
- **No released behavior changes**: no generator, dataset, truth table, RNG
  stream, fingerprint, estimator result, or campaign artifact moves.
- The Preamble expressly permits patch revisions to "clarify intent, tighten
  definitions, or correct ambiguities" (`CAUSAL_STRESS_CONSTITUTION.md:15-17`);
  the explicit uppercase/consecutive/one-based statement is a bounded forward
  clarification of `X1...Xk`.

**The literalist counter-argument is real and is not dismissed.** `x1` and `X1`
are distinct case-sensitive identifiers, so the *formal set of column-name strings
the sentence admits does change*: a DGP returning lowercase `x1` satisfies the
current sentence and would not satisfy the amended one. For that reason this
synthesis does **not** claim "no semantic change" in the absolute; it claims the
**intended contract and all released behavior are unchanged**, which is the correct
patch justification.

The justification rests on the fully-verifiable consistency of all 24 immutable
released versions with uppercase naming (and the test suite and two estimators
that assume it), which identifies the lowercase clause as a **transcription
defect** rather than a ratified naming policy. The independent review additionally
reports git-history evidence that uppercase generator output predates the
introduction of the lowercase clause; this synthesis records that as corroboration
and rests its finding on the released-artifact consistency.

## Corrections Applied (per the independent review)

Each correction from `20260722_covariate_naming_synthesis_review.md` is applied
here:

- **MAJOR-1 (review) — do not invert the authority order.** The prior draft argued
  a lowercase-`x1` DGP "was never actually conformant" because shipped estimators
  and tests would reject it. That reasoning let code redefine constitutional
  conformity, which is backwards: the Constitution is supreme (`:19-20`), and under
  the literal v2.0.0 text the **code**, not the DGP, is the non-conforming side of
  the mismatch. **Removed.** The patch is now justified as correction of a
  transcription defect that preserves released behavior (see Decision Grade).
- **MAJOR-2 (review) — do not over-specify reserved names in §3.2.A.** The prior
  draft added "no gaps, no duplicates," reserved-column disjointness, and
  Runner-issued keys "such as `unit_id`." `unit_id` is a CATE held-out prediction
  key (`:142,150`), not part of the synthetic generation `df`; no authoritative
  reserved-column vocabulary exists; "consecutive" already entails no gaps.
  **Removed.** §3.2.A now uses the review's minimal wording; the operational
  core/non-feature list is defined by the correction packet, not the Constitution.
- **MAJOR-3 (review) — enforcement must block the v0.2.0 release.** The prior draft
  authorized the validator in an "unspecified later" packet. Article VII requires
  continuous automated enforcement (`:448`). **Corrected:** the validator and its
  tests are a **v0.2.0 release blocker** (see Implementation Follow-Through).
- **MINOR-1 (review) — regrade the estimator-heterogeneity finding.** The
  response's MAJOR-2 (heterogeneous discovery + future real-data brittleness) is
  factually correct but is **not** a defect in this amendment; it is regraded to a
  deferred compatibility NOTE routed to the real-data RFC, not a ratification
  condition.
- **MINOR-2 (review) — amendment-history wording.** The absolute "notation-only /
  no semantic change" phrasing is replaced by the review's shorter entry, which
  states the change preserves intended scientific meaning and released behavior
  without claiming the formal text set is unchanged.
- **MINOR-3 (review) — don't overstate the historical-edit prohibition.** The prior
  "forbidden" claim is softened: the v2.0.0 amendment entry and historical RFCs are
  preserved verbatim for audit integrity, without asserting a broader written ban
  than the governance index actually states.
- **NOTE-1 (review) — state Option B's real cost.** The rejection of Option B no
  longer says it costs "nothing scientific"; it records that relaxing the uppercase
  synthetic contract after v2.0.1 would require deliberate constitutional work,
  likely major-grade.

## Constitutional Text to Ratify (maintainer applies; not applied here)

Adopted verbatim from the independent review's minimal proposal.

**Article III §3.2.A — replace the covariate bullet:**

> - `df`: Tibble including `y`, `w`, `p`, `y0`, `y1`, and one or more covariates
>   named `X1`, ..., `Xk`, using uppercase `X` with consecutive one-based integer
>   suffixes.

**Articles I §1.3, I §1.7, III §3.2.B — prose only:**

- §1.3: use "real-data DGPs" in the Real Data clause (line 49).
- §1.7: rename the bullet to "Real-data DGPs" (line 90).
- §3.2.B: use "Real-data DGPs (`type="real"`)" (line 170).
- Do **not** change the machine discriminator `type = "real"`; do **not** touch the
  2.0.0 amendment-history entry (line 24) or any historical RFC.

**Amendment-history entry to add:**

> - **v2.0.1 (Ratified <maintainer stamps date>):** Patch clarification of the
>   DGP contract. Corrects Article III §3.2.A's synthetic-covariate notation from
>   lowercase `x1...xk` to uppercase, consecutive, one-based `X1...Xk`, matching the
>   immutable outputs of all 24 released package-managed synthetic DGP versions. The
>   correction preserves the intended scientific meaning and changes no released DGP
>   implementation, generated data, truth, RNG stream, fingerprint, estimator
>   result, or campaign evidence. It also normalizes the human-facing term
>   `Real DGP` to `real-data DGP` in the three live contract sites; the machine
>   discriminator `type = "real"` is unchanged and real-data support remains
>   deferred. Historical amendment and RFC records are preserved verbatim. The bump
>   is *patch*.

## Implementation Follow-Through (release-blocking v0.2.0 correction packet)

Implementation is outside this RFC document, but per the review it is a
**release-blocking item for the public v0.2.0 tag**, not an unspecified later
follow-through. A narrow v0.2.0 correction packet must:

- Define **one shared covariate-name predicate**: each covariate matches
  `^X[1-9][0-9]*$` and the suffixes are exactly `1:k` (consecutive, one-based),
  with `k >= 1`.
- Apply that single predicate across all three validation surfaces: internal
  contract validation (`cs_check_dgp_synthetic()`, `R/cs-contracts.R`), the
  all-version registry executable pass (`cs_validate_dgp_registry()`), and public
  DGP certification (`cs_validate_dgp()`).
- Define one authoritative internal core/truth/Runner field list for the validator
  to exclude when isolating covariates — **operational only**, not elevated into
  the Constitution.
- Add positive tests (all 24 registered generators pass) and negative tests
  (lowercase `x1`, gap `X1,X3`, bare `X` each abort with `causalstress_dgp_error`).
- Land **before** the public v0.2.0 tag; touch no generator, RNG context, oracle,
  fingerprint schema, or the `type = "real"` discriminator.

## Rejected Alternatives

- **Major bump for the casing fix** — rejected; intended contract and released
  behavior are unchanged (see Decision Grade). Confirmed by the review.
- **Option B (registry-declared arbitrary names)** — rejected this cycle; expands
  the registry/validation contract, and its real use case (real-data covariate
  names) is out of scope. **Cost recorded honestly:** relaxing the uppercase
  synthetic contract after v2.0.1 would require deliberate constitutional work,
  likely major-grade — not a routine API tweak. Revisit inside the real-data RFC.
- **Option C (rename outputs to lowercase)** — rejected permanently; violates
  Article II/VII immutability.
- **Constitutionalizing a reserved-column vocabulary in §3.2.A** — rejected per
  review MAJOR-2; that belongs to the operational validator.
- **Two separate RFC cycles** — rejected; both corrections are pure-notation,
  patch-level, and non-interacting. Coupled but separately enumerated.

## Open Risks and Deferred Questions (route to horizon.md)

- **Future synthetic-extension API.** Uppercase `X1...Xk` becomes a deliberate v2.x
  constraint for user-defined synthetic DGPs. If semantic feature names later prove
  important, relaxing it needs explicit constitutional reconsideration (likely
  major-grade), not a routine change.
- **`grep("^X")` estimators vs. real-data covariates.** `est-lm-att`,
  `est-ipw-att` abort when no `X`-prefixed column exists; future real-data DGPs
  (§3.2.B, no `X` naming) carry names like `age`. Prefer a Runner-supplied feature
  roster over either naming heuristic; resolve in the real-data RFC. **Must not
  enter this amendment.**
- **Implicit feature roster.** Prefix-based and exclusion-based estimators do not
  share one authoritative covariate roster; the correction packet defines an
  internal list for validation only.
- **Enforcement sequencing.** Ratification may precede implementation, but the
  validator and tests must land before v0.2.0 is tagged.

## Ratification Status

- **Synthesis:** ACCEPTED by the maintainer on 2026-07-24 (corrected per the
  independent review). Binding at authority level 4 within scope.
- **Constitution v2.0.1:** authorized, **application pending** — the maintainer
  applies the §3.2.A edit, the three prose normalizations, and the amendment-
  history entry above, and stamps the ratification date. Until then the
  Constitution remains v2.0.0 in fact.
- **Validator enforcement:** a v0.2.0 release blocker; ticket in the v0.2.0
  correction packet before the public tag.
- **No further RFC cycle required** (review recommendation).

---

*No Constitution, DGP, estimator, or test code is modified by this document.
Ratification-application and the validator packet are the authorized next actions.*
