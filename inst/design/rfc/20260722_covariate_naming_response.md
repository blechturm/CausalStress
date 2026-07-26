# RFC Response: DGP Contract Terminology Clarifications

**Status:** RESPONSE — non-binding
**Date:** 2026-07-24
**Reviewer:** Claude
**Responds to:** `20260722_covariate_naming_seed.md`

## Executive Verdict

**ACCEPT WITH AMENDMENTS.**

The seed's central factual claim is correct and independently verified: all 24
registered synthetic DGP versions across the 12 package-managed DGP IDs emit
uppercase, consecutively numbered `X1...Xk` covariates, and no released artifact
would change. Option A (ratify uppercase `X1...Xk`) and the `Real DGP` →
`real-data DGP` normalization are both sound and are correctly patch-level, and
exact 2.0.1 wording is provided below.

The verdict is *with amendments*, not a clean ACCEPT, because the seed frames the
change too loosely in three ways that must be corrected before synthesis:

1. It calls the edit a "clarification" of an "ambiguity." It is not an ambiguity;
   the current text is **wrong** — it literally requires `x1` while every
   immutable released DGP emits `X1`, and two shipped estimators plus the test
   suite actively depend on the uppercase form. The amendment record must say it
   corrects a text-vs-immutable-reality inconsistency, which is still
   patch-permissible.
2. Option A silently adds a **new normative constraint** — "consecutive one-based
   integer suffixes" — that is absent from the current text. This is a forward
   definitional *tightening*, not a casing fix. Patch-level tightening is
   explicitly allowed by the Preamble, but it must be recorded as a tightening,
   not smuggled in under "notation."
3. The seed overstates uppercase `X*` as "the estimator-facing convention." It is
   **one of two** discovery conventions in the codebase; the majority of
   estimators are covariate-name-agnostic. This matters for the future real-data
   capability and must be recorded as a deferred risk.

None of these is a hard blocker; all are resolvable in the amendment framing and
the follow-through packet. The recommendation is to ratify Constitution 2.0.1 as
a patch, using the corrected wording in this response.

## Verified Baseline Facts

Every claim below was checked against source, not accepted from the seed.

- **12 DGP IDs, 24 versions, all `type = "synthetic"`.**
  `cs_dgp_registry()` has exactly 24 rows across 12 distinct `dgp_id`s, with
  `type = rep("synthetic", 24)`
  ([R/cs-dgp-registry.R:36](../../../R/cs-dgp-registry.R)). **Zero** rows are
  `type = "real"`. The real-data DGP is entirely prospective.

- **All 24 versions emit uppercase, consecutive `X1...Xk`.** Eleven DGP IDs build
  the covariate columns by explicit named assignment `X1 = X1, X2 = X2, ...`
  inside `tibble::tibble(...)`; verified in `dgp-synth-baseline.R:70-74,145-149`,
  `dgp-synth-heavytail.R:86-90,174-178`, `dgp-synth-qte1.R:70-74,146-150`,
  `dgp-synth-nonlinear-heteroskedastic.R:77-80,158-161,239-242,317-320` (four
  versions, `X1..X4`), `dgp-synth-overlap-stressed.R:72-76,155-159,237-241`,
  `dgp-synth-tilt-mild.R:67-71,144-148`, `dgp-synth-placebo-nonlinear.R:55-59`,
  `dgp-synth-placebo-heavytail.R:61-65`, `dgp-synth-placebo-tilted.R:55-59,122-126`,
  `dgp-synth-placebo-tau0.R:61-65`, and `dgp-synth-placebo-kangschafer.R:77-80`
  (`X1..X4`). The twelfth ID, `synth_hd_sparse_plm`, names its matrix columns via
  `colnames(X) <- paste0("X", seq_len(p_hd))` for all three versions
  (`dgp-synth-hd-sparse-plm.R:43,120,194`) with `p_hd ∈ {50, 100}`, then
  `dplyr::bind_cols(df, tibble::as_tibble(X))` (`:75`), yielding consecutive
  `X1...X50` / `X1...X100`. **Seed claim #1 is TRUE.**

- **No latent or non-`X` covariate leaks into `df`.** In
  `synth_placebo_kangschafer` the latent Gaussians `Z1..Z4` are used only to
  construct the observed nonlinear transforms; the returned `df` contains
  `X1..X4` only (`dgp-synth-placebo-kangschafer.R:70-81`). No generator emits a
  lowercase `x`, a gap, a duplicate, or a bare `X`.

- **The test suite already locks in uppercase, consecutive naming.**
  `test-dgp-baseline.R:15` asserts `X1..X5`; `test-dgp-hd-sparse-plm.R:8` asserts
  `paste0("X", 1:100)`; `test-dgp-heavytail.R:14` asserts `X1..X5`;
  `test-dgp-nonlinear-heteroskedastic.R:20` and `test-dgp-stress-v140.R:5,30` read
  `df$X1`, `df$X2`, `df$X4`. The implemented-and-tested reality is uppercase.

- **The Constitution text is lowercase.** Article III §3.2.A reads
  "covariates named `x1...xk`"
  ([CAUSAL_STRESS_CONSTITUTION.md:160](../CAUSAL_STRESS_CONSTITUTION.md)). This is
  the sole normative source of the mismatch. **The released DGPs are not merely
  "formally non-compliant" (seed's framing); the text is contradicted by shipped
  code and tests.**

- **Estimator covariate discovery is heterogeneous — not a single `X*`
  convention.**
  - `grep("^X", names(df))`, aborting if none found: `est-lm-att.R:43-46` and
    `est-ipw-att.R:42-45`. These two **require** the uppercase `X` prefix.
  - `setdiff(names, core)`, name-agnostic: `est-grf-dr.R:55` (core =
    `y,w,y0,y1,p,structural_te`), `est-bart.R:92`, `est-gengc.R:44`,
    `est-gengc-dr.R:53`, `est-tmle.R:55,107` (all `setdiff(names, c("y","w"))`).
    These accept **any** covariate names.
  - `est-oracle-att.R:25` discovers nothing by name; it consumes `structural_te`.

  So **seed claim "several estimators discover covariates using `X*`" is TRUE but
  materially incomplete**: exactly two of the built-in estimators depend on the
  uppercase prefix; the rest are agnostic. Note also that `grep("^X")` requires
  neither consecutiveness nor one-based numbering — it matches `X1`, `X10`, and
  even a bare `X` or `Xfoo` — so no estimator actually needs the "consecutive
  one-based" property the seed proposes to mandate.

- **No validator enforces covariate naming anywhere today.**
  `cs_check_dgp_synthetic()` checks `y,w,y0,y1,p`, `true_att`, `true_qst`, `meta`,
  and `structural_te`, but performs **no covariate-name check**
  ([R/cs-contracts.R:518-676](../../../R/cs-contracts.R)).
  `cs_validate_dgp_registry()` executes each generator and checks for
  `y0,y1,p,structural_te` only ([R/cs-validate-registry.R:160-174]).
  `cs_validate_dgp()` checks `y,w`, potential outcomes, and determinism, never
  covariate names ([R/cs-validate-dgp.R:25-50]). "Validators should enforce the
  canonical names" therefore describes **net-new work**, not a tightening of
  existing validation.

- **The machine discriminator is the literal string `"real"`, independent of
  prose.** `allowed_types <- c("synthetic", "real")`
  ([R/cs-validate-registry.R:27]); Constitution §3.2.B keys on
  `type = "real"` ([:170]). The human-facing phrase can change without touching
  this literal. **Seed claim #8 is TRUE.**

- **Text-only change ⇒ no runtime artifact moves.** The amendment edits prose in
  the Constitution and touches no generator, RNG context, oracle, or fingerprint
  code. Because the released DGPs already emit uppercase `X`, every byte-identical
  dataset, truth table, and fingerprint remains byte-identical by construction.
  **Seed claims #3 and #5 are TRUE.**

- **The registry carries no covariate-name declaration** (Option B premise).
  `cs_dgp_registry()` columns are `dgp_id, type, generator, version, description,
  status, rationale, date_status_changed, design_spec, tags` — no covariate-name
  field. **Seed's Option B cost claim is TRUE.**

## Findings

### MAJOR-1 — "Clarification" mislabels a correction-plus-tightening

- **Problem:** The seed calls the edit "a patch-level constitutional
  clarification" that "corrects the lowercase `x1...xk` spelling." Two things are
  understated. (a) The current text is not ambiguous, it is *inconsistent with
  immutable reality*: `x1` and `X1` are distinct case-sensitive R identifiers, and
  every released DGP emits `X1`. (b) Option A's parenthetical "consecutive
  one-based integer suffixes" is a **new constraint** not present in today's
  `x1...xk`.
- **Evidence:** Current text `x1...xk` at
  [CAUSAL_STRESS_CONSTITUTION.md:160]; all 24 generators emit `X1..Xk`
  (baseline references above); Preamble
  [:15-17] permits patch revisions to "clarify intent, tighten definitions, or
  correct ambiguities, but MUST NOT change the semantic meaning of any article
  without a major version bump."
- **Why it matters:** If synthesis adopts the seed verbatim, it records a
  substantive forward tightening under a cosmetic label, weakening the amendment
  audit trail. The tightening is *satisfied by every released version*, so it
  changes no released semantics and remains within the Preamble's "tighten
  definitions" patch allowance — but only if recorded as such.
- **Correction:** Keep it patch-level, but the amendment-history entry must (a)
  state it corrects a text-vs-immutable-reality inconsistency, and (b) name the
  consecutive/one-based/no-gap/no-duplicate requirement as a deliberate forward
  definitional tightening, satisfied by all 24 released versions. Wording provided
  below.

### MAJOR-2 — "Estimator-facing convention" is overstated; `X*` discovery is not universal

- **Problem:** The seed asserts uppercase `X*` is "the existing estimator-facing
  convention," implying it is load-bearing across estimators. It is used by two
  estimators; five others are name-agnostic.
- **Evidence:** `grep("^X")` at `est-lm-att.R:43`, `est-ipw-att.R:42` vs.
  `setdiff(names, core)` at `est-grf-dr.R:55`, `est-bart.R:92`, `est-gengc.R:44`,
  `est-gengc-dr.R:53`, `est-tmle.R:55,107`.
- **Why it matters:** The overstatement is favorable to the seed's conclusion but
  obscures a real deferred risk: the two `grep("^X")` estimators **abort** with
  "No covariate columns starting with 'X'" when handed a DGP whose covariates are
  not `X`-prefixed. Future real-data DGPs (§3.2.B, which imposes *no* `X` naming on
  real covariates) will carry empirical names like `age`, `income` — and will
  break `est-lm-att` / `est-ipw-att`. Mandating `X1...Xk` for the **synthetic**
  contract only (as Option A does) contains this for now, but the estimator
  brittleness must be recorded, not erased.
- **Correction:** The synthesis must scope the `X1...Xk` mandate to synthetic
  DGPs only (Option A already edits §3.2.A only — good) and log the
  `grep("^X")`-estimator incompatibility with real-data covariates in
  `horizon.md`/Open Risks.

### MAJOR-3 — The ratified rule would have zero enforcement

- **Problem:** Option A says "validators should enforce the canonical names and
  consecutive ordering," but no such validator exists in any of the three
  validation surfaces.
- **Evidence:** `cs_check_dgp_synthetic()` has no covariate-name check
  ([R/cs-contracts.R:538-676]); `cs_validate_dgp_registry()` checks only
  `y0,y1,p,structural_te` ([R/cs-validate-registry.R:168-171]);
  `cs_validate_dgp()` checks `y,w` and potential outcomes only
  ([R/cs-validate-dgp.R:25-50]).
- **Why it matters:** Ratifying a constitutional covariate-naming rule with no
  enforcement leaves the contract aspirational and lets a future non-compliant
  DGP through silently. The `contracts.md` Validation Contract already mandates
  "version-aware executable validation" of every `(dgp_id, version)` generator —
  the natural home for the check.
- **Correction:** The follow-through packet must add a covariate-name validator
  to `cs_check_dgp_synthetic()` (and mirror it in the registry executable pass
  and `cs_validate_dgp()`), with contract tests. Do not implement in this cycle;
  authorize it. Details under Implementation Follow-Through.

### MINOR-1 — Terminology normalization touches more sites than the seed implies

- **Problem:** The seed says "normalize the active Constitution's term `Real DGP`"
  (singular). The term appears at three live normative sites, plus one historical
  record that must **not** be reworded.
- **Evidence:** Live: §1.3 [:49] ("For Real DGPs"), §1.7 [:90] ("Real DGPs:"),
  §3.2.B [:170] ("B. Real DGPs (`type="real"`)"). Historical: the 2.0.0
  amendment-history entry [:24] ("the real-DGP external-truth clause (§1.3)").
- **Why it matters:** A precise edit list prevents a partial rename that leaves
  the Constitution internally inconsistent, and prevents retroactive editing of a
  historical amendment record (forbidden by the README's "do not silently edit
  old scientific history" principle).
- **Correction:** Normalize the three live sites only; leave [:24] verbatim.

### MINOR-2 — `grep("^X")` is a fragile discriminator; the validator must be stricter than the estimators

- **Problem:** `grep("^X")` matches any uppercase-`X`-prefixed name, including a
  bare `X`, `Xfoo`, or a hypothetical reserved `X`-prefixed column, and does not
  detect gaps or duplicates.
- **Evidence:** `est-lm-att.R:43`, `est-ipw-att.R:42`. Reserved columns today are
  `y,w,p,y0,y1,structural_te` (none `X`-prefixed), plus Wave 2 `unit_id`, and
  `tau`/`tau_id` on truth tables (`cs-contracts.R`).
- **Why it matters:** Point #6 asks for "separation from reserved columns." The
  validator should enforce the exact set `{X1..Xk}` (regex `^X[1-9][0-9]*$`, set
  equal to `X1..Xk`, disjoint from the reserved set) — stricter than any
  estimator's own discovery — so that the constitutional guarantee is real rather
  than "whatever `grep` happened to match."
- **Correction:** Specify the exact predicate in the follow-through packet (below).

### NOTE-1 — The mismatch strengthens, not weakens, the seed

The current lowercase text is contradicted by both shipped estimator code
(`grep("^X")`) and the test suite (`test-dgp-baseline.R:15` etc.). A DGP that
literally complied with the current `x1...xk` text would **fail** `est-lm-att`
and `est-ipw-att` at runtime. This is affirmative evidence that the ratified
intent in v2.0.0 was uppercase and the lowercase text is a transcription defect —
supporting Option A and reinforcing the "correct an inconsistency" framing.

### NOTE-2 — No `type = "real"` DGP exists

All 24 registry rows are synthetic. The terminology change has no runtime effect
and cannot affect any real-data code path, because none exists. This is why the
prose/discriminator separation (points #7–#8) is safe today and why the real-data
capability remains a clean deferral.

## Option Assessment

**Option A — Ratify uppercase `X1...Xk` (recommended): ACCEPT.**
Matches all 24 released versions and the test suite; makes the two `grep("^X")`
estimators sound against the synthetic contract; changes no released artifact.
The only defect is presentation (MAJOR-1): the "consecutive one-based" clause is a
forward tightening and must be labeled as one. Scoping the mandate to §3.2.A
(synthetic) only is correct — real covariates (§3.2.B) are intentionally left
unconstrained.

*Future user-defined synthetic DGP implication.* DGP authorship is
maintainer-gated (§1.2 "Only the core maintainers may define new DGP IDs";
community contributions must pass constitutional validation), so the lock-in falls
on maintainer-authored synthetic DGPs, not arbitrary end users. For synthetic
covariates — which are generated and semantically arbitrary — a single canonical
`X1...Xk` convention is a reasonable, low-cost constraint. The API-lock-in concern
(point #3) is real but bounded and lands in the right place: it does not touch the
real-data contract, where semantic names will matter.

**Option B — Arbitrary registry-declared covariate names: REJECT for this cycle,
DEFER for real-data.**
The seed is correct that the registry carries no covariate-name declaration and
that adopting one expands the registry and validation contract, and that two
estimators assume `X*` (verified above). For the **synthetic** contract, Option B
buys flexibility that nothing needs: synthetic covariates are arbitrary draws, so
naming them `X1..Xk` costs nothing scientific. Option B's genuine use case is the
**real-data** capability, where covariates arrive with empirical names — and that
is explicitly out of scope. Reject Option B now; record it as the design to
revisit inside the future real-data RFC, coupled with fixing the `grep("^X")`
estimators.

**Option C — Rename released DGP outputs to lowercase: REJECT.**
Correct as written. Released `(dgp_id, version)` outputs are immutable (Article II
§2.1, Article VII §7.2); a case-only rename would either mutate frozen outputs
(forbidden) or spawn new versions with identical science, fragmenting evidence
lineage for zero benefit. No further analysis needed.

## Real-Data DGP Terminology

- **Prose term.** "real-data DGP" is the better human-facing term. "Real DGP"
  invites the misreading that synthetic DGPs are somehow "unreal" DGPs, when the
  intended contrast is *generated data* vs. an *empirical-data benchmark*.
  "real-data DGP" names that contrast correctly. **Point #7: agree.**
- **Machine discriminator.** The prose can change without touching
  `type = "real"`: the discriminator is a literal string in the registry schema
  (`allowed_types` at [R/cs-validate-registry.R:27]) and the §3.2.B contract key,
  independent of any surrounding prose. Do **not** change the literal. **Point #8:
  agree — decouple prose from discriminator explicitly in the amendment record.**
- **Deferred capability.** Support for real-data DGPs stays unimplemented and
  unauthorized. The amendment is terminology only. Confirmed by the registry
  containing zero `type = "real"` rows and by §1.7's existing deferral of a
  generalized external-truth tier.
- **Historical RFCs and records.** Leave untouched. The 2.0.0 amendment-history
  entry ([:24], "real-DGP external-truth clause") and the accepted RFC-1 files are
  historical records; retroactively rewording them would violate the README's
  prohibition on editing old scientific history and would corrupt the audit trail.
  The new 2.0.1 entry documents the going-forward normalization; the past stays
  verbatim.

## Coupling vs. Separation (Point #9)

Keep the covariate correction and the terminology normalization **coupled in a
single 2.0.1 patch, but enumerated as two independent items** in the
amendment-history entry. Rationale: both are pure-notation, neither changes any
runtime artifact, and both are patch-level, so one ratification event is
efficient. Enumerating them separately preserves independent auditability and
reversibility. The one hard rule: the amendment entry must not let the terminology
change borrow the covariate change's "matches immutable outputs" justification, or
vice versa — they are justified independently (one by released DGP outputs, the
other by prose disambiguation of a not-yet-implemented capability).

## Recommended Constitutional Text

*Provided for the maintainer; this response does not edit the Constitution.*

**Article III §3.2.A — replace the covariate bullet:**

> - `df`: Tibble including `y`, `w`, `p`, `y0`, `y1`, and covariates named `X1`,
>   ..., `Xk` — uppercase `X` with consecutive one-based integer suffixes (no
>   gaps, no duplicates), disjoint from the reserved columns
>   (`y`, `w`, `p`, `y0`, `y1`, `structural_te`, and Runner-issued keys such as
>   `unit_id`).

**Article I §1.3, Article I §1.7, Article III §3.2.B — normalize prose only:**
replace the human-facing phrase "Real DGP(s)" with "real-data DGP(s)" at the three
live sites (lines 49, 90, 170). Do **not** alter the machine value
`type = "real"`. Do **not** alter the 2.0.0 amendment-history entry (line 24).

**Amendment-history entry to add (patch):**

> - **v2.0.1 (Ratified <maintainer stamps date>):** *Patch — notation and
>   terminology corrections; no semantic change and no released-artifact change.*
>   (1) Corrects Article III §3.2.A synthetic-DGP covariate notation from lowercase
>   `x1...xk` to uppercase `X1...Xk`, aligning the constitutional text with the
>   immutable outputs of all 24 released package-managed synthetic DGP versions and
>   with the shipped, tested estimator/test conventions; the prior lowercase text
>   was inconsistent with released reality. This changes no released DGP
>   implementation, generated data, truth table, RNG stream, fingerprint, estimator
>   behavior, or campaign evidence. (2) Tightens the synthetic covariate definition
>   to require consecutive one-based integer suffixes without gaps or duplicates,
>   disjoint from reserved columns — a forward-only definitional tightening
>   satisfied by every released version. (3) Normalizes the human-facing term
>   `Real DGP` to `real-data DGP` in Articles I §1.3, I §1.7, and III §3.2.B; the
>   machine discriminator `type = "real"` is unchanged, and the deferred real-data
>   capability remains unimplemented and unauthorized. The 2.0.0 amendment-history
>   entry and all historical RFC records are preserved verbatim. The bump is
>   *patch*.

## Implementation Follow-Through

A later correction packet — not this RFC — must authorize the minimum work below.
Nothing here is implemented in this cycle.

- **Validator (primary home: `cs_check_dgp_synthetic()`, [R/cs-contracts.R]).**
  After the core-column checks, compute
  `covs <- setdiff(names(df), reserved)` where `reserved` is the governed reserved
  set, and assert: (a) every covariate matches `^X[1-9][0-9]*$`; (b) the integer
  suffixes are exactly `1:k` (consecutive, one-based, no gaps, no duplicates);
  (c) `covs` is disjoint from the reserved set; (d) `k >= 1`. Emit a classed
  `causalstress_dgp_error`.
- **Registry executable pass (`cs_validate_dgp_registry()`,
  [R/cs-validate-registry.R]).** Extend the per-`(dgp_id, version)` synthetic
  generator check (currently `y0,y1,p,structural_te` only) to run the same
  covariate-name predicate on every one of the 24 versions, honoring the
  Validation Contract's "version-aware executable validation, not
  first-match-per-id."
- **Public certification (`cs_validate_dgp()`, [R/cs-validate-dgp.R]).** Add the
  covariate-name predicate to the `schema` check so user-supplied synthetic DGPs
  are rejected on non-compliant names, consistent with "Public DGP certification"
  in the Validation Contract.
- **Contract tests.** Add positive tests (all 24 registered generators pass) and
  negative tests (lowercase `x1`, gap `X1,X3`, duplicate, bare `X`, and a
  reserved-name collision each abort with `causalstress_dgp_error`). Existing DGP
  tests need no change — they already assert uppercase consecutive names.
- **Documentation.** Update `contracts.md` "DGP Contract" and "Validation
  Contract" sections to state the `X1...Xk` predicate and reserved-column
  disjointness. `README.md` index is updated in this same change (below).
- **Explicitly do not:** touch any generator, RNG context, oracle, fingerprint
  schema, or the `type = "real"` discriminator.

## Rejected Alternatives

- **Major constitutional bump (v3.0.0 or v2.1.0) for the casing fix.** Rejected.
  A major bump is reserved for changes to the *semantic meaning* of an article
  (Preamble [:15-17]). The corrected text changes no released semantics — every
  conforming implementation already used uppercase `X`, and no truth/data/RNG/
  fingerprint/estimator behavior moves. Patch is the correct grade.
- **Adopt Option B (registry-declared names) now for flexibility.** Rejected for
  this cycle. It expands the registry/validation contract for zero synthetic-side
  benefit and its real use case (empirical covariate names) is out of scope;
  deferred to the future real-data RFC.
- **Rename released DGP outputs (Option C).** Rejected — violates Article II/VII
  immutability; see Option Assessment.
- **Split into two separate RFC cycles (one per correction).** Rejected as
  over-process: both are pure-notation, patch-level, and non-interacting.
  Enumerated coupling (above) achieves the same auditability at lower cost.
- **Retroactively normalize `Real DGP` in the 2.0.0 amendment-history entry.**
  Rejected — editing a historical record violates the README's prohibition on
  silently editing old scientific history.

## Open Risks and Deferred Questions

- **`grep("^X")` estimators are incompatible with future real-data covariates.**
  `est-lm-att.R:43` and `est-ipw-att.R:42` abort when no `X`-prefixed column
  exists. Real-data DGPs (§3.2.B) impose no `X` naming, so these estimators will
  fail on empirical covariate names. Record in `horizon.md`; resolve inside the
  real-data RFC (candidate: pass an explicit covariate roster to estimators rather
  than discovering by prefix). **Must not be pulled into this amendment.**
- **Reserved-column set is not yet centrally governed.** The follow-through
  validator needs one authoritative reserved-name list (`y,w,p,y0,y1,
  structural_te`, `unit_id`, and truth-table keys `tau,tau_id`). Defining that set
  is a small design question that belongs to the correction packet, not here.
- **Wave 2 `unit_id` and held-out covariate frames.** CATE's held-out predict
  input is "covariates plus `unit_id`" (§3.1). The covariate-name predicate must
  be applied consistently to held-out frames when Wave 2 lands; deferred to Wave 2.
- **Arbitrary/semantic covariate naming for synthetic DGPs** (Option B) remains a
  legitimate future question if synthetic DGPs ever need semantic feature names;
  parked, not foreclosed.

---

*End of response. Non-binding. No synthesis created; no Constitution, DGP,
estimator, or test code modified.*
