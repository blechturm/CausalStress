# RFC Synthesis: Batch 3 Oracle Access and Bitwise Scope

**Status:** Accepted as amended by maintainer on 2026-06-13
**Date:** 2026-06-13
**Packet:** `causalstress_v0_1_10_spec_packet`
**Tickets:** CS-1108, CS-1109
**Sources:** audit M1, audit M9, `CAUSAL_STRESS_CONSTITUTION.md`,
`contracts.md`, Batch 3 packet tickets

## Process Note

This synthesis intentionally starts at the maintainer-facing decision document
stage. The audit finding, the adversarial audit review, and the v0.1.10 packet
cut already provide the seed and response material:

- M1 states the implementation over-grants oracle access by returning the full
  raw DGP dataframe to any registry-flagged oracle estimator.
- M9 states the Article II cross-platform bitwise claim is stronger than the
  current R/BLAS/libm substrate can honestly guarantee.
- `contracts.md` records the exact decision fork: implement column-scoped
  config-based oracle access, or amend the Constitution to bless the broad
  registry flag.

No implementation should begin until this synthesis is accepted or amended by
the maintainer.

## Decision 1: Oracle Access Mechanism

### Accepted Recommendation

Implement column-scoped oracle access. Do not amend the Constitution to bless
the current broad registry-flag behavior.

This is an intentionally stricter implementation of Article III Section 3.1.
The lead sentence permits explicitly oracle-configured estimators to receive
truth columns, while the parenthetical example names `p`. This synthesis treats
that example as illustrative, not exhaustive, and clarifies the v0.1.10 rule:
oracle access is column-scoped, and structural-oracle benchmark estimators may
receive `structural_te` when explicitly granted. The current broad registry flag
may be defensible under a permissive reading of Article III, but it is still an
unsafe over-grant. Batch 3 chooses the narrower rule.

The runner airlock should remain closed by default. Oracle columns are granted
only by explicit column intent:

- `config$use_true_propensity = TRUE` grants `p`.
- `config$use_structural_te = TRUE` grants `structural_te` when the estimator
  descriptor declares that column eligible.
- `y0` and `y1` are never passed to estimators through the ordinary runner
  airlock in v0.1.10.
- The registry `oracle` flag is provenance/eligibility metadata, not a blanket
  grant to the raw DGP dataframe.

### Required Implementation Shape

- Replace `cs_airlock(df, oracle_allowed = TRUE/FALSE)` with a helper that
  computes allowed oracle columns from estimator config and descriptor.
- Add estimator-descriptor fields:
  - `oracle_columns`: character vector of truth columns the estimator is
    eligible to receive. Allowed v0.1.10 values are `p` and `structural_te`.
  - `oracle_default_columns`: character vector of eligible truth columns granted
    without a user config flag. This must be empty except for internal benchmark
    estimators that cannot function otherwise.
- Compute granted columns as the union of descriptor defaults and requested
  config flags, checked against `oracle_columns`. A request for an ineligible
  oracle column must fail closed with a classed runner/airlock error.
- Register `oracle_att` with `oracle_columns = "structural_te"` and
  `oracle_default_columns = "structural_te"`, so it continues to work without
  exposing `p`, `y0`, or `y1`.
- Preserve attribute stripping after column selection, even for oracle grants.
- Keep `oracle_att` working by giving it a structural-treatment-effect grant
  through an explicit internal config/default, not by exposing `y0`, `y1`, or
  `p`.
- Record granted oracle columns in result metadata for auditability.

### Required Tests

- A non-oracle estimator sees none of `y0`, `y1`, `p`, or `structural_te`.
- A propensity-oracle estimator with `config$use_true_propensity = TRUE` sees
  `p` and does not see `y0`, `y1`, or `structural_te`.
- A registry-flagged oracle estimator with no column grant does not receive the
  full raw dataframe.
- `oracle_att` still runs and receives only the structural treatment effect it
  needs.
- Result metadata records the exact `oracle_columns_granted` vector.
- An estimator requesting an oracle column not listed in its descriptor fails
  closed.

### Rejected Alternative

Amend Article III to authorize `est_desc$oracle == TRUE` as a blanket raw-DGP
grant.

Reason: this would preserve the exact defect in M1. It makes a propensity-only
oracle impossible to express safely and grants counterfactual truth to code that
only needs `p`. The decision is not that Article III unambiguously forbids every
possible blanket grant; it is that v0.1.10 adopts a stricter column-scoped
contract because broad raw-DGP grants are unnecessary and audit-hostile.

## Decision 2: Article II Bitwise Scope

### Accepted Recommendation

Amend Article II to use a two-tier reproducibility claim:

1. Same-substrate identity: for the same DGP id, version, seed, R version,
   platform, and numerical-library substrate, DGP data and truth outputs must be
   bitwise identical under the mandated RNG policy.
2. Cross-substrate reproducibility: across operating systems, R versions, BLAS,
   LAPACK, and libm implementations, CausalStress claims documented
   tolerance-level numerical reproducibility unless a version-specific
   regression corpus proves bitwise identity.

The old unconditional phrase "regardless of the operating system or R version
used" is not retained for BLAS/libm-sensitive DGPs.

### Rationale

Current stable DGPs use operations whose final bits can depend on platform math
libraries:

- `chol()` and `%*%` route through BLAS/LAPACK in high-dimensional Gaussian
  DGPs.
- `plogis()` routes through platform math functions.
- Tiny differences in propensity can affect `rbinom()` treatment assignment at
  boundary draws.

Changing released DGP implementations in place would violate Article VII.
Adding new pure-R DGP versions for every affected design is larger than v0.1.10
and still does not solve all libm surfaces without replacing large parts of R's
math stack. The honest v0.1.10 repair is to scope the constitutional claim and
record validation evidence.

### Required Documentation Change

Patch Article II Section 2.2 so the bitwise guarantee applies to the same
declared computational substrate. Add a cross-substrate tolerance clause and a
requirement that release evidence records the R version, platform, and relevant
numeric-library/thread substrate used for validation.

Patch Article VII Section 7.2.d to cross-reference the same substrate scope
when using bitwise-identical outputs as the bar for a performance-only refactor
that keeps a DGP version unchanged.

### Required Validation Evidence

- Add a same-substrate regression test for at least one BLAS-sensitive DGP
  (`synth_hd_sparse_plm`) proving repeated same-version/same-seed generation is
  bitwise identical on the validation machine.
- Release closeout must record that cross-platform bitwise identity is not
  claimed for v0.1.10 unless separately proven by a regression corpus.
- `release_gate.md` must require release evidence to record R version,
  platform, and relevant numeric-library/thread substrate for reproducibility
  validation.

### Rejected Alternatives

- Mutate released DGP versions to pure-R implementations in place. Rejected:
  violates Article VII immutability.
- Add new DGP versions for all BLAS/libm-sensitive designs in v0.1.10. Rejected:
  too broad for this audit-repair packet and still incomplete without a full
  math-substrate policy.
- Leave Article II unchanged and rely on local tests. Rejected: preserves a
  known constitutional overclaim.

## Acceptance Checklist

Maintainer acceptance of this RFC authorizes Batch 3 implementation to:

- implement the column-scoped airlock and tests for CS-1108;
- patch Article II bitwise-scope wording and add same-substrate validation
  evidence for CS-1109;
- patch Article VII Section 7.2.d to share Article II's substrate scope;
- update `release_gate.md` with numeric-substrate evidence requirements;
- update `contracts.md`, packet ticket statuses, and release closeout notes
  consistently.

If the maintainer rejects either decision, Batch 3 must stop and a revised RFC
must be drafted before implementation.
