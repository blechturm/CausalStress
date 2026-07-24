# RFC Seed: DGP Contract Terminology Clarifications

**Status:** SEED — non-binding until an independent response, synthesis, and
maintainer acceptance complete the RFC cycle
**Date:** 2026-07-22
**Proposed constitutional version:** 2.0.1

## Decision Requested

Clarify Constitution Article III Section 3.2.A so canonical synthetic-DGP
covariates are named `X1`, ..., `Xk` (uppercase `X`, consecutive one-based
integer suffixes), matching every released package-managed DGP version and the
existing estimator-facing convention.

Normalize the active Constitution's term `Real DGP` to **real-data DGP**. This
is terminology only: real-data DGP support remains a planned future capability
and is not implemented or authorized by this RFC.

This is a patch-level constitutional clarification. It corrects the lowercase
`x1...xk` spelling in the current text; it does not change the scientific,
data-generating, truth, or RNG semantics of any DGP.

## Problem

Constitution v2.0.0 Article III Section 3.2.A currently requires covariates
named `x1...xk`. All 24 registered versions across the 12 package-managed DGP
IDs return uppercase names such as `X1`, ..., `X5` or `X1`, ..., `X100`.
Several built-in estimator contracts also discover covariates using the
uppercase `X*` convention.

Renaming released DGP outputs would violate DGP immutability and would create a
large, scientifically empty compatibility break. Leaving the constitutional
text unchanged would instead make all released DGPs formally non-compliant.

The current phrase `Real DGP` is also ambiguous: it can be misread as
contrasting a genuine DGP with the package's synthetic DGPs. `Real-data DGP`
accurately identifies the intended distinction between generated data and an
empirical-data benchmark.

## Options

### Option A — Ratify uppercase `X1...Xk` as canonical (recommended)

Amend the Article III Section 3.2.A return contract to read:

> `df`: Tibble including `y`, `w`, `p`, `y0`, `y1`, and covariates named
> `X1`, ..., `Xk` (uppercase `X`, consecutive one-based integer suffixes).

Add a Constitution v2.0.1 amendment-history entry recording that this is a
notation correction aligned with immutable released DGP outputs.

Consequences:

- no released DGP implementation or version changes;
- no generated data, truth table, RNG stream, or fingerprint changes;
- validators should enforce the canonical names and consecutive ordering;
- future user-defined synthetic DGP contracts receive one unambiguous naming
  convention.

### Option B — Permit arbitrary registry-declared covariate names

This is more flexible, but the registry does not currently carry a governed
covariate-name declaration. It would expand the registry and validation
contract, and some built-in estimators currently assume uppercase `X*` names.
This is not a clarification-only change.

### Option C — Rename DGP outputs to lowercase

Reject. Released `(dgp_id, version)` outputs are constitutionally immutable.
New versions solely for a case-only rename would fragment the evidence lineage
without scientific benefit.

## Recommended Decision

Accept Option A, normalize `Real DGP` to `real-data DGP` in the active
Constitution, and ratify Constitution v2.0.1. Implement the validator change
through a narrow spec/correction ticket after ratification. Do not modify any
released DGP generator, and do not implement the deferred real-data capability
in this cycle.

## Blast Radius

The constitutional edit is small. The implementation follow-through is limited
to validation and focused contract tests. Existing DGPs and estimators already
use the recommended convention, so campaign results and scientific identities
remain unchanged.

## Explicitly Out of Scope

- implementation of real-data DGP support;
- arbitrary feature-name registration;
- DGP generator or version changes;
- estimator API redesign;
- fingerprint or persistence-schema changes.
