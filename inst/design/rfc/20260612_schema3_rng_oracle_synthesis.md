# RFC Synthesis: Schema 3 Fingerprints, RNG Isolation, and Oracle Truth Cache Identity

**Status:** Accepted as amended
**Date:** 2026-06-12
**Packet:** `causalstress_v0_1_10_spec_packet`
**Ticket:** CS-1105
**Sources:** audit C2, C3, M3, M7, M8, M12; `contracts.md`;
`CAUSAL_STRESS_CONSTITUTION.md`

This RFC starts at synthesis because the v0.1.9 audit Rev 2 served as the seed
and the independent adversarial audit review plus Batch 2 review served as the
response stage.

## Scope

This RFC defines the coordinated v0.1.10 design for:

- schema-3 run/config/task fingerprints and resume comparison;
- DGP-version-aware pin identity;
- RNG side-effect isolation at package load, validation, planning, and runner
  boundaries;
- oracle truth cache identity and atomic writes.

It intentionally does not implement the design. CS-1106, CS-1107, and CS-1117
remain blocked until this synthesis is accepted.

## Problem Statement

The audited failures are coupled and should not be fixed piecemeal.

- C2: default-config resume fails because stored fingerprints include
  runner-injected fields (`ci_method_source`) and CI intent differs between
  store and resume paths.
- M3: parallel execution fingerprints forced `num_threads = 1L`, while resume
  compares against the caller config.
- M7: pin identity and schema-2 fingerprints omit `dgp_version`, allowing stale
  old-version results to satisfy a new stable DGP lookup.
- M12: batch planning labels fingerprints as schema 2 while hashing raw config
  lists directly, bypassing canonicalization and unsupported-value rejection.
- C3: load-time validation executes DGP generators and leaks RNG kind/seed into
  the user's session; campaign shuffling depends on ambient `sample.kind`.
- M8: oracle truth cache files are keyed only by `(dgp_id, version)`, omit
  oracle algorithm identity, and are written non-atomically.

## Decision

Adopt a single schema-3 provenance spine for new v0.1.10 artifacts.

### 1. Schema-3 Fingerprint Payload

New run fingerprints must be built through one canonical helper and store
`config_fingerprint_schema = 3L`.

The schema-3 payload must include:

- `config_fingerprint_schema = 3L`;
- `dgp_id`;
- `dgp_version`;
- `estimator_id`;
- `estimator_version`;
- `n`;
- `seed`;
- `oracle`;
- `max_runtime`;
- `tau_id`;
- normalized `ci_intent`;
- canonical estimator config after removing runner-only bookkeeping fields;
- optional `parallel_policy` fields that affect science outputs, not worker
  implementation details.

The canonical config normalizer must:

- sort named list entries recursively;
- canonicalize numeric values through the existing stable numeric/tau
  representation;
- reject functions, environments, calls, expressions, external pointers, S4,
  reference objects, and unsupported classed objects;
- remove runner-injected bookkeeping keys before hashing, including `seed`,
  `ci_method`, `ci_method_source`, `estimator_id`, `num_threads` when it is
  worker-enforced parallel bookkeeping, and future keys explicitly marked as
  non-science provenance.

`ci_intent` must be derived by a shared helper used by store, resume, grid
campaigns, and batch planning. The helper must receive the caller-supplied
pre-injection config plus `(bootstrap, B)` and must run before any runner
injects `ci_method`, `ci_method_source`, `seed`, or worker thread caps:

- explicit `config$ci_method` wins;
- `bootstrap = TRUE` with `B > 0` maps to `"bootstrap"` when no explicit method
  is provided;
- otherwise the intent is `"none"` for the current runner default.

This makes the default no-CI path hash the same way on store and resume.

### 2. Pin Identity and Resume Rules

New result pins must include `dgp_version` in identity. The recommended v0.1.10
pin name is:

`results__dgp={dgp_id}__dgpver={dgp_version}__est={estimator_id}__n={n}__seed={seed}`

Pin metadata must also carry `dgp_version`, `estimator_version`,
`config_fingerprint_schema`, and `config_fingerprint`.

Resume lookup rules:

1. Resolve the requested DGP to an exact `(dgp_id, dgp_version)` before looking
   for a pin.
2. For schema-3 pins, compare the schema-3 fingerprint exactly.
3. For schema-2 pins, keep deterministic read/compare support for historical
   artifacts but do not let a schema-2 pin satisfy a request whose resolved DGP
   version cannot be proven equal from metadata.
4. For legacy v0.1.7 pins without schema metadata, preserve existing read
   support and continue refusing resume under finite `max_runtime`.
5. `force = TRUE` must delete or overwrite only the exact versioned pin target,
   not every historical result for a `(dgp_id, estimator_id, n, seed)` tuple.

### 3. Batch Planning Fingerprints

`cs_plan_campaign()` must stop hashing raw config lists directly. It must call
the same schema-3 canonical helper used by runner store/resume paths.

Plan task rows must carry:

- `dgp_version` after registry resolution;
- `estimator_version`;
- `resolved_config_hash` from canonical normalized config;
- `task_fingerprint` from `(dgp_id, dgp_version, estimator_id,
  estimator_version, n, seed, tau_id, ci_intent, canonical_config)`;
- `config_fingerprint_schema = 3L`;
- `fingerprint_version = 3L`.

Batch workers must execute the exact DGP version recorded in the plan, not a
fresh "latest stable" lookup.

### 4. RNG Isolation

Package load, validation helpers, and planning must not leak RNG side effects.

Required rules:

- `.onLoad()` validation must preserve both RNG kind and `.Random.seed` state.
  If `.Random.seed` was absent before load, it must remain absent afterward.
- `cs_validate_dgp_registry()` and executable DGP validation must preserve RNG
  state around generator probes.
- `cs_plan_campaign()` must use a local mandated RNG scope for task shuffling:
  `Mersenne-Twister`, `Inversion`, `Rounding`, with a local seed, and must
  restore the caller state afterward.
- Runner execution may set the mandated RNG kind for benchmark work, but this
  side effect must be documented as runner-scope behavior. Helpers that are not
  runner entry points must preserve caller RNG state unless they explicitly
  document otherwise.
- The preservation helper must restore all three values returned by `RNGkind()`
  and must handle the absent-seed case.

### 5. Oracle Truth Cache Identity and Atomicity

Oracle truth cache identity must include the oracle algorithm identity, not just
the DGP identity.

Define an internal oracle algorithm descriptor containing at least:

- `oracle_algorithm_version`;
- `ORACLE_SEED`;
- `ORACLE_N`;
- `chunk_n`;
- `tau_id`;
- quantile type;
- retention rule identifier;
- CausalStress package version or source hash for the oracle helper.

The cache key must include:

- `dgp_id`;
- `dgp_version`;
- `oracle_algorithm_fingerprint`.

Cached payloads must store the same metadata and must be invalidated if any
metadata field mismatches the requested descriptor.

Writes must be atomic:

1. write to a unique temporary file in the target cache directory;
2. close and verify the temp file exists and is non-empty;
3. rename to the final path;
4. fail loudly if rename fails;
5. never return a partially read cache file.

The current in-process recursion guard may remain as a convenience, but it is
not a cross-process lock and must not be documented as one.

## Rejected Alternatives

- **Patch schema 2 in place.** Rejected because existing schema-2 artifacts
  already have ambiguous semantics. Mutating schema 2 would make historical
  metadata harder to interpret.
- **Only strip `ci_method_source`.** Rejected because it leaves DGP version,
  planner hashing, parallel `num_threads`, and oracle cache identity unresolved.
- **Keep unversioned pin names and rely on fingerprints.** Rejected because pin
  lookup currently happens before fingerprint comparison, and force/delete
  behavior needs exact artifact identity.
- **Disable load-time validation instead of preserving RNG state.** Rejected
  because validation-on-load remains useful, and the constitutional violation is
  the leaked side effect, not validation itself.
- **Key oracle cache only by package version.** Rejected because package version
  is too coarse for local source changes and too indirect for algorithm
  parameters such as chunk size and quantile type.

## Required Implementation Tickets After Acceptance

CS-1106 must implement:

- schema-3 canonical fingerprint helper;
- shared CI-intent helper;
- versioned pin naming and exact-version resume lookup;
- schema-2 and v0.1.7 deterministic legacy read support;
- planner use of canonical fingerprints;
- tests for default-config resume, DGP version changes, planner canonicalization,
  unsupported config values, `bootstrap = TRUE, B = 0` CI intent, and parallel
  `num_threads` normalization.

CS-1107 must implement:

- RNG preserve/restore helper;
- `.onLoad()` and validation preservation;
- mandated local RNG scope for campaign shuffling;
- tests for absent `.Random.seed`, present `.Random.seed`, restored `RNGkind()`,
  and campaign plan equality across ambient RNG kinds.

CS-1117 must implement:

- oracle algorithm descriptor and fingerprint;
- versioned cache keys and payload metadata checks;
- atomic cache writes;
- stale-cache invalidation tests;
- race/partial-file behavior tests where feasible without nondeterministic
  timing assumptions.

## Acceptance Criteria

This RFC is accepted when a maintainer records one of:

- accept as written;
- accept with explicit amendments listed in the active packet;
- reject and replace with a different schema/RNG/cache design.

Implementation must not begin until acceptance is recorded.

## Acceptance Record

Maintainer accepted CS-1105 as amended on 2026-06-12. The accepted amendments
are the pre-injection `ci_intent` rule, explicit handling of worker-enforced
`num_threads`, the `bootstrap = TRUE, B = 0` test obligation, the Article VI
scope correction, and the audit-trail note explaining why this RFC starts at
synthesis.

## Open Risks

- Versioned pin names may require compatibility shims in user-facing delete and
  read helpers.
- Existing tests may assume schema-2 pin names. Those tests should be updated
  only where they assert new v0.1.10 behavior; historical-read tests should keep
  schema-2 fixtures.
- Atomic rename semantics differ across filesystems. The implementation must
  fail loudly and leave enough diagnostics for Windows cache directories.
