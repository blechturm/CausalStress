## CausalStress – Suggested Enhancements (Non‑Critical)

### Parallel Safety & Progress
- Document how to enable progress bars: `progressr::handlers(global = TRUE)` or wrap runs in `progressr::with_progress({ ... })`.
- If parallel backends are added later, consider a Stage-and-Gather pattern (compute locally, pin sequentially) to avoid pins race conditions.

### Airlock Hardening
- Factor a small helper (e.g., `cs_airlock(df, oracle_allowed)`) to centralize dropping `y0`, `y1`, `p`, `structural_te` and related attributes.
- Add a contract-level test that any non-oracle estimator fails if oracle columns are present (in addition to the spy test).

### Provenance & Manifesting
- Ensure optional/registered estimators set and pin an `estimator_version` (not just core estimators using the package version).
- Build a read-only “manifest” helper atop `cs_audit()` to summarize counts by dgp/estimator/git_hash for quick provenance queries.

### Pins & Resume
- Naming is deterministic; for very large campaigns consider chunked pinning/subfolders to avoid OS limits on many files in one directory.
- Document that `skip_existing` assumes cached results are schema/version compatible; optionally check `estimator_version`/`git_hash` before reuse.

### Truth & Oracle Cache
- Keep option `causalstress.N_oracle` documented for users needing lighter oracles; note that `cs_get_oracle_qst()` seeds deterministically and caches by dgp/version/N.

### Estimator Registry
- Set `capabilities` explicitly for all estimators (e.g., `c("att")`) to avoid NULL checks downstream.
- If adding estimators with internal parallelism, default to single-threaded unless user overrides.

### Documentation & UX
- Add README/vignette snippets:
  - Progress: enable handler and wrap `cs_run_seeds`/`cs_run_grid` in `with_progress`.
  - Persistence: per-seed silent pins; resumable with `skip_existing`.
  - Airlock: non-oracle estimators never see oracle fields.

### Edge Cases
- Non-git environments: consider a one-time warning when `git_hash` is NA during pinning (optional).
- Session info size: metadata includes full `sessionInfo()`; consider an opt-out or slimmer summary for massive campaigns (optional).
