# CausalStress 0.1.0

- First MVP:
  - Synthetic DGPs: `synth_baseline`, `synth_heavytail`
  - Estimators: `oracle_att`, `lm_att`, `ipw_att`, `grf_dr_att` (optional)
  - Registry system for DGPs and estimators
  - Runners: `cs_run_single()`, `cs_run_seeds()`
  - Summary: `cs_summarise_runs()`
  - Contracts + tests for DGPs, estimators, runners, truth helpers


# CausalStress 0.1.1

* **Documentation:** Added comprehensive `README.Rmd` and "From Run to History" vignette.
* **UX:** Implemented `cs_audit()` for provenance tracking and `cs_tidy()` for unified output.
* **Resilience:** Enabled atomic persistence and crash recovery via `pins`.
* **Fixes:** Silenced console spam from runners and added progress bars.# CausalStress 0.1.2

### Major Improvements
* Added **bootstrap-resume guard**: the runner now refuses to reuse cached 
  results if they were generated without bootstrap CIs but the user requests
  `bootstrap = TRUE`. Prevents silent reuse of incompatible pins.

* Introduced **`cs_airlock()`**, a centralized helper for dropping forbidden
  columns (`y0`, `y1`, `p`, `structural_te`) and attributes, hardening the 
  constitutional “Airlock” rule. Integrated in all runner paths.

* Added **`cs_collect_att()`** and **`cs_collect_qst()`**, lightweight
  collectors for ATT/QST fields, improving ergonomics when analyzing large
  campaigns.

### Runner & Resume Behavior
* Runners now raise a clear error when `skip_existing = TRUE` encounters a pin
  computed under mismatched bootstrap settings.
* Added full resume tests (including spy estimators) verifying Airlock behavior
  even after pins reload.

### Documentation
* README updated to clarify that **v0.1.x is serial-only**, with parallelism
  planned for v0.2.0.
* Added clearer examples for bootstrap usage and tidy collectors.
* Constitution updated to Version **1.7.2**, including revised Gatekeeper rules 
  and prohibitions.

### Developer Experience
* Added alias `cs_run_campaign()` for design doc consistency.
* Expanded internal triage roadmap with planned improvements (config 
  fingerprint, expanded provenance, parallel safety).
* 400+ tests passing, ensuring that refactors are fully covered.

