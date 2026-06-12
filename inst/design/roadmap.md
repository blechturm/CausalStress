# CausalStress Roadmap

**Status:** Active roadmap
**Authority:** Planning document below active packets and accepted RFC syntheses.

## Current Horizon: v0.1.10

Goal: convert the v0.1.9 audit Rev 2 into a governed repair cycle and bootstrap
the spec-packet process.

Planned sequence:

1. Governance bootstrap: authority README, contract index, release gate, RFC
   cycle, canonical templates, active v0.1.10 packet.
2. Mechanical high-yield repairs: C4 disposition/implementation, C1, C5, M6,
   M19, and adversarial contract tests.
3. Schema/RNG/oracle-truth-cache design change: schema 3 fingerprint/resume
   model, RNG isolation, and oracle truth cache identity, preceded by a short
   RFC.
4. Constitutional decisions: oracle-access mechanism and Art. II bitwise scope
   for BLAS/libm-sensitive DGPs.
5. Governance conformance: gatekeeper, parallel governance, warnings, bootstrap
   failure semantics, dropped arguments, and batching spec gaps.
6. Cleanup: minor findings, design-doc corrections, archive migration, and
   release closeout.

## Non-Scope For v0.1.10

- new DGP families.
- new estimators.
- sensitivity analysis.
- Python spoke work.
- CRAN hardening beyond the release gate required by the packet.

## Later Horizons

- Registry-spec version bump for stale scientific registry entries.
- Sensitivity-analysis design, if still desired, as a DGP stress-axis RFC.
- Python interoperability after the R package contract is stable.
- Public article/vignette polish after runner and artifact contracts settle.
