# Governance Audit & Proposal: Spec-Packet-Driven Development for CausalStress

**Date:** 2026-06-11
**Type:** Governance audit + process proposal
**Inputs:** `ledgr/inst/design/` governance structure (README authority index, rfc_cycle.md, spec packets v0.1.0 → v0.1.9.4, audits/ routing practice); CausalStress `inst/design/` as-is; findings from `audit/v0_1_9_deep_code_review_audit.md` (esp. Section 8 process observations and finding D5).
**Decision sought:** Adopt the spec-packet / spec-document-driven development pattern for CausalStress, **with the Constitution retained as the supreme authority above all process artifacts.**
**Status:** PROPOSED — pending maintainer acceptance.
**Revision 2 (2026-06-11):** Synced to audit Rev 2 (post-adversarial-review) and corrected per the adversarial review of this proposal: C1 contract-test semantics fixed, M19 routed, Batch 0 (authority index + contracts) made mandatory and first, release gate now requires an *enhanced* strict validator, C4 moved from mechanical work to a decision-gated batch, the historical-packets rule softened for artifact-compatibility reading, and a self-containment requirement added so the governance contract does not depend on the ledgr repo.
**Revision 3 (2026-06-11):** Second adversarial pass corrections: C4 restored to fix-order priority 1 — disposition decided at packet cut (a one-time product decision, not RFC-gated), implementation in Batch 1, decoupled from M1/M9; the missing-package invariant unified (post-M15 fix: missing package → `success = FALSE` result row; `batch$errors` reserved for failures escaping `cs_run_single()`); release-gate item 2 split into a named three-surface validation suite (registry/sidecar, version-aware executable, public certification); canonical templates relocated to `inst/design/templates/` instead of inside the first packet.

---

## 1. Why this is needed (diagnosis)

The v0.1.9 deep code review (Rev 2, after adversarial verification) found that the DGP truth/oracle core held up under hostile review, but the estimator and gatekeeper statistical surfaces still carry material defects (a wrong estimand under an `_att` id in `est_tmle`, C4; a gatekeeper that passes CI-less estimators, M2; a DGP validator that certifies missing potential outcomes, M19). The common thread across both the statistical and the operational defects, however, is *process*:

1. **No authority model.** `inst/design/` contains 16 documents with no index declaring which is binding, in what order, or for which version. The "Frozen" v0.3.0 design describes APIs that never shipped; two specs define conflicting pin-naming conventions; the registry spec is stale against the code; the design doc cites a superseded Constitution version. A reader — human or LLM agent — cannot determine the current normative state without archaeology. (Audit finding D5.)
2. **Spec drift is undetected.** Several major findings have the shape "the spec mandates X, the code does Y, nothing checks": the v0.1.8 patch spec's parallel gating is absent from the v0.1.9 campaign path; the batching spec's error schema, consolidator validation, and tidy propagation requirements were partially implemented; YAML sidecar validation is warning-only at load, and sidecar version/status fields are never checked at all (audit M10, Rev 2 — noise/effect drift *is* test-covered, the rest is not). There is no release gate that walks the spec's acceptance criteria.
3. **Tests verify the implementation, not the contract.** The two worst bugs (silent error loss; broken resume) survive because tests were written around the bug — hand-crafted fingerprints, avoided error paths — rather than through the documented contract.
4. **Audit findings have no routing discipline.** `V0.1.8_backlock_tickets.md` exists, but there is no rule forcing each finding to be ticketed, deferred-with-rationale, or rejected-with-rationale before a release closes.

These are exactly the failure modes ledgr's governance structure was built to prevent, and its 40+ completed spec packets demonstrate the pattern works at this scale of solo-maintainer + LLM-agent development.

---

## 2. What ledgr does (analysis of the reference structure)

From `ledgr/inst/design/`:

### 2.1 The authority-leveled design index
`README.md` is the single entry point. It declares **authority levels** explicitly:

| Role | Meaning (ledgr) |
| --- | --- |
| Contract | Must be preserved unless changed by a new spec or ADR |
| Roadmap | Milestone sequence, active horizon |
| Spec packet | Versioned implementation plan and ticket record |
| Accepted design decision | RFC synthesis accepted by maintainer; binding within scope |
| RFC / response | Proposal; binding only after acceptance |
| Audit / review | Findings that **must be routed** before release |
| Spike | Exploratory; informative unless promoted |
| Horizon note | Non-binding parking lot |

It also states the *current planning state* ("latest completed packet", "current active packet: none/cut"), and carries a **maintenance rule**: any document add/move/retire must update the README in the same change.

Key insight: historical packets are **records, not instructions**. "Do not treat an older packet as current just because it contains detailed instructions." This single rule eliminates the CausalStress problem of a frozen-but-stale v0.3.0 design competing with patch specs for authority.

### 2.2 The spec packet
One directory per release: `ledgr_v0_1_9_4_spec_packet/` containing:

- `v0_1_9_4_spec.md` — the bounded implementation spec, with explicit **non-scope** lists ("X, Y, Z remain non-scope")
- `v0_1_9_4_tickets.md` — human-readable tickets
- `tickets.yml` — machine-readable tickets (LLM agents consume these directly)
- `batch_plan.md` — execution order, batching of tickets, review checkpoints ("complete after Claude review" per batch)
- `release_closeout.md` — what shipped, what was deferred and where it went
- optionally `cycle_retrospective.md` — process lessons

The packet is *cut* (opened) deliberately, is the only authorization for implementation work while active, and becomes an archival record at the release gate.

### 2.3 The RFC cycle (for contested design decisions)
`rfc_cycle.md` documents a deliberately Hegelian seed → response → seed v2 → synthesis → final review pipeline, with **role rotation between different LLM agents** so each stage carries a different failure mode and the disagreement trail is preserved. Only the synthesis is binding, and only after maintainer acceptance. Deferred items get a durable **horizon entry** so they aren't lost.

### 2.4 Audit routing
Audits live in `audits/`, and the README's authority table binds them: "findings that must be routed before release; not all findings remain active after routing." Each completed release names which audit findings it consumed (e.g. "routed v0.1.8.2 auditr findings").

### 2.5 Supporting surfaces
`contracts.md` (authoritative contract index), `horizon.md` (parking lot), `release_ci_playbook.md` (release gate), `vignette_styleguide.md`, a maintainer manual with a two-layer article standard (Synthesis + Implementation Trace), and an explicit pre-CRAN compatibility policy ("intentional breaking changes allowed; accidental drift is not").

### 2.6 What CausalStress should *not* copy
- **The ADR layer.** ledgr created ADRs and then wound them down into manual articles — adopt the lesson, skip the detour.
- **Full packet granularity from day one.** ledgr's 0.1.7.x → 0.1.8.x era shows packets getting sharper over time; CausalStress should start with the mature shape (spec + tickets + tickets.yml + batch plan + closeout) but not retro-create packets for shipped versions.
- **contracts.md as the apex document.** ledgr's contracts are mutable by spec/ADR. CausalStress already has something stronger and should keep it — see Section 3.

---

## 3. The constitutional difference (what CausalStress keeps)

CausalStress is a *scientific instrument*; ledgr is a research engine. The Constitution exists because the package's value proposition is that **truth, reproducibility, and estimator-facing contracts do not move** — a stronger guarantee than ledgr's "contracts may change via spec." This proposal therefore keeps the Constitution as the **apex of the authority hierarchy**, above every process artifact:

```
1. CONSTITUTION            supreme; amendable only via its own Art. process
                           (patch clarifications; major bump for semantics)
2. contracts.md            operational contract index (NEW) — restates the
                           code-level contracts (DGP, estimator, runner output,
                           fingerprint schemas, pin naming, batch schema) with
                           pointers into the Constitution; mutable by spec
                           packet ONLY where the Constitution is silent
3. Active spec packet      the only authorization for implementation work
4. Accepted RFC syntheses  binding within stated scope until consumed by a packet
5. Roadmap                 milestone sequence
6. Audits / reviews        must be routed before the next release gate
7. Spikes / horizon        informative / parking lot
8. Historical packets,     records, not current implementation
   superseded designs      instructions — but they remain normative
                           evidence for reading/migrating the artifacts
                           their era produced (e.g. legacy pin formats,
                           fingerprint schemas)
```

A rule the Constitution already implies but the process must now enforce: **any spec packet ticket that would violate the Constitution is invalid at ticket-cut time**, not at review time. The packet template (Section 5.2) includes a mandatory "Constitutional check" field per ticket.

---

## 4. Proposed directory reorganization

```
inst/design/
├── README.md                          # NEW — authority index (Section 5.1)
├── CAUSAL_STRESS_CONSTITUTION.md      # unchanged location; apex authority
├── contracts.md                       # NEW — operational contract index
├── roadmap.md                         # rewritten from CAUSAL_STRESS_ROADMAP.md
├── horizon.md                         # NEW — non-binding parking lot
├── rfc_cycle.md                       # NEW — adapted from ledgr (Section 5.4)
├── release_gate.md                    # NEW — release checklist (Section 5.5)
├── templates/                         # NEW — canonical packet/RFC templates
│   ├── spec.md, tickets.md, tickets.yml, batch_plan.md, release_closeout.md
├── audit/                             # exists as of this proposal
│   ├── v0_1_9_deep_code_review_audit.md
│   └── governance_spec_packet_proposal.md   (this file)
├── rfc/                               # NEW — RFC threads
├── spikes/                            # NEW — exploratory research
├── causalstress_v0_1_10_spec_packet/  # FIRST packet (Section 6)
│   ├── v0_1_10_spec.md
│   ├── v0_1_10_tickets.md
│   ├── tickets.yml
│   ├── batch_plan.md
│   └── release_closeout.md            (at close)
└── archive/                           # superseded documents, kept verbatim
    ├── CAUSALSTRESS_DESIGN_v0.3.0.md
    ├── CAUSAL_STRESS_MVP_STATUS.md
    ├── CAUSAL_STRESS_V0.1.8.Patch_Spec.md
    ├── V0.1.8_backlock_tickets.md
    ├── DESIGN_V0.1.9_BATCHING.md
    └── ...
```

**Migration disposition for every existing document:**

| Document | Disposition |
| --- | --- |
| `CAUSAL_STRESS_CONSTITUTION.md` | **Keep in place.** Apex authority. One pending amendment from the audit: scope or qualify the Art. II bitwise-identity clause re: BLAS-dependent DGPs (audit M9) — via constitutional patch process. |
| `CAUSAL_STRESS_CONSTITUTION_EXPLANATION.md`, `WHY_A_CONSTITUTION.md` | Keep beside the Constitution as commentary (non-binding; mark as such in README). |
| `THREADMODEL.md` | Fold into `contracts.md` (it is 4 lines) or keep with a "contract-adjacent" authority label. |
| `CAUSALSTRESS_DESIGN_v0.3.0.md` | **Archive.** It is the largest source of drift: contains its own airlock-leaking pseudocode (audit D1), invalid code (D2), an unconstitutional DGP template (D3), and never-shipped APIs. Extract the still-true contract content into `contracts.md`; everything else is history. |
| `CAUSAL_STRESS_V0.1.8.Patch_Spec.md`, `V0.1.8_backlock_tickets.md`, `DESIGN_V0.1.9_BATCHING.md`, `CAUSAL_STRESS_MVP_STATUS.md` | **Archive** as the de-facto packets of past releases. The batching spec's unimplemented requirements (audit M18) get re-ticketed into the v0.1.10 packet — requirements move forward via tickets, not by keeping old specs "active". |
| `CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md`, `CAUSAL_STRESS_FAMILIES_SPEC_v3_2_final.md` | Keep as **versioned scientific registry specs** (a CausalStress-specific document class ledgr doesn't need). Fix the stale entries found in the audit (placebo_tilted params, LaLonde truth table) in a registry-spec version bump, consistent with how DGP code itself is versioned. |
| `CAUSAL_STRESS_METADATA.md` | Fold the still-accurate parts into `contracts.md`; archive the rest. |
| `CAUSALSTRESS_DESIGN_SENSITIVITY.md`, `CAUSAL_STRESS_PYTHON.md`, `CAUSAL_STRESS_DOCUMENTATION_ARCHITECTURE.md` | Move content into `horizon.md` entries (they are future-work documents); archive originals. |
| `CAUSAL_STRESS_ROADMAP.md` | Rewrite as `roadmap.md` with the new milestone arc (Section 6). |

Nothing is deleted. Superseded documents move to `archive/` verbatim — the same preservation discipline the Constitution applies to invalidated DGPs.

---

## 5. Proposed process artifacts

### 5.1 `inst/design/README.md` — the authority index
Adapted from ledgr's. Must contain:
- The authority table from Section 3 above.
- **Current planning state:** latest completed packet, current active packet (or "none — next planned: vX.Y.Z, not cut").
- A reading order for any non-trivial change: Constitution → contracts.md → active packet → relevant RFC/audit.
- The historical-packets-are-records rule, with the artifact-compatibility carve-out: old specs are not *current implementation instructions*, but they remain the normative reference for reading, validating, and migrating the historical artifacts they defined (the v0.1.9 batching spec is the authority on what a legacy batch pin must contain; the v0.1.7 fingerprint scheme is frozen by reference to its era's spec).
- The maintenance rule: every document add/move/retire updates the README in the same change.
- The pre-CRAN compatibility policy, adapted: *artifact formats, fingerprint schemas, and experimental APIs may break pre-CRAN — but anything the Constitution freezes (DGP outputs per (dgp_id, version), truth tables, RNG mandate) may **never** break, pre-CRAN or not.* This is the key place the CausalStress policy is deliberately stricter than ledgr's.

### 5.2 The spec packet template
Per release, directory `causalstress_v{X}_{Y}_{Z}_spec_packet/`:

- **`v..._spec.md`** — sections: Objective; Scope; **Non-scope** (explicit, named); Design; **Constitutional compliance** (which Articles are touched and how compliance is demonstrated); **Acceptance criteria** (testable, walked at release gate); Audit findings consumed (by ID, e.g. "C1–C5, M6 from v0_1_9_deep_code_review_audit").
- **`v..._tickets.md` + `tickets.yml`** — each ticket carries: id, title, motivation (link to audit finding / RFC / roadmap item), files, **constitutional check** (e.g. "touches fingerprint schema → Art. VII provenance; legacy compat required"), **test obligation** (the contract-level test that must exist — see 5.3), and review gate.
- **`batch_plan.md`** — ticket batches in dependency order; each batch closes only after review (the ledgr "complete after Claude review" pattern; for CausalStress, review = a different agent or the maintainer than the implementer).
- **`release_closeout.md`** — shipped/deferred/rejected per ticket; where each deferral went (horizon entry or next packet); release-gate checklist results.

**Packet rules:**
1. No implementation work without an active packet (exception: hotfixes for data-integrity bugs, which still get a retroactive mini-packet entry).
2. A packet is cut from: routed audit findings + accepted RFC syntheses + roadmap items. Nothing enters a packet from a stale design doc directly.
3. Non-scope lists are binding: discovering adjacent work mid-packet produces a horizon entry or a new ticket *proposal*, not silent scope growth.

### 5.3 The contract-test rule (closing the audit's Section 8 gap)
Every ticket that implements or modifies a documented contract must ship a test that exercises the contract **through its public default path**, not a path arranged to pass. Concretely, the v0.1.10 packet must add the adversarial tests the audit (Rev 2) identified as missing:
- resume with a *default* config (no explicit `ci_method`) → must skip, not error;
- batch error semantics, both halves (per audit C1 Rev 2, stated as the **post-M15-fix invariant**): any failure attributable to a single task's execution — estimator error, *and* (once M15 is fixed) a missing estimator package — → a `success = FALSE` row in `batch$results` with the failure recorded in result provenance; `batch$errors` is reserved for failures that still escape `cs_run_single()` (DGP generation failure, registry lookup failure, contract abort); and in all cases `length(results) + nrow(errors) == nrow(tasks)` (count reconciliation). (Pre-fix, the `packageVersion()` crash escapes and vanishes — that is the bug, not the target semantics.)
- a CI-less estimator through both gatekeeper paths → must be labeled "Unverified", never PASS/NA;
- a missing Suggests package mid-grid → `success = FALSE` row, not an aborted grid (the grid-path statement of the same M15 invariant above);
- `cs_validate_dgp()` on a synthetic DGP missing `y0`/`y1` → must fail, not certify (audit M19);
- `library(CausalStress)` RNG isolation, both cases: in a session where `.Random.seed` does not yet exist → it must still not exist after load; in a session where it does → `RNGkind()` and `.Random.seed` byte-identical after load.

### 5.4 The RFC cycle (adopted, scoped)
Adopt ledgr's `rfc_cycle.md` (seed → response → optional seed v2 → synthesis → final review, role rotation between agents, versioned-file naming, horizon entries for deferrals) for decisions that are **contested or expensive to reverse**. For CausalStress that means:
- new DGP families / estimand definitions (these are scientific claims — exactly what adversarial review is for);
- fingerprint/batch schema changes (the audit's schema-3 bundle is the first candidate);
- constitutional amendments (an RFC synthesis is the *input* to an amendment; the Constitution's own process governs ratification);
- anything touching the truth layer or oracle algorithm.

Routine implementation work does **not** need an RFC — the packet spec suffices. This keeps the ceremony proportional to a solo-maintainer project.

**Self-containment requirement:** this proposal cites `ledgr/inst/design/` as evidence, but the adopted process must not depend on another repository. Batch 0 (Section 6) authors the adapted artifacts *in this repo*: `inst/design/rfc_cycle.md` (adapted, with CausalStress-specific RFC triggers), the packet templates (skeleton `spec.md` / `tickets.md` / `tickets.yml` / `batch_plan.md` / `release_closeout.md`) in a **canonical, non-archival location — `inst/design/templates/`** — referenced from the authority README, and the authority README itself. Templates must not live inside a packet directory: once that packet closes it becomes a historical record, and the canonical template would be buried in an archive. After Batch 0, a future agent must be able to operate the full governance contract without ever opening the ledgr repo; ledgr references remain as historical rationale only.

### 5.5 The release gate (`release_gate.md`)
A release closes only when:
1. `R CMD check` passes with no ERRORs/WARNINGs (currently impossible — audit C5, M17 — which is itself evidence this gate is needed).
2. A named **validation suite** passes — three distinct surfaces, each fixed in the v0.1.10 packet before this gate item can bind (gating on the current validators would certify exactly the drift they are meant to catch):
   a. **Registry metadata / sidecar validation** (`cs_validate_dgp_registry(strict = TRUE)`, enhanced per M10): semver, status invariants, rationale presence, sidecar noise/effect *and* version/status claims compared per registered version.
   b. **Version-aware executable validation**: every registered `(dgp_id, version)` generator is executed and contract-checked — not first-match-per-id (M6).
   c. **Public DGP certification** (`cs_validate_dgp()`): contract tests including the negative case — missing potential outcomes must fail certification (M19).
3. The full test suite passes, including the contract tests of 5.3.
4. Every acceptance criterion in the packet spec is checked off with evidence.
5. Every finding in every open audit is routed (ticketed / deferred-with-rationale / rejected-with-rationale) — the disposition rule already stated at the top of the v0.1.9 audit.
6. `inst/design/README.md` reflects the new planning state; the packet gains its `release_closeout.md`.
7. No constitutional violation is known-open. (A violation may be explicitly *deferred* only by constitutional amendment, not by ticket.)

### 5.6 `contracts.md`
A single operational index restating, with pointers to Constitution articles and code:
DGP output contract (synthetic + real), estimator contract (incl. `ci_type`, threads, the oracle-access mechanism once M1 is fixed), runner output contract (success semantics, n_boot_ok), fingerprint schemas (legacy v0.1.7, schema-2, schema-3 when cut) and their resume-comparison rules, pin naming, batch artifact schema, staging/consolidation protocol, RNG mandate surface (which entry points set it, what is restored). Where the v0.3.0 design and the metadata spec conflict today, `contracts.md` records the single winner. This document is what every agent reads before touching the runner — it replaces re-deriving the contract from four partially-stale specs.

---

## 6. Concrete next steps (proposed v0.1.10 arc)

1. **Cut `causalstress_v0_1_10_spec_packet/`** seeded from the v0.1.9 audit **Rev 2** fix-order table (3 critical, 19 major after adversarial verification):
   - **Batch 0 (mandatory, first — governance bootstrap):** author `inst/design/README.md` (authority index), `contracts.md`, the adapted `rfc_cycle.md`, and the packet templates (the self-containment requirement of 5.4). This is not optional and does not run last: if Batches 1–3 execute before the authority index exists, every agent in those batches is still operating in the no-authority-index regime this proposal exists to end. Precedent: ledgr's v0.1.8.00 governance-only prep packet.
   - **C4 disposition at packet cut:** the maintainer chooses one of three options *when the packet is cut* — extract `fit$estimates$ATT` under the existing id; relabel to `tmle_ate` and exclude from ATT scoring; or drop the estimator. This is a one-time product decision, not an RFC and not coupled to the M1/M9 constitutional questions; recording it in the packet spec takes a sentence. The implementation (one line either way) then lands in Batch 1, honoring the audit's fix-order priority 1.
   - **Batch 1 (mechanical, high-yield):** C4 (per the disposition above), C1 (`<<-` + consolidator count reconciliation), C5 (DESCRIPTION), M6 (validator first-match), M19 (validator vacuous pass) + the contract tests of 5.3.
   - **Batch 2 (one coordinated design change):** fingerprint **schema-3** + RNG isolation = C2, C3, M3, M7, M12 — preceded by a short RFC (the first RFC-cycle candidate: it touches resume semantics and Art. VII provenance).
   - **Batch 3 (constitutionally gated fixes):** M1 and M9, once their constitutional questions (step 2 below) are decided via the RFC → amendment path.
   - **Batch 4 (governance conformance):** M2, M4, M5, M10 (validation suite per release-gate item 2), M11, M13, M15, M16, M18.
   - **Batch 5 (cleanup):** minors (incl. downgraded M17 and the ex-M14 tau-NA reporting item), design-doc corrections (D1–D4), `archive/` migration.
   - Non-scope: new DGPs, new estimators, sensitivity analysis, Python spoke. (M14 is not routed anywhere: it was refuted in audit Rev 2.)
2. **Decide the two constitutional questions the audit raised** (via the RFC → amendment path), gating Batch 3:
   - Art. II bitwise scope vs. BLAS reality (M9);
   - the oracle-access mechanism: registry-flag vs. the Constitution's `config$use_true_propensity` wording (M1) — either the code gains the config mechanism or the Constitution is amended to bless the registry flag with column-scoped grants.
3. **Adopt the maintenance rule immediately:** from this commit forward, design-document changes update the README index in the same change.
4. **Adopt the adversarial-verification rule:** an audit becomes a valid packet input only after an independent adversarial pass (a different agent than the auditor) has confirmed/refuted its findings — the v0.1.9 audit's own Rev 2 history (one finding killed, two blast radii corrected, one missed MAJOR found) is the demonstration that this step pays for itself.

---

## 7. Risks and mitigations

| Risk | Mitigation |
| --- | --- |
| Process overhead overwhelms a solo-maintainer project | RFCs only for contested/irreversible decisions (5.4); packets can be small (ledgr shipped one-week packets); tickets.yml lets agents carry the bookkeeping |
| Archive move breaks links/muscle memory | `archive/` keeps filenames verbatim; README maps old → new; one-time cost |
| Constitution + contracts.md drift apart | contracts.md entries carry article pointers; release gate item 7; any contradiction is resolved in favor of the Constitution by definition |
| Packets become stale mid-flight like the v0.3.0 design did | A packet is *active* only between cut and release gate; the README's planning-state section makes "what is active now" a one-line lookup; historical packets are records by rule |
| LLM agents treat old packets as instructions | The README rule is stated where agents start reading; ledgr's experience shows the explicit "records, not instructions" sentence is load-bearing |

---

## 8. Summary

Adopt ledgr's governance skeleton — authority-leveled README index, versioned spec packets with machine-readable tickets and batch plans, an RFC cycle for contested decisions, audit-routing discipline, horizon parking lot, and a hard release gate — while keeping the CausalStress Constitution as the supreme, separately-amended authority that no packet or RFC can override. Bootstrap the governance artifacts first (mandatory Batch 0: README, `contracts.md`, in-repo process templates — self-contained, no ledgr dependency), archive the stale design corpus, and cut the first packet (v0.1.10) from the v0.1.9 audit **Rev 2** — with the fingerprint schema-3 change as the first RFC, C4's estimand disposition as the first maintainer decision, and adversarial verification of audits adopted as a standing pre-ticket-cut rule.
