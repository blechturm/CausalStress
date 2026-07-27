# CausalStress v0.2.1 Batch Plan

**Status:** ACTIVE — Batches 0–4 and the Batch 5 Pages mechanism complete after independent review; release gate not started
**Packet:** `causalstress_v0_2_1_spec_packet`
**Authority commit:** `0b20f12`
**Accepted:** 2026-07-26

## Review Protocol

A batch is the implementation and independent-review unit.

- Work one batch at a time and stop when its scoped tickets are complete.
- Run the targeted obligations named in the ticket set.
- Update `v0_2_1_tickets.md`, `tickets.yml`, and this plan together.
- Ask for independent review with an inline prompt before starting the next
  batch.
- Route every finding before advancement. Do not silently broaden a ticket.
- A `complete_after_review` disposition requires actual independent acceptance;
  the implementer does not self-advance it.
- CS-1232 through CS-1236 are individually deferrable to v0.2.2 only through a
  reviewed disposition and a clean removal of any partial implementation.
- F2 schema-4 identity drift and F7 vocabulary drift stop the batch for
  investigation; they are not ordinary deferrals.
- Shared files have entry-level ownership. A ticket may add or update only the
  navigation entry, section, generated target, ignore rule, metadata field, or
  roadmap statement required by its own acceptance criteria and must preserve
  entries owned by earlier tickets. Rewriting another ticket's entry requires
  an explicit routed finding rather than opportunistic cleanup.

## Dependency Flow

1. Batch 0 establishes current governance and the pre-refactor runner safety
   net.
2. Batch 1 performs only the five audited low-risk maintenance changes.
3. Batch 2 establishes the Quarto substrate before migrating any long-form
   source or DGP dossier.
4. Batch 3 writes and reconciles user-facing content on the accepted Quarto
   substrate.
5. Batch 4 validates the combined artifact and receives independent review.
6. Batch 5 follows the release playbook from the reviewed final tree.

F1 production unification and F3 campaign-API redesign are not dependencies and
are not authorized in any batch.

## Batch 0 — Governance and Runner Characterization

- **State:** complete after independent review.
- **Purpose:** Make the active release boundary truthful and add the F1
  characterization net without changing production behavior.
- **Tickets:** CS-1230, CS-1231
- **Required evidence:** Governance diff review; public-path serial/parallel,
  persistence, forwarding, provenance, resume/rejection, and ordering tests;
  empty production diff for CS-1231.
- **Stop condition:** Any runner defect is recorded and routed; it is not fixed
  under characterization authority.
- **Review checkpoint:** Governance/authority correctness and contracts-not-
  internals characterization review.

## Batch 1 — Independently Deferrable Bounded Maintenance

- **State:** complete after independent review.
- **Purpose:** Remove only the five low-risk concepts approved by the simplicity
  audit while keeping identities, vocabularies, public behavior, and defensive
  boundaries fixed.
- **Tickets:** CS-1232, CS-1233, CS-1234, CS-1235, CS-1236
- **Required evidence:** Exact legacy rejection classes and schema-4 identity
  locks; thread-cap restoration; canonical output normalization; tau
  equivalence; exact descriptor/reason invariants; full affected tests.
- **Stop condition:** A ticket that cannot preserve behavior is reverted and
  reviewed for deferral. Any F2 identity or F7 vocabulary drift stops the batch
  for investigation.
- **Review checkpoint:** Review each ticket independently, including confirmation
  that no replacement abstraction or nearby cleanup was introduced.

## Batch 2 — Quarto Platform and Source Migration

- **State:** complete after independent review.
- **Purpose:** Establish a reproducible Quarto/pkgdown build and mechanically
  migrate every current long-form source before substantive new writing.
- **Tickets:** CS-1237, CS-1238, CS-1239
- **Required evidence:** Explicit `Suggests: quarto` and
  `VignetteBuilder: quarto`; pinned/verified CLI and R package on every release
  substrate; full-vignette build/check without ignore flags; Quarto-rendered
  README/four articles/all 12 DGP reports; registry-keyed sidecar validation;
  CS-1229 protected-prose diff; clean worktree.
- **Stop condition:** Any scientific-content, YAML, DGP, truth, RNG, identity,
  status, or protected-prose drift is reverted and investigated.
- **Review checkpoint:** Quarto substrate portability, format-only migration,
  all-12 dossier completeness, and scientific-content preservation review.

## Batch 3 — Canonical Documentation and Reference Truthfulness

- **State:** complete after independent review.
- **Purpose:** Publish the canonical workflow and supported extension guides,
  then reconcile all remaining public documentation with v0.2.0 behavior.
- **Tickets:** CS-1240, CS-1241, CS-1242, CS-1243
- **Required evidence:** Clean-install executable core examples without optional
  estimators; exported-API-only workflow; Airlock-safe estimator registration;
  explicit no-public-DGP-registration statement; current roxygen/Rd; no known
  stale claims; heavy-tail interpretation preserved.
- **Stop condition:** Documentation that requires a public signature, behavior,
  estimand, family, real-data DGP, or campaign-API change is routed out of this
  release.
- **Review checkpoint:** Scientific truthfulness, public contract accuracy,
  example execution, and no-invented-capability review.

## Batch 4 — Integrated Documentation Validation

- **State:** complete after independent review.
- **Purpose:** Validate the combined site and package as one reviewable artifact
  before release-gate work starts.
- **Tickets:** CS-1244
- **Required evidence:** Complete link/index/source-format checks; exactly 12 DGP
  reports; clean site build; focused/full tests; strict registry validation;
  lint and coverage where affected; executable examples; and a clean worktree.
- **Stop condition:** A failed validation is triaged and routed to its owning
  ticket; no test weakening, ignored vignette build, merge/tag, or publication.
- **Review checkpoint:** Integrated documentation-truthfulness and behavior-
  preservation review before Batch 5.

## Batch 5 — Release Gate and Publication

- **State:** Pages publication mechanism complete after independent review;
  release-gate execution and remote publication have not started.
- **Purpose:** Record complete final-tree release evidence and publish only in
  the governed order.
- **Tickets:** CS-1245
- **Required evidence:** Read the release gate and playbook; full-vignette R CMD
  build/check on Windows, WSL, R-release, and R-devel; focused/full tests;
  strict validation; lint; coverage; reproducibility substrate; clean tree;
  closeout; and green branch/main/tag CI in order.
- **Pages boundary:** Automatic branch, pull-request, main, and tag site builds
  remain previews. Pages publication is a manual dispatch at the exact package-
  version tag, only after green tag CI and final maintainer acceptance.
- **Stop condition:** A failed gate is triaged and routed to its owning ticket;
  no test weakening, vignette-ignore flag, premature merge/tag, or external
  publication.
- **Review checkpoint:** Final pre-merge/tag release-gate review followed by
  explicit maintainer authorization and final evidence closeout.
