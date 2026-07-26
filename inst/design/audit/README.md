# Audit Archive

This directory preserves non-binding review evidence. Under the authority order
in `../README.md`, audit findings do not authorize implementation until they are
routed through an accepted RFC or an active spec packet.

## Scientific-design audit

The root-level `phase0-triage.md`, `phase1-claims/`, `phase2-reports/`,
`phase3-rebuttals/`, `synthesis.md`, `meta-review.md`, `human-review-packet.md`,
and `checks/` form pass 1. They are retained as provenance, including failed or
deferred probes and conclusions later corrected after reviewers had omitted the
DGP sidecars and scientific narratives.

The authoritative audit disposition is
`pass2/maintainer-adjudication.md`. Read the remaining pass-2 material as the
evidence and deliberation leading to that adjudication. Production changes
accepted from it were routed through CS-1229 and released in v0.2.0; deferred
work remains subject to future planning and governance.

## Other audits

- `code-simplicity-audit.md` is the repository-wide simplicity and
  maintainability review. Its recommendations are advisory until ticketed.
- `v0_1_9_deep_code_review_audit.md` is the historical deep review that fed the
  v0.1.10 and v0.2.0 correction work.
- `governance_spec_packet_proposal.md` is the historical governance proposal
  from which the current packet process was derived.

Captured `*_output.txt` files are evidence from the environment and revision
named by their surrounding audit documents; they are not evergreen test
fixtures. Re-run the corresponding scripts when using a finding against a newer
revision.
