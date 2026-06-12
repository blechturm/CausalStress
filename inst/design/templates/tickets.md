# <Version> Tickets

**Status:** DRAFT
**Packet:** `<packet_directory>`

## Ticket Template

### `<ticket_id>` - `<title>`

- **Batch:** `<batch>`
- **Audit/RFC source:** `<source>`
- **Motivation:** `<why this must change>`
- **Files:** `<expected files>`
- **Constitutional check:** `<article and invariant, or "none">`
- **Test obligation:** `<contract-level test through public/default path>`
- **Review gate:** `<reviewer or checkpoint>`
- **Disposition:** one of the packet's allowed disposition values, for example
  `open`, `open_decision_required`, `open_rfc_or_amendment_required`,
  `blocked_by_<ticket_id>`, `implementation_complete_awaiting_review`,
  `complete_after_review`, `deferred`, or `rejected`.

## Tickets

Add release tickets below.

## Release-Gate Ticket Requirement

The final packet must include a release-gate ticket that names
`inst/design/release_gate.md` as a source, requires reading it before gate work,
and records gate evidence in `release_closeout.md`.
