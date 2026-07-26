# CausalStress RFC Cycle

**Status:** Active process

Use an RFC only for decisions that are contested, scientific, constitutional, or
expensive to reverse. Routine implementation work belongs in the active spec
packet.

## Triggers

Start an RFC for:

- new DGP families or estimand definitions.
- truth-layer or oracle-algorithm changes.
- fingerprint, pin, or batch schema changes.
- constitutional amendments.
- large public API changes that are hard to reverse.

## Flow

1. Seed: one agent or maintainer writes the proposal and decision options.
2. Response: a different reviewer attacks assumptions and alternatives.
3. Optional seed v2: the author incorporates or rejects response points.
4. Synthesis: a maintainer-facing document states the recommended decision,
   rejected alternatives, and open risks.
5. Final review: maintainer accepts, rejects, or asks for another cycle.

Only the accepted synthesis is binding, and only within its stated scope.
Deferred points must be recorded in `horizon.md` or the active packet closeout.

## File Naming

Use `rfc/YYYYMMDD_short_topic_seed.md`, `..._response.md`, and
`..._synthesis.md` unless the active packet specifies a narrower convention.
