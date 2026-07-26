# PASS 2 · Phase 3 — Adversarial round (cross-lane)

Orchestrator-run (disclosed limitation, as pass 1). Phase 2 *was* genuinely
isolated this pass; the fresh Phase-5 meta-review is the independent adversary of
record. Attacks target the **strongest** conclusions; each ends in a check,
citation, or concession. Classification: experiment-resolvable / open-factual /
value.

## A1 → the shared headline "metric-regime enforcement is prose-only" (estimands F1 = statistics F1 = dgp F4). Classification: value + open-factual. PARTIAL CONCEDE / DOWNGRADE.
The three lanes agree `metric_invalid_for_regime` is defined but never emitted, so
a naive user can rank a no-mean mean-ATT. **Adversarial counter (steelman of the
design):** (i) only maintainers may define DGPs (Const. §1.2); (ii) ~~every
heavy-tail DGP is `status: experimental`~~ **[struck per meta-review D1: false —
`synth_heavytail` v1.6.0 is `stable`/recommended; only `synth_placebo_heavytail` is
experimental, so this leg does not hold]**; (iii) the narrative explicitly says
"prefer QST/robust" (heavytail.Rmd §2/§8); (iv) the score row *does* carry
`provenance$dgp_noise_family="heavy"`; (v) the Families spec **already** designs
the enforcement (`moment_regime$rmse_valid`, `cs_summarise_stress`) for v0.3.0.
So the honest status is **not "defect"** but "a known guardrail gap whose fix is
already designed and reserved to the families program." **Resolution:** keep the
finding, but graded *"sound with a documented boundary"* — its action is a
VALUE/spec decision (enforce now vs. document-as-families-reserved), not a
correctness repair. This is the correctly-scoped survivor of pass-1's withdrawn
headline, and it must not be re-inflated to "materially misaligned."

## A2 → dgp §2.2 "hd_sparse is SOUND-by-intent". Classification: experiment-resolvable. UPHELD.
Could the constant-shift + shared-noise still mislead a *QST* comparison (flat +1
truth)? Check already answers it (p2_lane2 S3; lane2 §E): ATT truth =1 and QST
truth =+1 at every τ are identical under shared or independent ε, and QST has no
NA — so no scored quantity is affected. The only different quantity is the
paired Y1−Y0, which Const. §1.3 bars from being truth. Attack fails; hd_sparse is
sound-by-intent, with only the Registry §1.2 doc-consistency note (dgp F2).
Pass-1's "materially misaligned" verdict is withdrawn.

## A3 → statistics S2 / dgp §2.1 "heavytail instability is intended, SOUND". Classification: open-factual. UPHELD with a caveat.
Is calling non-convergence "sound" too generous — shouldn't a benchmark that
*scores* a meaningless ATT still be faulted? Concession bound: the DGP and its
truths are sound (S2/S3, three checks agree); the *only* residual is A1's
signaling gap. So "heavytail is sound" is correct at the DGP/truth layer; the
critique lives entirely in A1's metric-regime signaling, not in the DGP. No
double-counting.

## A4 → statistics F2 survivorship "low / deferred". Classification: experiment-resolvable. UPHELD, mild escalation.
The rebuilt probe (n_runs=8, 4 failures, no denominator; error means NA-poisoned)
is a real, now-empirically-demonstrated gap — pass 1 never actually tested it.
Concede it is genuinely *deferred* (horizon.md:535–542) and blocks no current
claim, but it is the second-most-material finding and should be surfaced as a
prerequisite for any future "survivorship-honest kill-plot," not left implicit.

## A5 → dgp F1 "Registry 1.4.0 stale". Classification: experiment-resolvable. UPHELD.
S6 numerically shows four wrong specs (overlap 3→9, hd_sparse p50→100, etc.). A
reader trusting the frozen Registry gets wrong DGP definitions. Uncontested;
documentation fix (bump/supersede the Registry; sidecars are authoritative).

**Net:** no attack overturns a Phase-2 verdict. The one live cross-cutting finding
(A1) is downgraded from "defect" to "designed-and-reserved guardrail gap / value
decision." All caps respected (5 attacks).
