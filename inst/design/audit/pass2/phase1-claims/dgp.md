# PASS 2 · Phase 1 · Lane DGP — Intent Reconstruction (docs+sidecars only)

## 1. Claim & suite role
Claim (triage): CausalStress *identifies where estimators succeed, degrade, and fail under
controlled synthetic stress, without misleading comparisons.* The suite is that substrate:
12 synthetic DGPs (7 signal-bearing + 5 sharp-null placebos), each isolating **one** declared
pathology axis, with immutable oracle truth (structural ATT on signal `tau(X)` only §1.3; QST via
10⁶ oracle §1.4). A graded 1–5-star battery locating each breakdown; placebos (Art. IV) gate
hallucination. Each sidecar tag + narrative §2 is the **intent baseline** pass-2 tests against.

## 2. Assumptions required
Each DGP isolates its declared `stress_profile`; stars track challenge; oracle target well-posed
for the estimand advertised; implementations match spec, immutable per `(id,version)`;
`ε₀,ε₁` independent (§1.2 l.48) **unless** the DGP declares otherwise (placebos `Y1≡Y0`;
`hd_sparse` `Y1=Y0+1`); placebos give ATT=0, QST≡0.

## 3. Intent baseline — profile = noise/overlap/linearity/effect
| id | profile | ★ | declared intent (narrative §2 / tag) |
|---|---|---|---|
| synth_baseline | gaussian/moderate/linear/linear | 1 | "fails here → fundamentally broken" |
| synth_heavytail | heavy/moderate/linear/linear | 3 | "Penalize estimators that rely on L2 loss"; Reg. "L2 break" |
| synth_qte1 | heavy/moderate/linear/heterogeneous | 4 | "sign-switching (+1/−1)… mean-ATT excellent while half harmed" |
| synth_nonlinear_heteroskedastic | heteroskedastic/moderate/smooth/constant | 3 | "nonlinear confounding + severe heteroskedasticity… OLS fits a plane to a cubic signal" |
| synth_tilt_mild | gaussian/mild/linear/linear | 2 | "Mild overlap tilt… warm-up for severe overlap" |
| synth_overlap_stressed | gaussian/severe/linear/linear | 4 | "Positivity violation… propensity→0/1… unstable weights" |
| synth_hd_sparse_plm | gaussian/moderate/linear/constant | 4 | "Sparsity/regularization in high dimensions… recover a sparse signal without overfitting" |
| synth_placebo_tau0 | gaussian/moderate/linear/constant | 1 | "Calibration Check… respect the Null" |
| synth_placebo_nonlinear | gaussian/moderate/smooth/constant | 2 | "Hallucination Test… mistake structure for signal?" |
| synth_placebo_heavytail | heavy/moderate/linear/constant | 3 | "Infinite Variance Test… sample mean never converges" |
| synth_placebo_tilted | gaussian/moderate/linear/constant | 4 | "Spurious Correlation Trap… Selection Bias vs Effect" |
| synth_placebo_kangschafer | gaussian/moderate/smooth/constant | 5 | "Blindfold Test… Double Robustness when propensity dead wrong?" |

## 4. Mandated intent findings
**(a) synth_heavytail:** §2/§4/§7 make mean-based instability the **INTENDED demonstration** —
Cauchy → "population mean of realized outcomes is undefined, any L2/MSE estimator is not
well-posed" (§4); "variance is infinite… RMSE fluctuates wildly across seeds" (§7).
Non-convergence of a mean-based ATT estimator IS success (confirms triage). Yet the ATT *truth*
stays well-posed (structural, signal-only §5); only mean-based *estimators* are ill-posed → QST
is the right target.

**(b) synth_hd_sparse_plm:** §4 declares `ε∼N(0,1)` (singular), `τ≡1, Y1=Y0+1`; Registry §2 block
agrees ("constant +1 shift"). The **constant +1 shift with shared noise (sd(y1−y0−τ)=0) is the
declared design**, contradicting only the *global* §1.2 l.48 "ε₀,ε₁ independent." Immaterial to
scored truths: structural ATT (=1) and QST (+1 at every τ, marginal) are identical whether ε is
shared or independent; only the realized paired difference (CATE-level, barred by §1.3) differs.
Intent-first → SOUND.

## 5. Divergences (registry drift; sidecars/narratives authoritative)
Registry 1.4.0 stale: overlap `expit(9X1+9X2)` (Reg.§2.6 `3.0`); nonlinear_het cubic `μ0`,
`σ=0.1+exp(0.5X2)` (Reg.§2.4 sin,`0.3+0.2|X3|`); hd_sparse p=100/ρ=0.95 (Reg.§2 p=50/0.5).
