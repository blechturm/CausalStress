---
title: "DGP Dossier: synth_qte1"
output: html_document
params:
  meta: NULL
  dgp_id: "synth_qte1"
---



## 1. Identity & Status

<table>
<tbody>
  <tr>
   <td style="text-align:left;"> DGP ID </td>
   <td style="text-align:left;"> synth_qte1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Version </td>
   <td style="text-align:left;"> 1.3.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Status </td>
   <td style="text-align:left;"> <span class="badge bg-warning">experimental</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Difficulty </td>
   <td style="text-align:left;"> <span style="color: #f1c40f;">★</span><span style="color: #f1c40f;">★</span><span style="color: #f1c40f;">★</span><span style="color: #f1c40f;">★</span><span style="color: #e0e0e0;">★</span> (4/5) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stress Profile </td>
   <td style="text-align:left;"> &nbsp;<span class="badge bg-warning" style="margin-right:4px;">overlap: moderate</span><span class="badge bg-danger" style="margin-right:4px;">noise: heavy</span><span class="badge bg-success" style="margin-right:4px;">linearity: linear</span><span class="badge bg-danger" style="margin-right:4px;">effect: heterogeneous</span><span class="badge bg-secondary" style="margin-right:4px;">target: both</span> </td>
  </tr>
</tbody>
</table>

## 2. What This DGP Stresses (Intent)

This DGP is **conceptually hard** rather than numerically hard: the treatment
effect is sign-switching (+1 or -1) across the population. A mean-ATT estimator
can look excellent (low bias/RMSE for the ATT) while completely missing that
roughly half the population is harmed. The oracle QST curve is the "X-ray" that
reveals the sign flip.

## 3. Identification Assumptions (Explicit)

-   Selection on observables holds.
-   Overlap is moderate.
-   SUTVA holds.

## 4. Mathematical Specification

Outcome (control):
\[
\mu_0(X) = 1 + X_1 + 0.5 X_2
\]

Propensity:
\[
\text{logit}(p(X)) = 0.5 X_1 - 0.5 X_2
\]

Discontinuous effect:
\[
\tau(X) =
\begin{cases}
 +1, & X_1 > 0 \\
 -1, & X_1 \le 0
\end{cases}
\]

Noise:
\[
\varepsilon = 0.5\,U,\quad U \sim t_4
\]

## 5. Oracle Truth Definition

-   True ATT is computed from `structural_te` among treated units.
-   True QST is computed on the oracle tau grid (`cs_get_oracle_qst()`).

## 6. Visual Diagnostics (n = 5000)

![plot of chunk visuals](figure/visuals-1.png)

## 7. Empirical Validation

OLS (`lm_att`) passes the numerical benchmark (low ATT bias/RMSE), but fails the
scientific benchmark: it reports only a mean ATT and provides no distributional
view. The oracle QST curve shows that the treatment is positive in one half of
the population and negative in the other.


Table: Empirical validation (n=1000, seeds=1:10)

|estimator_id | mean_bias|  rmse|
|:------------|---------:|-----:|
|lm_att       |    -0.008| 0.056|
|oracle_att   |     0.000| 0.000|

## 8. Failure Mode Summary

-   Mean-ATT estimators can look excellent while masking that the treatment harms
    a large subpopulation (here, roughly those with \(X_1 \le 0\)).
-   Distributional diagnostics (QST) are mandatory in sign-switching regimes.

## 9. Implementation Reference

-   Generator: `R/dgp-synth-qte1.R`
-   Registry: `cs_dgp_registry()` entry for `synth_qte1`
-   Oracle truth: `cs_get_oracle_qst("synth_qte1", version = "<registry default>")`

## 10. Validation Checklist

- [x] Discontinuous tau(X) specified
- [x] Outcome and propensity formulas specified
- [x] Student-t noise specified (df = 4, scaled by 0.5)
- [x] X1 vs X2 titled "Covariate Overlap (Joint Support)"

## 11. Changelog

- v1.3.0 dossier: Updated to emphasize conceptual blindness trap; removed TMLE narrative; added lm_att + oracle_att validation table.
