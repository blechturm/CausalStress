**Threat model.**\

CausalStress guards against *accidental* truth leakage and governance drift (e.g. using `y1 - y0`, true `p`, or seed-dependent truths). It does **not** attempt to sandbox R or defend against malicious code.\

The Airlock removes forbidden columns and enforces strict contracts so that honest estimators cannot cheat by mistake but a determined user can always bypass safeguards in R.