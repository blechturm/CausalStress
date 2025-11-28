# **CausalStress Constitution (Condensed Edition)**

### *Why these rules exist, and what they protect.*

CausalStress is not just an R package.\
It is a **scientific instrument** for comparing causal inference methods.\
Like a scale or a thermometer, it only works if we all use it **the same way**.

This Constitution explains, in plain language, the core principles that keep the framework mathematically valid, reproducible, and fair.

# **1. What counts as “truth” (and why we define it so strictly)**

### **Rule:**

Synthetic datasets must separate the **true effect** (signal) from the **random noise** we add.

### **Why:**

If truth included random noise, different seeds or sample sizes would change the “right answer,” making method comparisons meaningless.

### **How:**

-    *Structural truth* (the true ATT) is calculated only from the noise-free treatment effect τ(X).

-    *Distributional truth* (QST) uses the full noisy outcomes but is computed once via a large Monte Carlo oracle so every estimator compares to the **same** target.

**This ensures all methods compete against the same scoreboard.**

# **2. Data Generating Processes must be frozen and reproducible**

### **Rule:**

Once a DGP is released, its behavior can never change.\
Parameters, formulas, noise distributions, dimensionality — all fixed forever.

### **Why:**

If the data changes, historical benchmarks break, papers become incomparable, and gains/losses in estimator performance become meaningless.

### **How:**

-    DGPs accept a `seed` and must call `set.seed()` inside.

-   Changing the seed changes only random draws, not parameters.

-   DGPs cannot read external state (files, system time, RNGkind, locale, BLAS threads).

-   If anything must change, it becomes a new version or a new ID.

**This ensures reproducibility across machines, years, and contributors.**

# **3. Standard interfaces for DGPs and estimators**

### **Rule:**

All estimators follow the same function signature.\
All DGPs return the same fields.

### **Why:**

Without a strict interface, running 10,000 experiments across dozens of estimators becomes chaotic and error-prone.

### **How (Estimators):**

Every estimator gets `(df, tau, config)` and returns:

-    `att`

-    optional `qst`

-    `meta`

**Importantly:** Estimators are forbidden from using y0, y1, or true propensity p in the DGP output unless explicitly marked “Oracle”.

### **How (DGPs):**

Synthetic DGPs must include y0, y1, p, and structural treatment effects.\
Real DGPs must NOT include them.

**This ensures fairness (no cheating) and interoperability.**

# **4. Placebos protect us from hallucinated effects**

### **Rule:**

A special family of DGPs sets Y1 = Y0 exactly.\
The true effect is zero. Always.

### **Why:**

If a method produces non-zero effects on placebos, it is untrustworthy.\
This catches:

-    overfitting

-    sensitivity to model misspecification

-    noise-induced bias

-    “variance hallucination” problems

### **How:**

-    Y1 is literally a copy of Y0 (pathwise identity).

-    QST truth is exactly zero for all quantiles.

-    Estimators must not reject zero \>10% of the time at 5% significance.

**This ensures methods don’t find effects when none exist.**

# **5. Concurrency and computational safety**

### **Rule:**

Parallelism must be controlled:

-    Runner parallelizes **across** runs (“wide”).

-    Estimators must stay **within** their thread budget (“shallow”).

### **Why:**

Uncontrolled threading creates race conditions, crashes, and nondeterministic results.

### **How:**

If the runner sets `config$num_threads = 1`, estimators must obey it.

**This ensures stable large-scale benchmarking without thread chaos.**

# **6. Persistence and data integrity**

### **Rule:**

Each run (Estimator × DGP × Seed) gets its own save file.

### **Why:**

Large benchmarking campaigns (thousands of runs) crash sometimes.\
You must never lose 90% of your progress because one run failed.

### **How:**

-   Results are pinned after every single run.

-    Aggregation happens only after all atomic runs are stored safely.

**This ensures fault-tolerance and reproducibility.**

# **Why these rules matter**

### Because causal inference is fragile.

Small violations, using the wrong seed, silently changing a DGP, using hidden covariates, create irreproducible results and false conclusions.

### Because benchmarking must be scientific.

Two researchers in different countries should get the **same answer** from the same estimator on the same DGP, down to the decimal.

### Because the field lacks standards.

CausalStress aims to become the **baseline**, the **test suite**, the **stress harness** everyone uses to validate new estimators, like ImageNet, GLUE, or MMLU for their fields.

### Because loose standards lead to bad methods.

The Placebo Suite prevents unsafe estimators from being used in real research or policy.

# **Summary: The Constitution in a sentence**

> **CausalStress is governed by strict, frozen, reproducible, interoperable, and fairness-preserving rules to ensure trustworthy causal benchmarking across time, estimators, and contributors.**