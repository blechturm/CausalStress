# **CausalStress Constitution (Condensed Edition)**  
### *Why these rules exist, and what they protect.*

CausalStress is not just an R package.  
It is a **scientific instrument** for comparing causal inference methods.  
Like a calibrated scale or thermometer, it only works if every user follows the **same rules**.

These principles explain — in plain language — the rules that keep the system reproducible, comparable, and scientifically defensible.

---

# **1. What counts as “truth” (and why it must be defined so strictly)**

### **Rule:**  
Synthetic datasets must cleanly separate:

- **Structural truth** (the signal), from  
- **Random noise** (the variability)

### **Why:**  
If truth depended on random noise, then each seed or machine would produce a *different* “correct answer,” making comparisons meaningless.

### **How:**  

- **Structural truth (ATT)** is computed only from the deterministic treatment effect τ(X).  
  Noise plays **no role** in defining truth.

- **Distributional truth (QST)** uses the full noisy outcomes, but is computed **once** using a large Monte-Carlo oracle (1,000,000 draws) so every estimator compares to the **same fixed truth**.

**This creates a universal scoreboard that never changes.**

---

# **2. DGPs must be frozen and fully reproducible**

### **Rule:**  
When a DGP is released (ID + version), its behavior is **frozen forever**.

### **Why:**  
Changing a DGP breaks old results, invalidates comparisons, and destroys historical reproducibility.

### **How:**  

- A DGP must accept a `seed` and call `set.seed()` internally.
- Parameters, formulas, and noise distributions can never change.
- DGPs cannot read external state (system time, BLAS threads, locale, options).
- Any change requires a **new version** or a **new DGP ID**.

**Run the same DGP with the same seed in 2025 or 2035 → identical, bit-for-bit.**

### **Governance:**  
Only maintainers may define new DGP IDs.  
Community DGPs must pass constitutional validation.

---

# **3. Standard interfaces protect fairness and interoperability**

### **Rule:**  
All estimators and DGPs follow strict contracts.

### **How (Estimators):**

Every estimator receives `(df, tau, config)` and returns:

- `att`
- optional `qst`
- `meta`

The Runner automatically removes:

- `y0`, `y1`  
- true propensity `p`  
- structural treatment effect `tau(X)`  

unless the estimator is explicitly marked **Oracle**.

This prevents accidental or intentional cheating.

### **How (DGPs):**

- Synthetic DGPs must include `y0`, `y1`, `p`, and structural τ(X).  
- Real DGPs must NOT include them.

**This ensures fairness, consistency, and plug-and-play interoperability.**

---

# **4. Placebos protect us from hallucinated effects**

### **Rule:**  
Placebo DGPs enforce the **Sharp Null**:  
**Y1 is a bitwise identical copy of Y0.**

### **Why:**  
If a method finds a non-zero effect where the true effect is *exactly* zero, it is unsafe.

Placebos reveal:

- overfitting  
- model misspecification  
- unstable weighting  
- “variance hallucination” where noise appears as a treatment effect  

### **Gatekeeper (Aligned with Constitution 1.7.2)**

An estimator **passes** the Gatekeeper if:

- **ATT:**  
  In at least **90%** of runs, the **95% CI** includes zero.

- **QST (the “10/10 Rule”):**  
  A run fails if the CI excludes zero for  
  **more than 10%** of quantiles (>9 of the 99-point grid).  
  The estimator must not fail more than **10%** of runs.

Estimators without CIs are labeled **Unverified**, not penalized.

**This ensures that estimators do not imagine patterns where no effect exists.**

---

# **5. Concurrency and computational safety**

### **Rule:**  
Parallelism follows the **Wide & Shallow** pattern:

- Runners parallelize **across** tasks  
- Estimators must respect the thread budget in `config$num_threads`

### **Why:**  
Uncontrolled threading leads to:

- race conditions  
- nondeterministic results  
- crashes  
- irreproducibility  

### **Current Status:**  
v0.1.x runs **serially**.  
These rules define how parallelism must behave once implemented.

---

# **6. Atomic persistence ensures data integrity**

### **Rule:**  
Each run (DGP × Estimator × Seed) is saved independently.

### **Why:**  
Benchmarking jobs can run for hours or days; crashes must not erase progress.

### **How:**  

- After each run, results are pinned immediately.  
- Aggregation happens only *after* all atomic units are safely stored.  
- Parallel workers may never overwrite or modify existing pins.

**This guarantees fault tolerance and long-run reproducibility.**

---

# **Why these rules matter**

### **Because causal inference is fragile.**  
Small deviations in noise, seeds, or DGP logic can create misleading results.

### **Because benchmarking must be scientific.**  
Two researchers using the same estimator on the same DGP should get **identical numbers**, down to the decimal.

### **Because the field has no standard.**  
CausalStress aims to become the **default test suite** for causal inference — like ImageNet, GLUE, or MMLU in other fields.

### **Because loose rules produce unsafe estimators.**  
The Placebo Suite prevents unreliable methods from being used in real research or policy.

---

# **Summary (One Sentence)**  
> **CausalStress enforces strict, reproducible, fair, and interoperable rules so causal estimators can be evaluated scientifically, consistently, and safely across time and implementations.**
