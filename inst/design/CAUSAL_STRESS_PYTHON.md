# **8.1 The “Hub & Spoke” Architecture (v0.4.0) — *Deferred***

**Status:** Deferred (Target Release: v0.4.0)\
**Goal:** Enable universal benchmarking from Python, Julia, Rust, and other ecosystems through a strict data-exchange protocol, while *preserving Constitutional authority exclusively inside R*.

------------------------------------------------------------------------

## **8.1.1 Architectural Philosophy**

CausalStress adopts a **Hub & Spoke** model that separates *generation* and *governance* (Hub) from *consumption* (Spokes):

-   **The Hub (R):**\
    The sole source of truth.

    -   Generates DGP draws.\
    -   Computes ATT / QST truth.\
    -   Performs Airlock sanitization.\
    -   Executes the official Gatekeeper validation.\
    -   Controls versioning and provenance.

-   **The Protocol (Parquet / Arrow):**\
    A language-agnostic interface for exchanging sanitized data and (optionally) summary truth tables.

-   **The Spokes (Python/Julia/Rust Clients):**\
    Lightweight libraries that load exported tasks, run local estimators, and optionally perform *local convenience checks*.\
    Spokes **cannot certify correctness**; only the Hub may issue “Pass / Fail” verdicts.

------------------------------------------------------------------------

## **8.1.2 Exporting a Challenge: `cs_export_challenge()`**

The export function creates challenge folders using a **strict two-mode system** that prevents Oracle-overfitting:

### **Modes**

-   **`open_book` (Educational)**\
    Exports:

    -   Sanitized data (public view)\
    -   Summary truth (scalar ATT + QST table)

    Intended for local exploration, tutorials, and estimator debugging.

-   **`blind` (Official Benchmark)**\
    Exports:

    -   Sanitized data only\
        Truth is *not* exported.\
        The Hub holds the internal truth registry.

------------------------------------------------------------------------

### **Function Specification**

\`\`\`r cs_export_challenge \<- function( suite_id, path = "challenge_export", mode = c("open_book", "blind"), \# Strict mode separation seeds = 1:50, N = 2000 ) { mode \<- match.arg(mode) fs::dir_create(path)

\# Write manifest for provenance write_manifest(path, suite_id, seeds, N, mode)

\# Iterate over DGPs in the suite purrr::walk(cs_get_suite(suite_id), function(dgp_id) { purrr::walk(seeds, function(seed) {

```         
  # 1. Generate DGP object (df_full + df_public + truth)
  dgp <- get_dgp_registry()[[dgp_id]](N = N, seed = seed)

  # 2. Export sanitized data ("The Question")
  # Schema: row_id, y, w, x1...xk (NO y0, y1, p, structural_te, or attributes)
  arrow::write_parquet(
    dgp$df_public,
    glue::glue("{path}/{dgp_id}_seed{seed}_data.parquet")
  )

  # 3. Export truth summary ("The Answer Key") – only in open_book mode
  if (mode == "open_book") {
    truth_summary <- tibble::tibble(
      dgp_id   = dgp_id,
      seed     = seed,
      true_att = dgp$true_att,
      true_qst = list(dgp$true_qst)   # Nested: tau → qst_true
    )

    arrow::write_parquet(
      truth_summary,
      glue::glue("{path}/{dgp_id}_seed{seed}_truth.parquet")
    )
  }
})
```

}) } 8.1.3 Manifest Contract Every export directory must contain a machine-readable manifest describing provenance, versions, and reproducibility constraints.

manifest.json Example:

json Copy code { "suite_id": "heavytail", "mode": "open_book", "causalstress_version": "0.4.0", "dgp_registry_hash": "a1b2c3d4", "N": 2000, "seeds": \[1,2,3,4,5\], "exported_at": "2025-11-28T14:30:00Z" } dgp_registry_hash is a content hash (e.g., SHA-256) of the DGP registry to guarantee immutability.

seeds explicitly lists all seeds for verification (the count alone is insufficient).

8.1.4 Python Client: causalstress-client The Python client is a Convenience Shim, NOT a certifying authority. Its job is to:

Load data and truth (if present)

Align features

Provide local bias/coverage checks

Prevent accidental truth misuse in blind mode

Python API (Illustrative) python Copy code class Task: def **init**(self, data_path, truth_path=None): self.train = pd.read_parquet(data_path) self.truth = pd.read_parquet(truth_path) if truth_path else None

```         
def evaluate(self, estimate, ci_lower=None, ci_upper=None):
    """
    LOCAL check only.
    Passing this check does NOT guarantee certification.
    Only the Hub can validate Placebo/Gatekeeper tests.
    """
    if self.truth is None:
        raise RuntimeError("Cannot evaluate in blind mode.")

    true_att = self.truth["true_att"].iloc[0]
    bias = estimate - true_att

    covered = None
    if ci_lower is not None:
        covered = (ci_lower <= true_att <= ci_upper)

    return {
      "bias": bias,
      "covered": covered
    }
```

The client performs local debugging, but official certification must be done in R.

8.1.5 Official Benchmark Workflow (Blind Mode) 1. Export (Hub, R) r Copy code cs_export_challenge(suite_id, mode = "blind") Produces:

{dgp}\_seed{n}\_data.parquet

manifest.json

2.  Execute (Spoke, Python/Julia/Rust) Participants train estimators and generate a standardized submission file.

3.  Submission Participants submit:

Copy code submission.parquet Minimum required schema:

dgp_id

seed

estimator_id

att_hat

ci_lower

ci_upper

(QST fields may be added in later versions.)

4.  Certification (Hub, R) Using the internal Truth Registry (never exported):

Join submission with truth

Run Gatekeeper tests (e.g., placebo correctness, CI coverage)

Issue official verdict (pass, fail_gatekeeper, fail_coverage, etc.)

Only the Hub may perform certification.

8.1.6 Summary of Guarantees Authority: Only R / the Hub can issue official benchmark results.

Safety: No Oracle leakage in blind mode.

Universality: Any language can participate via Parquet.

Traceability: Every challenge is version-locked with a manifest and registry hash.

Extensibility: Additional languages (Julia, Rust, C++) inherit the same protocol.