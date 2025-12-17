testthat::test_that("GenGC bootstrap returns QST CI bands", {
  testthat::skip_if_not_installed("GenGC")

  dgp <- CausalStress::dgp_synth_baseline(n = 200, seed = 1L)

  res <- CausalStress::est_gengc(
    df = dgp$df,
    tau = CausalStress::cs_tau_oracle,
    config = list(
      ci_method = "bootstrap",
      n_boot = 10,
      seed = 123L,
      # keep fast
      n_draws = 50,
      num_trees = 100,
      num_threads = 1L,
      dgp_id = "synth_baseline"
    )
  )

  testthat::expect_true(is.data.frame(res$qst))
  testthat::expect_true(all(c("tau", "value", "qst_ci_lo", "qst_ci_hi") %in% names(res$qst)))

  ok <- is.finite(res$qst$qst_ci_lo) & is.finite(res$qst$qst_ci_hi) & (res$qst$qst_ci_lo <= res$qst$qst_ci_hi)
  testthat::expect_true(all(ok))

  testthat::expect_identical(res$meta$qst_ci_method, "bootstrap")
  testthat::expect_identical(res$meta$qst_ci_type, "percentile")
  testthat::expect_true(is.numeric(res$meta$qst_ci_level))
  testthat::expect_length(res$meta$qst_ci_valid_by_dim, nrow(res$qst))
})

