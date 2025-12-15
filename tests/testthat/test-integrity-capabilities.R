test_that("estimator registry matches estimator outputs", {
  reg <- cs_estimator_registry()

  dgp <- dgp_synth_baseline(n = 100, seed = 1)

  for (i in seq_len(nrow(reg))) {
    est <- reg[i, ]
    required <- est$requires_pkgs[[1]]
    if (length(required) > 0) {
      for (pkg in required) {
        skip_if_not_installed(pkg)
      }
    }

    df_run <- cs_airlock(dgp$df, oracle_allowed = isTRUE(est$oracle))

    res <- est$generator[[1]](
      df = df_run,
      config = list(),
      tau = cs_tau_oracle
    )

    has_qst <- !is.null(res$qst)
    expect_identical(has_qst, est$supports_qst)
    expect_identical(res$meta$oracle, est$oracle)
    expect_identical(res$meta$estimator_id, est$estimator_id)
  }
})
