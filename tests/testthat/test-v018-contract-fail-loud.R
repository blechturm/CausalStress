test_that("estimator contract violations fail loudly (runner-side)", {
  bad_qst <- function(df, tau = cs_tau_oracle, config = list(), ...) {
    list(
      att = list(estimate = 0),
      qst = tibble::tibble(
        tau = rev(tau),
        estimate = seq_along(tau)
      ),
      meta = list(estimator_id = "bad_qst", oracle = FALSE, supports_qst = TRUE)
    )
  }

  cs_register_estimator(
    estimator_id  = "bad_qst",
    type          = "test",
    generator     = bad_qst,
    oracle        = FALSE,
    supports_qst  = TRUE,
    version       = "0.0.0",
    description   = "Estimator that violates tau order contract",
    source        = "test",
    requires_pkgs = character(0)
  )

  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "bad_qst",
      n            = 50,
      seed         = 1L,
      tau          = c(0.1, 0.5, 0.9)
    ),
    "tau grid",
    ignore.case = TRUE
  )
})

