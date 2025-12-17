test_that("est_lm_att returns a valid estimator object on synth_baseline", {
  dgp_desc <- cs_get_dgp("synth_baseline")
  dgp      <- dgp_desc$generator[[1]](n = 500, seed = 123L)

  df <- dgp$df

  est <- est_lm_att(
    df     = df,
    tau    = cs_tau_oracle,
    config = list(seed = 1, ci_method = "none")
  )

  expect_invisible(cs_check_estimator_output(est, require_qst = FALSE))

  expect_true(is.list(est$att))
  expect_true(is.numeric(est$att$estimate))
  expect_length(est$att$estimate, 1L)
  expect_false(isTRUE(est$meta$oracle))
})

test_that("est_lm_att errors if y/w are missing", {
  df_bad <- tibble::tibble(
    X1 = rnorm(20),
    X2 = rnorm(20)
  )

  expect_error(
    est_lm_att(df_bad),
    class = "causalstress_estimator_error"
  )
})

test_that("est_lm_att errors if no X covariates are present", {
  df_bad <- tibble::tibble(
    y = rnorm(20),
    w = rbinom(20, size = 1, prob = 0.5)
  )

  expect_error(
    est_lm_att(df_bad),
    class = "causalstress_estimator_error"
  )
})
