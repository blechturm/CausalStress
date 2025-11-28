test_that("est_oracle_att matches structural ATT truth on synth_baseline", {
  desc <- cs_get_dgp("synth_baseline")
  dgp  <- desc$generator(n = 500, seed = 123L)

  df <- dgp$df

  est <- est_oracle_att(df, tau = cs_tau_oracle, config = list())

  expect_invisible(cs_check_estimator_output(est, require_qst = FALSE))

  att_structural <- cs_true_att(
    structural_te = df$structural_te,
    w             = df$w
  )

  expect_equal(est$att$estimate, att_structural)
})

test_that("est_oracle_att errors if required columns are missing", {
  df_bad <- tibble::tibble(
    y = rnorm(10),
    w = rbinom(10, size = 1, prob = 0.5)
  )

  expect_error(
    est_oracle_att(df_bad),
    class = "causalstress_estimator_error"
  )
})

test_that("est_oracle_att errors if df is not a data.frame", {
  not_df <- 1:10

  expect_error(
    est_oracle_att(not_df),
    class = "causalstress_estimator_error"
  )
})
