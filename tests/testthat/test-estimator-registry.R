test_that("cs_get_estimator returns a valid descriptor for oracle_att", {
  desc <- cs_get_estimator("oracle_att")

  expect_type(desc, "list")
  expect_equal(desc$estimator_id, "oracle_att")
  expect_true(is.function(desc$fn))
  expect_true(desc$oracle)
  expect_false(desc$supports_qst)
  expect_type(desc$version, "character")
  expect_type(desc$description, "character")

  dgp_desc <- cs_get_dgp("synth_baseline")
  dgp      <- dgp_desc$generator(n = 200, seed = 123L)

  df <- dgp$df

  est <- desc$fn(
    df     = df,
    tau    = cs_tau_oracle,
    config = list()
  )

  expect_invisible(cs_check_estimator_output(est, require_qst = FALSE))

  att_structural <- cs_true_att(
    structural_te = df$structural_te,
    w             = df$w
  )

  expect_equal(est$att$estimate, att_structural)
})

test_that("cs_get_estimator fails for unknown estimator IDs", {
  expect_error(
    cs_get_estimator("does_not_exist"),
    class = "causalstress_registry_error"
  )
})

test_that("cs_estimator_registry contains oracle_att entry", {
  reg <- CausalStress:::cs_estimator_registry()

  expect_true(is.data.frame(reg))
  expect_true(all(c("estimator_id", "fn") %in% names(reg)))
  expect_true("oracle_att" %in% reg$estimator_id)
  expect_true("lm_att" %in% reg$estimator_id)
})
