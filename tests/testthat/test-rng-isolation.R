test_that("validation preserves absent Random.seed", {
  old_state <- CausalStress:::cs_rng_state_capture()
  on.exit(CausalStress:::cs_rng_state_restore(old_state), add = TRUE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    remove(list = ".Random.seed", envir = .GlobalEnv)
  }

  cs_validate_dgp_registry(strict = FALSE)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("validation preserves existing RNGkind and seed", {
  old_state <- CausalStress:::cs_rng_state_capture()
  on.exit(CausalStress:::cs_rng_state_restore(old_state), add = TRUE)

  set.seed(42)
  before_kind <- RNGkind()
  before_seed <- .Random.seed

  cs_validate_dgp_registry(strict = FALSE)

  expect_identical(RNGkind(), before_kind)
  expect_identical(.Random.seed, before_seed)
})

test_that("campaign planning is independent of ambient RNG kind", {
  old_state <- CausalStress:::cs_rng_state_capture()
  on.exit(CausalStress:::cs_rng_state_restore(old_state), add = TRUE)

  suppressWarnings(RNGkind("Mersenne-Twister", "Inversion", "Rejection"))
  set.seed(1)
  plan_rejection <- cs_plan_campaign(
    dgp_list = c("synth_baseline", "synth_heavytail"),
    estimator_list = c("lm_att", "ipw_att"),
    n_seeds = 1:4,
    batch_size = 3L,
    campaign_seed = 99L,
    strategy_map = list(defaults = list(n = 50, ci_method = "none"))
  )

  suppressWarnings(RNGkind("Mersenne-Twister", "Inversion", "Rounding"))
  set.seed(1)
  plan_rounding <- cs_plan_campaign(
    dgp_list = c("synth_baseline", "synth_heavytail"),
    estimator_list = c("lm_att", "ipw_att"),
    n_seeds = 1:4,
    batch_size = 3L,
    campaign_seed = 99L,
    strategy_map = list(defaults = list(n = 50, ci_method = "none"))
  )

  expect_identical(plan_rejection, plan_rounding)
})
