test_that("cs_plan_campaign is deterministic for a fixed seed", {
  plan_a <- cs_plan_campaign(
    dgp_list = c("synth_baseline", "synth_heavytail"),
    estimator_list = c("lm_att", "ipw_att"),
    n_seeds = 1:4,
    batch_size = 3L,
    campaign_seed = 42L,
    strategy_map = list(
      defaults = list(n = 50, ci_method = "bootstrap", n_boot = 100),
      overrides = list(ipw_att = list(ci_method = "native"))
    )
  )
  plan_b <- cs_plan_campaign(
    dgp_list = c("synth_baseline", "synth_heavytail"),
    estimator_list = c("lm_att", "ipw_att"),
    n_seeds = 1:4,
    batch_size = 3L,
    campaign_seed = 42L,
    strategy_map = list(
      defaults = list(n = 50, ci_method = "bootstrap", n_boot = 100),
      overrides = list(ipw_att = list(ci_method = "native"))
    )
  )
  expect_identical(plan_a, plan_b)
})

test_that("cs_plan_campaign applies global shuffle", {
  base_grid <- tidyr::expand_grid(
    dgp_id = c("synth_baseline", "synth_heavytail"),
    estimator_id = c("lm_att", "ipw_att"),
    seed = 1:6
  )
  plan <- cs_plan_campaign(
    dgp_list = c("synth_baseline", "synth_heavytail"),
    estimator_list = c("lm_att", "ipw_att"),
    n_seeds = 1:6,
    batch_size = 4L,
    campaign_seed = 123L,
    strategy_map = list(defaults = list(n = 50))
  )
  plan_tasks <- tidyr::unnest(plan, cols = c(tasks))
  expect_false(
    identical(
      plan_tasks[, c("dgp_id", "estimator_id", "seed")],
      base_grid[, c("dgp_id", "estimator_id", "seed")]
    )
  )
  expect_true(all(plan_tasks$config_fingerprint_schema == 3L))
  expect_true(all(plan_tasks$fingerprint_version == 3L))
  expect_true(all(c("dgp_version", "estimator_version", "task_fingerprint") %in% names(plan_tasks)))
})

test_that("cs_plan_campaign applies per-estimator overrides", {
  plan <- cs_plan_campaign(
    dgp_list = c("synth_baseline"),
    estimator_list = c("lm_att", "ipw_att"),
    n_seeds = 1:2,
    batch_size = 10L,
    campaign_seed = 7L,
    strategy_map = list(
      defaults = list(n = 50, ci_method = "bootstrap", n_boot = 200),
      overrides = list(ipw_att = list(ci_method = "native"))
    )
  )
  tasks <- tidyr::unnest(plan, cols = c(tasks))
  cfg_a <- tasks$task_config[tasks$estimator_id == "lm_att"]
  cfg_b <- tasks$task_config[tasks$estimator_id == "ipw_att"]

  expect_true(all(vapply(cfg_a, function(x) x$ci_method, character(1)) == "bootstrap"))
  expect_true(all(vapply(cfg_b, function(x) x$ci_method, character(1)) == "native"))
  expect_true(all(vapply(cfg_b, function(x) x$n_boot, numeric(1)) == 200))
})
