test_that("cs_plan_campaign is deterministic for a fixed seed", {
  plan_a <- cs_plan_campaign(
    dgp_list = c("dgp_a", "dgp_b"),
    estimator_list = c("est_a", "est_b"),
    n_seeds = 1:4,
    batch_size = 3L,
    campaign_seed = 42L,
    strategy_map = list(
      defaults = list(ci_method = "bootstrap", n_boot = 100),
      overrides = list(est_b = list(ci_method = "native"))
    )
  )
  plan_b <- cs_plan_campaign(
    dgp_list = c("dgp_a", "dgp_b"),
    estimator_list = c("est_a", "est_b"),
    n_seeds = 1:4,
    batch_size = 3L,
    campaign_seed = 42L,
    strategy_map = list(
      defaults = list(ci_method = "bootstrap", n_boot = 100),
      overrides = list(est_b = list(ci_method = "native"))
    )
  )
  expect_identical(plan_a, plan_b)
})

test_that("cs_plan_campaign applies global shuffle", {
  base_grid <- tidyr::expand_grid(
    dgp_id = c("dgp_a", "dgp_b"),
    estimator_id = c("est_a", "est_b"),
    seed = 1:6
  )
  plan <- cs_plan_campaign(
    dgp_list = c("dgp_a", "dgp_b"),
    estimator_list = c("est_a", "est_b"),
    n_seeds = 1:6,
    batch_size = 4L,
    campaign_seed = 123L,
    strategy_map = list()
  )
  plan_tasks <- tidyr::unnest(plan, cols = c(tasks))
  expect_false(
    identical(
      plan_tasks[, c("dgp_id", "estimator_id", "seed")],
      base_grid[, c("dgp_id", "estimator_id", "seed")]
    )
  )
})

test_that("cs_plan_campaign applies per-estimator overrides", {
  plan <- cs_plan_campaign(
    dgp_list = c("dgp_a"),
    estimator_list = c("est_a", "est_b"),
    n_seeds = 1:2,
    batch_size = 10L,
    campaign_seed = 7L,
    strategy_map = list(
      defaults = list(ci_method = "bootstrap", n_boot = 200),
      overrides = list(est_b = list(ci_method = "native"))
    )
  )
  tasks <- tidyr::unnest(plan, cols = c(tasks))
  cfg_a <- tasks$task_config[tasks$estimator_id == "est_a"]
  cfg_b <- tasks$task_config[tasks$estimator_id == "est_b"]

  expect_true(all(vapply(cfg_a, function(x) x$ci_method, character(1)) == "bootstrap"))
  expect_true(all(vapply(cfg_b, function(x) x$ci_method, character(1)) == "native"))
  expect_true(all(vapply(cfg_b, function(x) x$n_boot, numeric(1)) == 200))
})
