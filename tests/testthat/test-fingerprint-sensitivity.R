test_that("fingerprints are identical for identical inputs", {
  fp1 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = TRUE, B = 10, oracle = FALSE, estimator_version = "1.0",
    config = list(num_trees = 100, alpha = 0.1),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  fp2 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = TRUE, B = 10, oracle = FALSE, estimator_version = "1.0",
    config = list(alpha = 0.1, num_trees = 100),  # different order
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  expect_identical(fp1, fp2)
})

test_that("fingerprints differ when config changes", {
  fp1 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = TRUE, B = 10, oracle = FALSE, estimator_version = "1.0",
    config = list(num_trees = 100),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  fp2 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = TRUE, B = 10, oracle = FALSE, estimator_version = "1.0",
    config = list(num_trees = 500),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  expect_false(identical(fp1, fp2))
})

test_that("fingerprints differ when tau changes", {
  fp1 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = TRUE, B = 10, oracle = FALSE, estimator_version = "1.0",
    config = list(num_trees = 100),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  fp2 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = TRUE, B = 10, oracle = FALSE, estimator_version = "1.0",
    config = list(num_trees = 100),
    tau = c(0.1, 0.9),
    max_runtime = Inf
  )
  expect_false(identical(fp1, fp2))
})

test_that("schema-3 fingerprints include dgp_version", {
  fp1 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = FALSE, B = 0, oracle = FALSE, estimator_version = "1.0",
    dgp_version = "1.0.0",
    config = list(),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  fp2 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = FALSE, B = 0, oracle = FALSE, estimator_version = "1.0",
    dgp_version = "2.0.0",
    config = list(),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  expect_false(identical(fp1, fp2))
})

test_that("schema-3 CI intent uses pre-injection config and B=0 edge", {
  expect_identical(cs_ci_intent(list(), bootstrap = TRUE, B = 0L), "none")
  expect_identical(cs_ci_intent(list(), bootstrap = TRUE, B = 10L), "bootstrap")
  expect_identical(cs_ci_intent(list(ci_method = "native"), bootstrap = TRUE, B = 10L), "native")
})

test_that("schema-3 strips runner bookkeeping fields from config payload", {
  fp1 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = FALSE, B = 0, oracle = FALSE, estimator_version = "1.0",
    dgp_version = "1.0.0",
    config = list(alpha = 0.1),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  fp2 <- cs_build_config_fingerprint(
    dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
    bootstrap = FALSE, B = 0, oracle = FALSE, estimator_version = "1.0",
    dgp_version = "1.0.0",
    config = list(
      alpha = 0.1,
      seed = 1L,
      ci_method = "none",
      ci_method_source = "runner_none",
      estimator_id = "e1",
      num_threads = 1L
    ),
    tau = c(0.1, 0.5),
    max_runtime = Inf
  )
  expect_identical(fp1, fp2)
})

test_that("fingerprint rejects non-serializable config values", {
  expect_error(
    cs_build_config_fingerprint(
      dgp_id = "d1", estimator_id = "e1", n = 100, seed = 1,
      bootstrap = TRUE, B = 10, oracle = FALSE, estimator_version = "1.0",
      config = list(fn = function(x) x),
      tau = c(0.1, 0.5),
      max_runtime = Inf
    ),
    class = "causalstress_fingerprint_error"
  )
})

test_that("skip_existing errors when config changes (fingerprint mismatch)", {
  board <- pins::board_temp()
  pin_name <- CausalStress:::cs_result_pin_name("synth_baseline", "lm_att", 30, 1, "1.6.0")

  cfg1 <- list(alpha = 1)
  cfg2 <- list(alpha = 2)

  cs_run_seeds(
    dgp_id        = "synth_baseline",
    estimator_id  = "lm_att",
    n             = 30,
    seeds         = 1,
    bootstrap     = FALSE,
    config        = cfg1,
    board         = board,
    skip_existing = FALSE,
    parallel      = FALSE,
    show_progress = FALSE
  )
  pin1 <- pins::pin_read(board, pin_name)
  fp1 <- pin1$meta$config_fingerprint

  expect_error(
    cs_run_seeds(
      dgp_id        = "synth_baseline",
      estimator_id  = "lm_att",
      n             = 30,
      seeds         = 1,
      bootstrap     = FALSE,
      config        = cfg2,
      board         = board,
      skip_existing = TRUE,
      parallel      = FALSE,
      show_progress = FALSE
    ),
    "Configuration fingerprint mismatch"
  )

  pin_after <- pins::pin_read(board, pin_name)
  expect_identical(fp1, pin_after$meta$config_fingerprint)
})
