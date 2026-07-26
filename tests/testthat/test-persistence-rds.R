test_that("result staging uses RDS and validates existing destinations", {
  staging_dir <- tempfile("cs_result_rds_")
  res <- cs_run_single(
    dgp_id = "synth_placebo_tau0",
    estimator_id = "lm_att",
    n = 30,
    seed = 11,
    bootstrap = FALSE,
    quiet = TRUE,
    config = list(ci_method = "none")
  )

  path <- CausalStress:::cs_stage_result(res, staging_dir)
  expect_match(path, "\\.rds$")
  expect_equal(infoRDS(path)$version, 3L)
  expect_identical(readRDS(path), res)
  expect_identical(CausalStress:::cs_stage_result(res, staging_dir), path)
  expect_length(list.files(staging_dir, pattern = "\\.(tmp|lock)$"), 0L)

  wrong <- res
  wrong$meta$seed <- 12
  saveRDS(wrong, path, version = 3)
  bytes_before <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  expect_error(
    CausalStress:::cs_stage_result(res, staging_dir),
    "wrong logical identity",
    class = "causalstress_staging_error"
  )
  expect_identical(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    bytes_before
  )
})

test_that("gather refuses mixed legacy staging and corrupt RDS", {
  skip_if_not_installed("pins")
  staging_dir <- tempfile("cs_result_legacy_")
  res <- cs_run_single(
    dgp_id = "synth_placebo_tau0",
    estimator_id = "lm_att",
    n = 30,
    seed = 13,
    bootstrap = FALSE,
    quiet = TRUE,
    config = list(ci_method = "none")
  )
  rds_path <- CausalStress:::cs_stage_result(res, staging_dir)
  legacy_path <- file.path(staging_dir, "result__legacy.qs")
  legacy_bytes <- charToRaw("legacy staged result")
  writeBin(legacy_bytes, legacy_path)
  board <- pins::board_folder(file.path(staging_dir, "board"))

  expect_error(
    cs_gather_results(board, staging_dir),
    "Legacy `.qs` staging artifacts",
    class = "causalstress_legacy_persistence_error"
  )
  expect_true(file.exists(rds_path))
  expect_identical(
    readBin(legacy_path, "raw", n = as.integer(file.info(legacy_path)$size)),
    legacy_bytes
  )

  unlink(legacy_path)
  writeBin(charToRaw("partial RDS"), rds_path)
  expect_error(
    cs_gather_results(board, staging_dir),
    "Failed to read RDS artifact",
    class = "causalstress_staging_error"
  )
  expect_true(file.exists(rds_path))
})

test_that("duplicate staged batch ids fail closed", {
  skip_if_not_installed("pins")
  staging_dir <- tempfile("cs_batch_duplicate_")
  plan <- cs_plan_campaign(
    dgp_list = "synth_placebo_tau0",
    estimator_list = "lm_att",
    n_seeds = 21L,
    batch_size = 1,
    campaign_seed = 1,
    strategy_map = list(defaults = list(n = 30, ci_method = "none"))
  )

  cs_run_batch(1L, plan, staging_dir)
  cs_run_batch(1L, plan, staging_dir)
  paths <- list.files(staging_dir, pattern = "\\.rds$", full.names = TRUE)
  expect_length(paths, 2L)
  expect_error(
    cs_consolidate(staging_dir, pins::board_folder(file.path(staging_dir, "board"))),
    "same batch id",
    class = "causalstress_batch_artifact_error"
  )
  expect_true(all(file.exists(paths)))
})

test_that("resume validates staged batch identities against the plan", {
  staging_dir <- tempfile("cs_batch_identity_")
  plan <- cs_plan_campaign(
    dgp_list = "synth_placebo_tau0",
    estimator_list = "lm_att",
    n_seeds = 22L,
    batch_size = 1,
    campaign_seed = 1,
    strategy_map = list(defaults = list(n = 30, ci_method = "none"))
  )

  path <- cs_run_batch(1L, plan, staging_dir)
  batch_obj <- readRDS(path)
  batch_obj$results[[1L]]$meta$task_fingerprint <- "wrong_task"
  saveRDS(batch_obj, path, version = 3)

  expect_error(
    CausalStress:::cs_run_campaign_plan(
      plan,
      staging_dir,
      workers = 1,
      show_progress = FALSE,
      experimental_parallel = TRUE
    ),
    "do not match the campaign plan",
    class = "causalstress_batch_artifact_error"
  )
})
