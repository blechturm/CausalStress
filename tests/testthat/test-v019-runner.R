test_that("cs_run_campaign resumes based on staged batches", {
  staging_dir <- tempfile("cs_v019_runner_resume_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  plan <- cs_plan_campaign(
    dgp_list = "synth_baseline",
    estimator_list = "lm_att",
    n_seeds = 1:2,
    batch_size = 1,
    campaign_seed = 1,
    strategy_map = list(defaults = list(n = 100))
  )

  cs_run_batch(batch_id = 1L, plan = plan, staging_dir = staging_dir)

  expect_warning(
    cs_run_campaign(
      plan = plan,
      staging_dir = staging_dir,
      workers = 1,
      experimental_parallel = TRUE
    ),
    class = "causalstress_experimental_parallel"
  )

  files <- list.files(staging_dir, pattern = "^batch_[0-9]+", full.names = FALSE)
  batch1 <- files[grepl("^batch_1", files)]
  batch2 <- files[grepl("^batch_2", files)]

  expect_equal(length(batch1), 1L)
  expect_true(length(batch2) >= 1L)
})

test_that("cs_run_campaign refuses legacy qs staging without modifying it", {
  staging_dir <- tempfile("cs_v019_runner_legacy_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  legacy_file <- file.path(staging_dir, "batch_1_legacy.qs")
  legacy_bytes <- charToRaw("legacy campaign evidence")
  writeBin(legacy_bytes, legacy_file)

  plan <- cs_plan_campaign(
    dgp_list = "synth_baseline",
    estimator_list = "lm_att",
    n_seeds = 1L,
    batch_size = 1,
    campaign_seed = 1,
    strategy_map = list(defaults = list(n = 100))
  )

  expect_error(
    cs_run_campaign(
      plan = plan,
      staging_dir = staging_dir,
      workers = 1,
      experimental_parallel = TRUE
    ),
    "Preserve the files",
    class = "causalstress_legacy_persistence_error"
  )
  expect_identical(
    readBin(legacy_file, "raw", n = as.integer(file.info(legacy_file)$size)),
    legacy_bytes
  )
})

test_that("cs_run_campaign executes batches in parallel", {
  staging_dir <- tempfile("cs_v019_runner_parallel_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  plan <- cs_plan_campaign(
    dgp_list = "synth_baseline",
    estimator_list = "lm_att",
    n_seeds = 1:2,
    batch_size = 1,
    campaign_seed = 2,
    strategy_map = list(defaults = list(n = 100))
  )

  expect_warning(
    cs_run_campaign(
      plan = plan,
      staging_dir = staging_dir,
      workers = 2,
      experimental_parallel = TRUE
    ),
    class = "causalstress_experimental_parallel"
  )

  files <- list.files(staging_dir, pattern = "^batch_[0-9]+", full.names = FALSE)
  expect_true(any(grepl("^batch_1", files)))
  expect_true(any(grepl("^batch_2", files)))
})
