test_that("cs_run_campaign resumes based on staged batches", {
  staging_dir <- file.path(tempdir(), "cs_v019_runner_resume")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  plan <- cs_plan_campaign(
    dgp_list = "synth_baseline",
    estimator_list = "lm_att",
    n_seeds = 1:2,
    batch_size = 1,
    campaign_seed = 1,
    strategy_map = list(defaults = list(n = 100))
  )

  file.create(file.path(staging_dir, "batch_1_dummy.qs"))

  cs_run_campaign(plan = plan, staging_dir = staging_dir, workers = 1)

  files <- list.files(staging_dir, pattern = "^batch_[0-9]+", full.names = FALSE)
  batch1 <- files[grepl("^batch_1", files)]
  batch2 <- files[grepl("^batch_2", files)]

  expect_equal(length(batch1), 1L)
  expect_true(length(batch2) >= 1L)
})

test_that("cs_run_campaign executes batches in parallel", {
  staging_dir <- file.path(tempdir(), "cs_v019_runner_parallel")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  plan <- cs_plan_campaign(
    dgp_list = "synth_baseline",
    estimator_list = "lm_att",
    n_seeds = 1:2,
    batch_size = 1,
    campaign_seed = 2,
    strategy_map = list(defaults = list(n = 100))
  )

  cs_run_campaign(plan = plan, staging_dir = staging_dir, workers = 2)

  files <- list.files(staging_dir, pattern = "^batch_[0-9]+", full.names = FALSE)
  expect_true(any(grepl("^batch_1", files)))
  expect_true(any(grepl("^batch_2", files)))
})
