test_that("cs_run_campaign runs multiple DGPs in parallel", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # Note: sequential plan only exercises the experimental-parallel code path;
  # it does not create true multiprocessing workers.
  future::plan(future::sequential)

  expect_warning(
    res <- cs_run_campaign(
      dgp_ids       = c("synth_baseline", "synth_placebo_tau0"),
      estimator_ids = "lm_att",
      seeds         = 1:2,
      n             = 30,
      parallel      = TRUE,
      experimental_parallel = TRUE,
      show_progress = FALSE,
      quiet         = TRUE
    ),
    class = "causalstress_experimental_parallel"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)
  expect_true(all(res$success))
})

test_that("cs_run_campaign forwards documented dots to cs_run_single", {
  dot_est <- function(df, config = list(), tau = cs_tau_oracle, required_arg = NULL, ...) {
    if (!identical(required_arg, "forwarded")) {
      stop("required_arg was not forwarded")
    }
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "dot_forward_est", oracle = FALSE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "dot_forward_est",
    type          = "test",
    generator     = dot_est,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Dots forwarding estimator",
    source        = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_campaign(
    dgp_ids       = "synth_baseline",
    estimator_ids = "dot_forward_est",
    seeds         = 1L,
    n             = 30,
    show_progress = FALSE,
    required_arg  = "forwarded"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_true(res$success[[1]])
})

test_that("cs_run_campaign does not silence DGP governance warnings by default", {
  expect_warning(
    cs_run_campaign(
      dgp_ids       = "synth_baseline",
      estimator_ids = "lm_att",
      seeds         = 1L,
      n             = 30,
      version       = "1.3.0",
      status        = "deprecated",
      show_progress = FALSE
    ),
    "deprecated"
  )
})
