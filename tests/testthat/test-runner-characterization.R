test_that("seed and grid campaign runners share the serial result contract", {
  estimator_id <- "runner_characterization_qst"
  registry_env <- get(
    ".causalstress_estimator_registry_extra",
    envir = asNamespace("CausalStress")
  )
  registry_before <- registry_env$tbl
  withr::defer(registry_env$tbl <- registry_before)

  if (!estimator_id %in% cs_estimator_registry()$estimator_id) {
    cs_register_estimator(
      estimator_id = estimator_id,
      type = "test",
      generator = function(df, config = list(), tau = cs_tau_oracle, ...) {
        level <- as.numeric(config$level %||% 0)
        list(
          att = list(estimate = level),
          qst = tibble::tibble(tau = tau, value = level + seq_along(tau)),
          meta = list(
            estimator_id = "runner_characterization_qst",
            oracle = FALSE,
            supports_qst = TRUE
          )
        )
      },
      oracle = FALSE,
      supports_qst = TRUE,
      version = "0.0.0-test",
      description = "Runner characterization estimator",
      source = "test",
      requires_pkgs = character(0)
    )
  }

  seeds <- c(3L, 1L, 2L)
  tau <- c(0.1, 0.5, 0.9)
  config <- list(
    level = 0.25,
    ci_method = "none",
    estimand_targets = c("att", "qst")
  )

  seed_runs <- cs_run_seeds(
    dgp_id = "synth_baseline",
    estimator_id = estimator_id,
    n = 30L,
    seeds = seeds,
    tau = tau,
    B = 0L,
    config = config,
    show_progress = FALSE,
    quiet = TRUE
  )
  campaign_runs <- cs_run_campaign(
    dgp_ids = "synth_baseline",
    estimator_ids = estimator_id,
    n = 30L,
    seeds = seeds,
    tau = tau,
    B = 0L,
    defaults = config,
    show_progress = FALSE,
    quiet = TRUE
  )

  expect_identical(seed_runs$seed, sort(seeds))
  expect_identical(campaign_runs$seed, sort(seeds))

  stable_columns <- c(
    "success", "dgp_id", "dgp_version", "estimator_id",
    "estimator_version", "n", "seed", "true_att", "est_att",
    "att_error", "att_abs_error", "config_fingerprint",
    "config_fingerprint_schema", "fit_fingerprint", "truth_version"
  )
  expect_identical(seed_runs[stable_columns], campaign_runs[stable_columns])
  expect_identical(seed_runs$qst, campaign_runs$qst)
  expect_identical(seed_runs$scores, campaign_runs$scores)
  expect_identical(
    seed_runs$score_fingerprints,
    campaign_runs$score_fingerprints
  )
  expect_identical(
    seed_runs$score_row_fingerprints,
    campaign_runs$score_row_fingerprints
  )
  expect_true(all(vapply(
    seed_runs$qst,
    function(x) identical(x$tau, tau),
    logical(1)
  )))
})

test_that("experimental runner paths share stage-and-gather provenance", {
  skip_if_not_installed("pins")
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # This exercises the experimental-parallel branches deterministically without
  # asserting anything about multisession scheduling or worker call order.
  future::plan(future::sequential)

  seeds <- c(2L, 1L)
  config <- list(ci_method = "none", num_threads = 8L)
  seed_board <- pins::board_temp()
  campaign_board <- pins::board_temp()
  seed_staging <- tempfile("cs_characterize_seed_stage_")
  campaign_staging <- tempfile("cs_characterize_campaign_stage_")

  seed_warnings <- list()
  seed_runs <- withCallingHandlers(
    cs_run_seeds(
      dgp_id = "synth_baseline",
      estimator_id = "lm_att",
      n = 30L,
      seeds = seeds,
      B = 0L,
      config = config,
      board = seed_board,
      staging_dir = seed_staging,
      parallel = TRUE,
      experimental_parallel = TRUE,
      show_progress = FALSE,
      quiet = TRUE
    ),
    warning = function(w) {
      seed_warnings <<- append(seed_warnings, list(w))
      invokeRestart("muffleWarning")
    }
  )

  campaign_warnings <- list()
  campaign_runs <- withCallingHandlers(
    cs_run_campaign(
      dgp_ids = "synth_baseline",
      estimator_ids = "lm_att",
      n = 30L,
      seeds = seeds,
      B = 0L,
      defaults = config,
      board = campaign_board,
      staging_dir = campaign_staging,
      parallel = TRUE,
      experimental_parallel = TRUE,
      show_progress = FALSE,
      quiet = TRUE
    ),
    warning = function(w) {
      campaign_warnings <<- append(campaign_warnings, list(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(
    sum(vapply(
      seed_warnings,
      inherits,
      logical(1),
      "causalstress_experimental_parallel"
    )),
    1L
  )
  expect_equal(
    sum(vapply(
      campaign_warnings,
      inherits,
      logical(1),
      "causalstress_experimental_parallel"
    )),
    1L
  )
  expect_identical(seed_runs$seed, sort(seeds))
  expect_identical(campaign_runs$seed, sort(seeds))
  expect_identical(
    seed_runs$config_fingerprint,
    campaign_runs$config_fingerprint
  )
  expect_identical(seed_runs$fit_fingerprint, campaign_runs$fit_fingerprint)
  expect_identical(
    seed_runs$score_fingerprints,
    campaign_runs$score_fingerprints
  )

  dgp_version <- seed_runs$dgp_version[[1L]]
  seed_pin <- pins::pin_read(
    seed_board,
    CausalStress:::cs_find_result_pin(
      board = seed_board,
      dgp_id = "synth_baseline",
      dgp_version = dgp_version,
      estimator_id = "lm_att",
      n = 30L,
      seed = 1L
    )
  )
  campaign_pin <- pins::pin_read(
    campaign_board,
    CausalStress:::cs_find_result_pin(
      board = campaign_board,
      dgp_id = "synth_baseline",
      dgp_version = dgp_version,
      estimator_id = "lm_att",
      n = 30L,
      seed = 1L
    )
  )

  provenance_fields <- c(
    "experimental_parallel", "parallel_warning_emitted",
    "parallel_backend", "thread_caps_applied", "thread_caps_env",
    "requested_num_threads", "effective_num_threads", "staging_dir_used"
  )
  expect_identical(
    seed_pin$provenance[provenance_fields],
    campaign_pin$provenance[provenance_fields]
  )
  expect_true(isTRUE(seed_pin$provenance$experimental_parallel))
  expect_true(isTRUE(seed_pin$provenance$thread_caps_applied))
  expect_true(isTRUE(seed_pin$provenance$staging_dir_used))
  expect_identical(seed_pin$provenance$requested_num_threads, 8L)
  expect_identical(seed_pin$provenance$effective_num_threads, 1L)
  expect_true(all(seed_pin$provenance$thread_caps_env == "1"))
})

test_that("cached and forced runner behavior is explicit and non-overwriting", {
  skip_if_not_installed("pins")

  seed_board <- pins::board_temp()
  campaign_board <- pins::board_temp()
  config <- list(ci_method = "none")

  seed_first <- cs_run_seeds(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 30L,
    seeds = 1L,
    B = 0L,
    config = config,
    board = seed_board,
    show_progress = FALSE,
    quiet = TRUE
  )
  campaign_first <- cs_run_campaign(
    dgp_ids = "synth_baseline",
    estimator_ids = "lm_att",
    n = 30L,
    seeds = 1L,
    B = 0L,
    defaults = config,
    board = campaign_board,
    show_progress = FALSE,
    quiet = TRUE
  )

  dgp_version <- seed_first$dgp_version[[1L]]
  seed_pin_name <- CausalStress:::cs_find_result_pin(
    board = seed_board,
    dgp_id = "synth_baseline",
    dgp_version = dgp_version,
    estimator_id = "lm_att",
    n = 30L,
    seed = 1L
  )
  campaign_pin_name <- CausalStress:::cs_find_result_pin(
    board = campaign_board,
    dgp_id = "synth_baseline",
    dgp_version = dgp_version,
    estimator_id = "lm_att",
    n = 30L,
    seed = 1L
  )
  seed_before <- pins::pin_read(seed_board, seed_pin_name)
  campaign_before <- pins::pin_read(campaign_board, campaign_pin_name)

  seed_cached <- cs_run_seeds(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 30L,
    seeds = 1L,
    B = 0L,
    config = config,
    board = seed_board,
    skip_existing = TRUE,
    show_progress = TRUE,
    quiet = TRUE
  )
  campaign_messages <- character()
  campaign_cached <- withCallingHandlers(
    cs_run_campaign(
      dgp_ids = "synth_baseline",
      estimator_ids = "lm_att",
      n = 30L,
      seeds = 1L,
      B = 0L,
      defaults = config,
      board = campaign_board,
      skip_existing = TRUE,
      show_progress = TRUE,
      quiet = TRUE
    ),
    message = function(m) {
      campaign_messages <<- c(campaign_messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  seed_after_cache <- pins::pin_read(seed_board, seed_pin_name)
  campaign_after_cache <- pins::pin_read(campaign_board, campaign_pin_name)
  expect_equal(nrow(seed_cached), 1L)
  expect_equal(nrow(campaign_cached), 0L)
  expect_true(length(campaign_messages) >= 1L)
  expect_identical(seed_cached$fit_fingerprint, seed_first$fit_fingerprint)
  expect_identical(
    seed_before$provenance$timestamp,
    seed_after_cache$provenance$timestamp
  )
  expect_identical(
    campaign_before$provenance$timestamp,
    campaign_after_cache$provenance$timestamp
  )

  forced_config <- list(ci_method = "none", characterization_nonce = 1L)
  seed_forced <- cs_run_seeds(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 30L,
    seeds = 1L,
    B = 0L,
    config = forced_config,
    board = seed_board,
    force = TRUE,
    skip_existing = TRUE,
    show_progress = FALSE,
    quiet = TRUE
  )
  campaign_forced <- cs_run_campaign(
    dgp_ids = "synth_baseline",
    estimator_ids = "lm_att",
    n = 30L,
    seeds = 1L,
    B = 0L,
    defaults = forced_config,
    board = campaign_board,
    force = TRUE,
    skip_existing = TRUE,
    show_progress = FALSE,
    quiet = TRUE
  )

  expect_false(identical(
    seed_forced$config_fingerprint,
    seed_first$config_fingerprint
  ))
  expect_false(identical(
    campaign_forced$config_fingerprint,
    campaign_first$config_fingerprint
  ))
  expect_identical(
    seed_forced$config_fingerprint,
    campaign_forced$config_fingerprint
  )
  expect_identical(seed_forced$fit_fingerprint, campaign_forced$fit_fingerprint)
})

test_that("DGP warning counts remain runner-path behavior", {
  seed_warnings <- list()
  seed_runs <- withCallingHandlers(
    cs_run_seeds(
      dgp_id = "synth_baseline",
      estimator_id = "lm_att",
      n = 30L,
      seeds = 1:2,
      version = "1.3.0",
      status = "deprecated",
      B = 0L,
      show_progress = TRUE,
      quiet = FALSE
    ),
    warning = function(w) {
      seed_warnings <<- append(seed_warnings, list(w))
      invokeRestart("muffleWarning")
    }
  )

  campaign_warnings <- list()
  campaign_runs <- withCallingHandlers(
    cs_run_campaign(
      dgp_ids = "synth_baseline",
      estimator_ids = "lm_att",
      n = 30L,
      seeds = 1:2,
      version = "1.3.0",
      status = "deprecated",
      B = 0L,
      show_progress = TRUE,
      quiet = FALSE
    ),
    warning = function(w) {
      campaign_warnings <<- append(campaign_warnings, list(w))
      invokeRestart("muffleWarning")
    }
  )

  seed_deprecation <- vapply(
    seed_warnings,
    function(w) grepl("deprecated DGP", conditionMessage(w), fixed = TRUE),
    logical(1)
  )
  campaign_deprecation <- vapply(
    campaign_warnings,
    function(w) grepl("deprecated DGP", conditionMessage(w), fixed = TRUE),
    logical(1)
  )
  expect_equal(sum(seed_deprecation), 1L)
  expect_equal(sum(campaign_deprecation), 2L)
  expect_true(all(vapply(
    seed_warnings[seed_deprecation],
    inherits,
    logical(1),
    "rlang_warning"
  )))
  expect_true(all(vapply(
    campaign_warnings[campaign_deprecation],
    inherits,
    logical(1),
    "rlang_warning"
  )))
  expect_identical(seed_runs$seed, 1:2)
  expect_identical(campaign_runs$seed, 1:2)
})

test_that("grid and planned campaigns retain distinct public contracts", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")

  grid <- cs_run_campaign(
    dgp_ids = "synth_baseline",
    estimator_ids = "lm_att",
    n = 30L,
    seeds = 1L,
    B = 0L,
    defaults = list(ci_method = "none"),
    show_progress = FALSE,
    quiet = TRUE
  )

  staging_dir <- tempfile("cs_characterize_planned_")
  plan <- cs_plan_campaign(
    dgp_list = "synth_baseline",
    estimator_list = "lm_att",
    n_seeds = 1L,
    batch_size = 1L,
    campaign_seed = 1L,
    strategy_map = list(defaults = list(n = 30L, ci_method = "none"))
  )
  expect_warning(
    executed <- cs_run_campaign(
      plan = plan,
      staging_dir = staging_dir,
      workers = 1L,
      show_progress = FALSE,
      experimental_parallel = TRUE
    ),
    class = "causalstress_experimental_parallel"
  )
  resumed <- suppressMessages(cs_run_campaign(
    plan = plan,
    staging_dir = staging_dir,
    workers = 1L,
    show_progress = FALSE,
    experimental_parallel = TRUE
  ))

  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 1L)
  expect_identical(executed, as.integer(plan$batch_id))
  expect_identical(resumed, integer(0))
  expect_equal(
    length(list.files(staging_dir, pattern = "^batch_[0-9]+.*\\.rds$")),
    1L
  )
})
