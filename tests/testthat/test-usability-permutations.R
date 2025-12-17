test_that("skip_existing/force permutations behave as intended", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()
  dgp_id <- "synth_baseline"
  est_id <- "lm_att"
  n <- 50
  seed <- 1
  name <- glue::glue("results__dgp={dgp_id}__est={est_id}__n={n}__seed={seed}")

  # Setup: base pin without bootstrap
  cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seed,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE
  )
  meta_base <- pins::pin_meta(board, name)$user$timestamp

  # 1) Happy Resume: same config + skip_existing = TRUE => cached
  out_cached <- cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seed,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE
  )
  meta_cached <- pins::pin_meta(board, name)$user$timestamp
  expect_identical(meta_cached, meta_base)
  expect_equal(out_cached$seed, seed)

  # 2) Explicit Re-run: skip_existing = FALSE overwrites
  Sys.sleep(1)
  out_rerun <- cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seed,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE
  )
  expect_equal(out_rerun$seed, seed)

  # 3) Force Re-run: force = TRUE recomputes
  Sys.sleep(1)
  out_force <- cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seed,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    force         = TRUE,
    show_progress = FALSE
  )
  expect_equal(out_force$seed, seed)

  # 4) Safety Stop: different config + skip_existing = TRUE -> error
  expect_error(
    cs_run_seeds(
      dgp_id       = dgp_id,
      estimator_id = est_id,
      n            = n,
      seeds        = seed,
      bootstrap    = TRUE,
      B            = 10,
      board        = board,
      skip_existing = TRUE,
      show_progress = FALSE
    ),
    "fingerprint mismatch"
  )
  cached_fp <- pins::pin_read(board, name)$meta$config_fingerprint
  expect_false(is.null(cached_fp))

  # 5) Manual Override: different config + skip_existing = FALSE -> recompute
  Sys.sleep(1)
  cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seed,
    bootstrap    = TRUE,
    B            = 10,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE
  )
  pin_after_override <- pins::pin_read(board, name)
  expect_false(is.null(pin_after_override$meta$config_fingerprint))
  expect_false(identical(pin_after_override$meta$config_fingerprint, cached_fp))

  # 6) Force Override: different config + force = TRUE -> recompute
  Sys.sleep(1)
  cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seed,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    force         = TRUE,
    show_progress = FALSE
  )
  pin_after_force <- pins::pin_read(board, name)
  expect_false(is.null(pin_after_force$meta$config_fingerprint))
  expect_false(identical(pin_after_force$meta$config_fingerprint, pin_after_override$meta$config_fingerprint))
  expect_identical(pin_after_force$meta$config_fingerprint, cached_fp)

  # 7) Nuclear Option: delete then rerun
  cs_delete_result(board, dgp_id, est_id, n = n, seed = seed)
  expect_false(CausalStress:::cs_pin_exists(board, dgp_id, est_id, n, seed))
  cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seed,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE
  )
  expect_true(CausalStress:::cs_pin_exists(board, dgp_id, est_id, n, seed))
})
