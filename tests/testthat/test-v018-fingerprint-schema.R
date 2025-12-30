test_that("v0.1.8 writes config_fingerprint_schema=2 and max_runtime participates in identity", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 30,
    seeds        = 1:1,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE,
    quiet = TRUE,
    max_runtime = Inf
  )

  name <- "results__dgp=synth_baseline__est=lm_att__n=30__seed=1"
  pin <- pins::pin_read(board, name)
  expect_identical(pin$meta$config_fingerprint_schema, 2L)
  expect_true(is.character(pin$meta$config_fingerprint))

  expect_error(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 30,
      seeds        = 1:1,
      board        = board,
      skip_existing = TRUE,
      show_progress = FALSE,
      quiet = TRUE,
      max_runtime = 0.5
    ),
    "Configuration fingerprint mismatch"
  )
})

test_that("legacy (schema-missing) pins resume deterministically and forbid finite max_runtime", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  dgp_id <- "synth_baseline"
  estimator_id <- "lm_att"
  n <- 30L
  seed <- 1L

  est_desc <- cs_get_estimator(estimator_id)
  legacy_fp <- CausalStress:::cs_build_config_fingerprint_legacy(
    dgp_id = dgp_id,
    estimator_id = estimator_id,
    n = n,
    seed = seed,
    bootstrap = FALSE,
    B = 200L,
    oracle = isTRUE(est_desc$oracle),
    estimator_version = est_desc$version,
    config = list(seed = seed),
    tau = cs_tau_oracle
  )

  legacy_obj <- list(
    att = list(
      estimate = 999,
      true = 0,
      error = 999,
      abs_error = 999,
      ci_lo = NA_real_,
      ci_hi = NA_real_,
      boot_covered = NA,
      ci_width = NA_real_
    ),
    qst = NULL,
    boot_draws = NULL,
    meta = list(
      success = TRUE,
      error = NA_character_,
      dgp_id = dgp_id,
      estimator_id = estimator_id,
      n = n,
      seed = seed,
      oracle = FALSE,
      supports_qst = FALSE,
      estimator_version = est_desc$version,
      config_fingerprint = legacy_fp
      # NOTE: schema intentionally omitted to simulate v0.1.7 artifacts.
    ),
    provenance = list()
  )

  name <- glue::glue("results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}")
  pins::pin_write(
    board = board,
    x = legacy_obj,
    name = name,
    type = "qs",
    metadata = list(
      dgp_id = dgp_id,
      estimator_id = estimator_id,
      n = n,
      seed = seed,
      config_fingerprint = legacy_fp
      # schema intentionally omitted
    )
  )

  resumed <- cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = estimator_id,
    n            = n,
    seeds        = seed,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE,
    quiet = TRUE,
    max_runtime = Inf
  )
  expect_equal(resumed$est_att[1], 999)

  expect_error(
    cs_run_seeds(
      dgp_id       = dgp_id,
      estimator_id = estimator_id,
      n            = n,
      seeds        = seed,
      board        = board,
      skip_existing = TRUE,
      show_progress = FALSE,
      quiet = TRUE,
      max_runtime = 0.5
    ),
    "legacy.*max_runtime|Cannot resume legacy",
    ignore.case = TRUE
  )
})
