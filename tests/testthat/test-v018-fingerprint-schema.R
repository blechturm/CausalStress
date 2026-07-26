test_that("v0.2.0 writes config_fingerprint_schema=4 and max_runtime participates in identity", {
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

  name <- CausalStress:::cs_result_pin_name("synth_baseline", "lm_att", 30, 1, "1.6.0")
  pin <- pins::pin_read(board, name)
  expect_identical(pin$meta$config_fingerprint_schema, 4L)
  expect_identical(pin$meta$dgp_version, "1.6.0")
  expect_true(is.character(pin$meta$config_fingerprint))
  expect_true(is.character(pin$meta$fit_fingerprint))
  expect_true(is.character(pin$meta$truth_version))

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

test_that("representative schema-4 configuration fingerprints remain fixed", {
  expect_identical(
    CausalStress:::cs_build_config_fingerprint(
      dgp_id = "synth_baseline",
      estimator_id = "lm_att",
      n = 30L,
      seed = 1L,
      bootstrap = FALSE,
      B = 200L,
      oracle = FALSE,
      estimator_version = "0.2.0",
      config = list(),
      tau = cs_tau_oracle,
      max_runtime = Inf,
      dgp_version = "1.6.0"
    ),
    "a1affaf96ebaf3149f22311f7dd14e8e2a3588d6e4822555d1c5b894075a6539"
  )

  expect_identical(
    CausalStress:::cs_build_config_fingerprint(
      dgp_id = "synth_baseline",
      estimator_id = "lm_att",
      n = 75L,
      seed = 17L,
      bootstrap = TRUE,
      B = 99L,
      oracle = FALSE,
      estimator_version = "0.2.0",
      config = list(
        alpha = 0.1,
        ci_method = "bootstrap",
        nested = list(z = 2L, a = TRUE)
      ),
      tau = c(0.1, 0.5, 0.9),
      max_runtime = 12.5,
      dgp_version = "1.6.0"
    ),
    "2bf5d8b6028401e660ae4b1fb3fccb05a08ebcaacd8fd5ba4a489ea4399d69a8"
  )
})

test_that("historical schemas fail before fingerprint comparison", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()
  dgp_id <- "synth_baseline"
  estimator_id <- "lm_att"
  n <- 30L
  est_desc <- cs_get_estimator(estimator_id)
  sentinel <- "NOT-A-HASH-SCHEMA-REJECTED-BEFORE-COMPARISON"
  schemas <- list(NULL, 1L, 2L, 3L)
  expected_message <- paste0(
    "Schema 1-3 artifacts are read-only historical inputs in v0.2.0 and ",
    "cannot be resumed into schema-4 runs. Use skip_existing = FALSE, ",
    "force = TRUE, or a fresh board."
  )

  for (i in seq_along(schemas)) {
    schema <- schemas[[i]]
    seed <- as.integer(i)
    metadata <- list(
      dgp_id = dgp_id,
      estimator_id = estimator_id,
      n = n,
      seed = seed,
      config_fingerprint = sentinel
    )
    object_meta <- c(
      metadata,
      list(
        success = TRUE,
        error = NA_character_,
        oracle = FALSE,
        supports_qst = FALSE,
        estimator_version = est_desc$version
      )
    )
    if (!is.null(schema)) {
      metadata$config_fingerprint_schema <- schema
      object_meta$config_fingerprint_schema <- schema
    }

    pins::pin_write(
      board = board,
      x = list(
        att = list(estimate = 999, true = 0, error = 999, abs_error = 999),
        qst = NULL,
        boot_draws = NULL,
        meta = object_meta,
        provenance = list()
      ),
      name = CausalStress:::cs_result_pin_name_legacy(
        dgp_id,
        estimator_id,
        n,
        seed
      ),
      type = "rds",
      metadata = metadata
    )

    err <- expect_error(
      cs_run_seeds(
        dgp_id = dgp_id,
        estimator_id = estimator_id,
        n = n,
        seeds = seed,
        board = board,
        skip_existing = TRUE,
        show_progress = FALSE,
        quiet = TRUE,
        max_runtime = Inf
      ),
      class = "causalstress_schema_migration_error"
    )
    expect_identical(conditionMessage(err), expected_message)
    expect_false(grepl(sentinel, conditionMessage(err), fixed = TRUE))
  }
})
