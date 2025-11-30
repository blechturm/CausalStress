test_that("force and skip_existing=FALSE overwrite pins", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()
  dgp_id <- "synth_baseline"
  est_id <- "lm_att"
  n <- 40
  seeds <- 1
  name <- glue::glue("results__dgp={dgp_id}__est={est_id}__n={n}__seed={seeds}")

  # First run writes pin
  cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seeds,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE
  )
  expect_true(pins::pin_exists(board, name))

  # skip_existing = FALSE should overwrite
  cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seeds,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE
  )
  expect_true(pins::pin_exists(board, name))

  Sys.sleep(1)

  # force = TRUE should also overwrite even if skip_existing TRUE
  cs_run_seeds(
    dgp_id       = dgp_id,
    estimator_id = est_id,
    n            = n,
    seeds        = seeds,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    force         = TRUE,
    show_progress = FALSE
  )
  expect_true(pins::pin_exists(board, name))
})
