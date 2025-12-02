test_that("cs_delete_result and cs_delete_campaign remove pins", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  # write a couple of pins
  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seeds        = 1:2,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = FALSE
  )

  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 50, 1))
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 50, 2))

  # delete one seed
  cs_delete_result(board, "synth_baseline", "lm_att", n = 50, seed = 1)
  expect_false(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 50, 1))
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 50, 2))

  # delete remaining campaign
  cs_delete_campaign(board, "synth_baseline", "lm_att")
  expect_false(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 50, 2))
  pl <- pins::pin_list(board)
  remaining <- if (is.data.frame(pl)) nrow(pl) else length(pl)
  expect_equal(remaining, 0)
})
