test_that("runner path enforces mandated RNGkind", {
  # Derive the mandated RNGkind from cs_set_rng() (do not hardcode).
  old_kind <- RNGkind()
  on.exit(RNGkind(old_kind[1], old_kind[2], old_kind[3]), add = TRUE)

  cs_set_rng(1L)
  mandated <- RNGkind()

  # Put the session in a different RNG mode first.
  RNGkind("Wichmann-Hill")

  cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 30,
    seeds        = 1:1,
    parallel     = FALSE,
    experimental_parallel = FALSE,
    show_progress = FALSE,
    quiet = TRUE
  )

  after <- RNGkind()
  expect_identical(after[1:3], mandated[1:3])
})

