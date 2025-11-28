test_that("cs_get_dgp returns a valid descriptor for synth_baseline", {
  desc <- cs_get_dgp("synth_baseline")

  expect_type(desc, "list")
  expect_equal(desc$dgp_id, "synth_baseline")
  expect_identical(desc$type, "synthetic")
  expect_true(is.function(desc$generator))

  dgp <- desc$generator(n = 50, seed = 1L)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_get_dgp fails for unknown DGP IDs", {
  expect_error(
    cs_get_dgp("does_not_exist"),
    class = "causalstress_registry_error"
  )
})

test_that("cs_dgp_registry contains synth_baseline entry", {
  reg <- CausalStress:::cs_dgp_registry()

  expect_true(is.data.frame(reg))
  expect_true(all(c("dgp_id", "type", "generator") %in% names(reg)))
  expect_true("synth_baseline" %in% reg$dgp_id)
})
