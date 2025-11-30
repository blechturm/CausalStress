# tests/testthat/test-dgp-registry.R

test_that("cs_dgp_registry has required columns and sane types", {
  reg <- CausalStress:::cs_dgp_registry()

  expect_true(is.data.frame(reg))

  # Required columns
  required_cols <- c("dgp_id", "type", "generator", "version", "description")
  expect_true(all(required_cols %in% names(reg)))

  # Types
  expect_true(is.character(reg$dgp_id))
  expect_true(is.character(reg$type))
  expect_true(is.character(reg$version))
  expect_true(is.character(reg$description))
  expect_true(all(vapply(reg$generator, is.function, logical(1))))

  # All current DGPs are synthetic in the MVP
  expect_true(all(reg$type == "synthetic"))

  # Registry versions allowed
  expect_true(all(reg$version %in% c("1.3.0", "1.4.0")))

  # Known IDs must be present
  expect_true("synth_baseline"   %in% reg$dgp_id)
  expect_true("synth_heavytail"  %in% reg$dgp_id)
  expect_true("synth_placebo_tau0" %in% reg$dgp_id)
  expect_true("synth_qte1" %in% reg$dgp_id)
  expect_true("synth_nonlinear_heteroskedastic" %in% reg$dgp_id)
  expect_true("synth_overlap_stressed" %in% reg$dgp_id)
  expect_true("synth_tilt_mild" %in% reg$dgp_id)
  expect_true("synth_placebo_nonlinear" %in% reg$dgp_id)
  expect_true("synth_placebo_heavytail" %in% reg$dgp_id)
  expect_true("synth_placebo_tilted" %in% reg$dgp_id)
  expect_true("synth_placebo_kangschafer" %in% reg$dgp_id)
  expect_true("synth_hd_sparse_plm" %in% reg$dgp_id)
})

test_that("cs_get_dgp returns a valid descriptor for synth_baseline", {
  desc <- cs_get_dgp("synth_baseline")

  expect_type(desc, "list")
  expect_equal(desc$dgp_id, "synth_baseline")
  expect_identical(desc$type, "synthetic")
  expect_true(is.function(desc$generator))
  expect_equal(desc$version, "1.3.0")

  # Generator should produce a DGP object that passes the synthetic contract
  dgp <- desc$generator(n = 50, seed = 1L)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_get_dgp returns a valid descriptor for synth_heavytail", {
  desc <- cs_get_dgp("synth_heavytail")

  expect_type(desc, "list")
  expect_equal(desc$dgp_id, "synth_heavytail")
  expect_identical(desc$type, "synthetic")
  expect_true(is.function(desc$generator))
  expect_equal(desc$version, "1.3.0")

  dgp <- desc$generator(n = 50, seed = 1L)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_get_dgp returns a valid descriptor for synth_placebo_tau0", {
  desc <- cs_get_dgp("synth_placebo_tau0")

  expect_type(desc, "list")
  expect_equal(desc$dgp_id, "synth_placebo_tau0")
  expect_identical(desc$type, "synthetic")
  expect_true(is.function(desc$generator))
  expect_equal(desc$version, "1.3.0")

  dgp <- desc$generator(n = 50, seed = 2L)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_get_dgp returns a valid descriptor for synth_placebo_kangschafer", {
  desc <- cs_get_dgp("synth_placebo_kangschafer")

  expect_type(desc, "list")
  expect_equal(desc$dgp_id, "synth_placebo_kangschafer")
  expect_identical(desc$type, "synthetic")
  expect_true(is.function(desc$generator))
  expect_true(desc$version %in% c("1.3.0", "1.4.0"))

  dgp <- desc$generator(n = 50, seed = 3L)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_get_dgp returns a valid descriptor for synth_hd_sparse_plm", {
  desc <- cs_get_dgp("synth_hd_sparse_plm")

  expect_type(desc, "list")
  expect_equal(desc$dgp_id, "synth_hd_sparse_plm")
  expect_identical(desc$type, "synthetic")
  expect_true(is.function(desc$generator))
  expect_true(desc$version %in% c("1.4.0"))

  dgp <- desc$generator(n = 30, seed = 4L)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_get_dgp fails for unknown DGP IDs", {
  expect_error(
    cs_get_dgp("does_not_exist"),
    class = "causalstress_registry_error"
  )
})
