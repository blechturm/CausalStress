test_that("cs_plot_att_error returns ggplot", {
  tib <- tibble::tibble(
    dgp_id = c("d1", "d1"),
    estimator_id = c("e1", "e2"),
    att_error = c(0.1, -0.2)
  )
  p <- cs_plot_att_error(tib)
  expect_s3_class(p, "ggplot")
})

test_that("cs_plot_placebo returns ggplot", {
  tib <- tibble::tibble(
    dgp_id = c("synth_placebo_tau0", "synth_placebo_tau0"),
    estimator_id = c("e1", "e1"),
    est_att = c(0.0, 0.05),
    att_ci_lo = c(-0.1, -0.05),
    att_ci_hi = c(0.1, 0.1),
    att_covered = c(TRUE, FALSE)
  )
  p <- cs_plot_placebo(tib)
  expect_s3_class(p, "ggplot")
})
