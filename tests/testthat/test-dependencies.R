test_that("ggplot2 is available", {
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
})

test_that("tidyr is available", {
  expect_true(requireNamespace("tidyr", quietly = TRUE))
})
