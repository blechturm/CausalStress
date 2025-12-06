# Provide expect_no_error for environments where it is absent (testthat < 3.2)
if (!exists("expect_no_error")) {
  expect_no_error <- function(object, ...) {
    testthat::expect_error(object, NA, ...)
  }
}

