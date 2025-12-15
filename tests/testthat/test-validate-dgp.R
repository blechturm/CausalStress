test_that("cs_validate_dgp passes on well-formed deterministic DGP", {
  good_dgp <- function(n, seed) {
    set.seed(seed)
    w <- rbinom(n, 1, 0.5)
    y0 <- rnorm(n, 0, 1)
    y1 <- y0 + 1
    y <- ifelse(w == 1, y1, y0)
    true_att <- mean(y1 - y0)
    list(
      df = data.frame(y = y, w = w, true_y0 = y0, true_y1 = y1),
      true_att = true_att
    )
  }

  res <- cs_validate_dgp(good_dgp, n = 50, seeds = 1:5, verbose = FALSE)
  expect_true(res$valid)
  expect_true(res$checks["schema"])
  expect_true(res$checks["determinism"])
})

test_that("cs_validate_dgp fails on missing columns", {
  bad_dgp <- function(n, seed) {
    set.seed(seed)
    data.frame(y = rnorm(n))
  }

  res <- cs_validate_dgp(bad_dgp, n = 10, seeds = 1:3, verbose = FALSE)
  expect_false(res$valid)
  expect_false(res$checks["schema"])
})
