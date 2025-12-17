test_that("cs_bootstrap_ci handles probe failure", {
  stat_fn <- function(df) stop("boom")
  res <- cs_bootstrap_ci(stat_fn, data.frame(x = 1), n_boot = 5, seed = 123)
  expect_equal(length(res$ci_lo), 0)
  expect_equal(res$meta$ci_fail_code, "initial_failure")
})

test_that("cs_bootstrap_ci gates by dimension success", {
  set.seed(1)
  stat_fn <- function(df) {
    if (runif(1) < 0.5) {
      c(1, NA_real_)
    } else {
      c(1, 2)
    }
  }
  df <- data.frame(x = rnorm(20))
  res <- cs_bootstrap_ci(stat_fn, df, n_boot = 20, seed = 42)
  expect_equal(res$meta$ci_fail_code, "low_boot_success")
  expect_false(is.na(res$ci_lo[1]))
  expect_true(is.na(res$ci_lo[2]))
})

test_that("cs_bootstrap_ci is deterministic with same seed", {
  df <- data.frame(x = rnorm(50))
  stat_fn <- function(df) mean(df$x)
  res1 <- cs_bootstrap_ci(stat_fn, df, n_boot = 30, seed = 99)
  res2 <- cs_bootstrap_ci(stat_fn, df, n_boot = 30, seed = 99)
  expect_identical(res1$ci_lo, res2$ci_lo)
  expect_identical(res1$meta$n_boot_ok, res2$meta$n_boot_ok)
})

test_that("cs_bootstrap_ci flags collapsed intervals", {
  df <- data.frame(x = rnorm(10))
  stat_fn <- function(df) 5
  res <- cs_bootstrap_ci(stat_fn, df, n_boot = 10, seed = 7)
  expect_true(any(res$meta$collapsed))
})

test_that("est_lm_att returns bootstrap CIs when requested", {
  dgp <- dgp_synth_baseline(n = 200, seed = 1)
  res <- est_lm_att(
    dgp$df,
    config = list(ci_method = "bootstrap", seed = 123, n_boot = 50)
  )
  expect_false(is.na(res$att$ci_lo))
  expect_false(is.na(res$att$ci_hi))
  expect_true(res$meta$n_boot_ok[1] >= 0)
})
