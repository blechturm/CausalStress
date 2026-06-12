test_that("cs_validate_registry validates every registered DGP version", {
  gen_v1 <- function(n, seed) NULL
  gen_v2 <- function(n, seed) NULL
  attr(gen_v1, "marker") <- "v1"
  attr(gen_v2, "marker") <- "v2"
  seen <- character()

  with_mocked_bindings(
    cs_dgp_registry = function() {
      tibble::tibble(
        dgp_id = c("foo", "foo"),
        type = c("synthetic", "synthetic"),
        generator = list(gen_v1, gen_v2),
        version = c("1.0.0", "2.0.0"),
        description = c("old", "new")
      )
    },
    cs_validate_dgp = function(dgp_fn, ...) {
      seen <<- c(seen, attr(dgp_fn, "marker"))
      list(
        valid = TRUE,
        cv_true_att = 0,
        mean_naive_bias = 0,
        checks = c(schema = TRUE, determinism = TRUE)
      )
    },
    {
      res <- cs_validate_registry()
    }
  )

  expect_equal(res$dgp_id, c("foo", "foo"))
  expect_equal(res$version, c("1.0.0", "2.0.0"))
  expect_equal(seen, c("v1", "v2"))
})
