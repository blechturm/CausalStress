make_certification_dgp <- function(covariate_names = "X1") {
  force(covariate_names)
  function(n, seed) {
    set.seed(seed)
    w <- rbinom(n, 1, 0.5)
    y0 <- rnorm(n, 0, 1)
    y1 <- y0 + 1
    y <- ifelse(w == 1, y1, y0)
    structural_te <- y1 - y0
    df <- data.frame(
      y = y,
      w = w,
      p = rep(0.5, n),
      y0 = y0,
      y1 = y1,
      structural_te = structural_te
    )
    for (name in covariate_names) {
      df[[name]] <- rnorm(n)
    }
    list(
      df = df,
      true_att = mean(structural_te[w == 1]),
      true_qst = tibble::tibble(
        tau = cs_tau_oracle,
        value = rep(1, length(cs_tau_oracle))
      ),
      meta = list(
        dgp_id = "certification_probe",
        type = "synthetic",
        structural_te = structural_te
      )
    )
  }
}

test_that("cs_validate_dgp passes on well-formed deterministic DGP", {
  good_dgp <- make_certification_dgp()

  res <- cs_validate_dgp(good_dgp, n = 50, seeds = 1:5, verbose = FALSE)
  expect_true(res$valid)
  expect_true(res$checks["schema"])
  expect_true(res$checks["determinism"])
})

test_that("cs_validate_dgp rejects an empty seed set", {
  expect_error(
    cs_validate_dgp(
      make_certification_dgp(),
      seeds = integer(0),
      verbose = FALSE
    ),
    class = "causalstress_contract_error"
  )
})

test_that("cs_validate_dgp fails on missing columns", {
  bad_dgp <- function(n, seed) {
    set.seed(seed)
    data.frame(y = rnorm(n))
  }

  expect_error(
    cs_validate_dgp(bad_dgp, n = 10, seeds = 1:3, verbose = FALSE),
    class = "causalstress_dgp_error"
  )
})

test_that("cs_validate_dgp fails when potential outcomes are missing", {
  bad_dgp <- function(n, seed) {
    set.seed(seed)
    w <- rbinom(n, 1, 0.5)
    y <- rnorm(n) + w
    list(
      df = data.frame(y = y, w = w),
      true_att = 1
    )
  }

  expect_error(
    cs_validate_dgp(bad_dgp, n = 20, seeds = 1:3, verbose = FALSE),
    class = "causalstress_dgp_error"
  )
})

test_that("cs_validate_dgp enforces canonical covariates and preserves RNG state", {
  old_state <- CausalStress:::cs_rng_state_capture()
  on.exit(CausalStress:::cs_rng_state_restore(old_state), add = TRUE)

  set.seed(20260724)
  before_kind <- RNGkind()
  before_seed <- .Random.seed

  expect_error(
    cs_validate_dgp(
      make_certification_dgp("x1"),
      n = 20,
      seeds = 1:3,
      verbose = FALSE
    ),
    class = "causalstress_dgp_error"
  )
  expect_identical(RNGkind(), before_kind)
  expect_identical(.Random.seed, before_seed)
})
