# Internal cache for oracle QST tables
.cs_oracle_qst_cache <- new.env(parent = emptyenv())

cs_get_oracle_qst <- function(dgp_id,
                              version  = "1.3.0",
                              tau_grid = cs_tau_oracle,
                              N_oracle = 1e6L) {
  N_oracle <- getOption("causalstress.N_oracle", N_oracle)
  key <- paste(dgp_id, version, N_oracle, sep = "_")

  if (exists(key, envir = .cs_oracle_qst_cache, inherits = FALSE)) {
    return(get(key, envir = .cs_oracle_qst_cache, inherits = FALSE))
  }

  cs_set_rng(1L)
  sim <- cs_simulate_synth(dgp_id = dgp_id, N = N_oracle)

  w  <- sim$w
  y0 <- sim$y0
  y1 <- sim$y1

  idx  <- w == 1L
  y0_t <- y0[idx]
  y1_t <- y1[idx]

  truth <- tibble::tibble(
    tau   = tau_grid,
    value = vapply(
      tau_grid,
      function(u) {
        stats::quantile(y1_t, u, type = 7) -
          stats::quantile(y0_t, u, type = 7)
      },
      numeric(1)
    )
  )

  assign(key, truth, envir = .cs_oracle_qst_cache)
  truth
}

cs_simulate_synth <- function(dgp_id, N) {
  if (dgp_id == "synth_baseline") {
    cs_simulate_synth_baseline(N)
  } else if (dgp_id == "synth_heavytail") {
    cs_simulate_synth_heavytail(N)
  } else {
    rlang::abort(
      glue::glue("No synthetic simulator registered for DGP '{dgp_id}'."),
      class = "causalstress_dgp_error"
    )
  }
}

cs_simulate_synth_baseline <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- 1 + 0.5 * X1
  p   <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w   <- stats::rbinom(N, size = 1, prob = p)

  eps0 <- stats::rnorm(N, mean = 0, sd = 0.5)
  eps1 <- stats::rnorm(N, mean = 0, sd = 0.5)

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_heavytail <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- 1 + 0.5 * X1
  p   <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w   <- stats::rbinom(N, size = 1, prob = p)

  mix0 <- stats::rbinom(N, size = 1, prob = 0.8)
  mix1 <- stats::rbinom(N, size = 1, prob = 0.8)

  eps0 <- ifelse(
    mix0 == 1L,
    stats::rnorm(N, mean = 0, sd = 0.5),
    stats::rcauchy(N, location = 0, scale = 1)
  )

  eps1 <- ifelse(
    mix1 == 1L,
    stats::rnorm(N, mean = 0, sd = 0.5),
    stats::rcauchy(N, location = 0, scale = 1)
  )

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
    p = p, structural_te = tau
  )
}
