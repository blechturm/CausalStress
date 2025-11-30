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

  if (dgp_id == "synth_placebo_tau0") {
    truth <- tibble::tibble(
      tau   = tau_grid,
      value = rep(0, length(tau_grid))
    )
    assign(key, truth, envir = .cs_oracle_qst_cache)
    return(truth)
  }
  if (dgp_id == "synth_qte1") {
    cs_set_rng(1L)
    sim <- cs_simulate_synth_qte1(N_oracle)

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
    return(truth)
  }
  if (dgp_id == "synth_overlap_stressed") {
    cs_set_rng(1L)
    sim <- cs_simulate_synth_overlap_stressed(N_oracle)

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
    return(truth)
  }
  if (dgp_id == "synth_nonlinear_heteroskedastic") {
    cs_set_rng(1L)
    sim <- cs_simulate_synth_nonlinear_heteroskedastic(N_oracle)

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
    return(truth)
  }
  if (dgp_id == "synth_overlap_stressed") {
    cs_set_rng(1L)
    sim <- cs_simulate_synth_overlap_stressed(N_oracle)

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
    return(truth)
  }
  if (dgp_id == "synth_tilt_mild") {
    cs_set_rng(1L)
    sim <- cs_simulate_synth_tilt_mild(N_oracle)

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
    return(truth)
  }
  if (dgp_id %in% c(
    "synth_placebo_nonlinear",
    "synth_placebo_heavytail",
    "synth_placebo_tilted",
    "synth_placebo_kangschafer"
  )) {
    truth <- tibble::tibble(
      tau   = tau_grid,
      value = rep(0, length(tau_grid))
    )
    cs_validate_tau_grid(truth)
    assign(key, truth, envir = .cs_oracle_qst_cache)
    return(truth)
  }
  if (dgp_id == "synth_hd_sparse_plm") {
    truth <- tibble::tibble(
      tau   = tau_grid,
      value = rep(1, length(tau_grid))
    )
    cs_validate_tau_grid(truth)
    assign(key, truth, envir = .cs_oracle_qst_cache)
    return(truth)
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

  cs_validate_tau_grid(truth)
  assign(key, truth, envir = .cs_oracle_qst_cache)
  truth
}

cs_simulate_synth <- function(dgp_id, N) {
  if (dgp_id == "synth_baseline") {
    cs_simulate_synth_baseline(N)
  } else if (dgp_id == "synth_heavytail") {
    cs_simulate_synth_heavytail(N)
  } else if (dgp_id == "synth_placebo_tau0") {
    cs_simulate_synth_placebo_tau0(N)
  } else if (dgp_id == "synth_qte1") {
    cs_simulate_synth_qte1(N)
  } else if (dgp_id == "synth_overlap_stressed") {
    cs_simulate_synth_overlap_stressed(N)
  } else if (dgp_id == "synth_nonlinear_heteroskedastic") {
    cs_simulate_synth_nonlinear_heteroskedastic(N)
  } else if (dgp_id == "synth_tilt_mild") {
    cs_simulate_synth_tilt_mild(N)
  } else if (dgp_id == "synth_placebo_nonlinear") {
    cs_simulate_synth_placebo_nonlinear(N)
  } else if (dgp_id == "synth_placebo_heavytail") {
    cs_simulate_synth_placebo_heavytail(N)
  } else if (dgp_id == "synth_placebo_tilted") {
    cs_simulate_synth_placebo_tilted(N)
  } else if (dgp_id == "synth_placebo_kangschafer") {
    cs_simulate_synth_placebo_kangschafer(N)
  } else if (dgp_id == "synth_hd_sparse_plm") {
    cs_simulate_synth_hd_sparse_plm(N)
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

cs_simulate_synth_placebo_tau0 <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- rep(0, N)
  p   <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w <- stats::rbinom(N, size = 1, prob = p)

  eps <- stats::rnorm(N, mean = 0, sd = 0.5)

  y0 <- mu0 + eps
  y1 <- y0

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_qte1 <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- ifelse(X1 > 0, 1, -1)
  p   <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w <- stats::rbinom(N, size = 1, prob = p)

  eps0 <- 0.5 * stats::rt(N, df = 4)
  eps1 <- 0.5 * stats::rt(N, df = 4)

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_overlap_stressed <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- 1 + 0.5 * X1
  p   <- stats::plogis(3.0 * X1 + 3.0 * X2)

  w <- stats::rbinom(N, size = 1, prob = p)

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

cs_simulate_synth_nonlinear_heteroskedastic <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::runif(N, min = -2, max = 2)
  X4 <- stats::rbinom(N, size = 1L, prob = 0.4)

  mu0 <- 1 + 0.8 * sin(X1) + 0.5 * X2^2 - 0.3 * X4
  tau <- rep(1.0, N)

  sigma <- 0.3 + 0.2 * abs(X3)
  eps0  <- stats::rnorm(N, mean = 0, sd = sigma)
  eps1  <- stats::rnorm(N, mean = 0, sd = sigma)

  p <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w <- stats::rbinom(N, size = 1L, prob = p)

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_tilt_mild <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- 1 + 0.5 * X1
  p   <- stats::plogis(0.45 * X1 - 0.3 * X2 - 0.25 * X4)

  w <- stats::rbinom(N, size = 1, prob = p)

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

cs_simulate_synth_placebo_nonlinear <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- sin(X1) + cos(X2)
  tau <- rep(0, N)

  p <- stats::plogis(0.5 * X1 - 0.5 * X2)
  w <- stats::rbinom(N, size = 1, prob = p)

  eps <- stats::rnorm(N, mean = 0, sd = 0.5)

  y0 <- mu0 + eps
  y1 <- y0

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_placebo_heavytail <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- rep(0, N)

  p <- stats::plogis(0.5 * X1 - 0.5 * X2)
  w <- stats::rbinom(N, size = 1, prob = p)

  mix_ind0 <- stats::rbinom(N, size = 1, prob = 0.8)
  eps <- ifelse(
    mix_ind0 == 1L,
    stats::rnorm(N, mean = 0, sd = 0.5),
    stats::rcauchy(N, location = 0, scale = 1)
  )

  y0 <- mu0 + eps
  y1 <- y0

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_placebo_tilted <- function(N) {
  X1 <- stats::rnorm(N, mean = 0, sd = 1)
  X2 <- stats::rnorm(N, mean = 0, sd = 1)
  X3 <- stats::rnorm(N, mean = 0, sd = 1)
  X4 <- stats::rnorm(N, mean = 0, sd = 1)
  X5 <- stats::rnorm(N, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- rep(0, N)

  p <- stats::plogis(1.0 * X1 + 1.2 * X2)
  w <- stats::rbinom(N, size = 1, prob = p)

  eps <- stats::rnorm(N, mean = 0, sd = 0.5)

  y0 <- mu0 + eps
  y1 <- y0

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_placebo_kangschafer <- function(N) {
  Z1 <- stats::rnorm(N, mean = 0, sd = 1)
  Z2 <- stats::rnorm(N, mean = 0, sd = 1)
  Z3 <- stats::rnorm(N, mean = 0, sd = 1)
  Z4 <- stats::rnorm(N, mean = 0, sd = 1)

  X1 <- exp(Z1 / 2)
  X2 <- Z2 / (1 + exp(Z1)) + 10
  X3 <- (Z1 * Z3 / 25 + 0.6)^3
  X4 <- (Z2 + Z4 + 20)^2

  lin_ps <- -Z1 + 0.5 * Z2 - 0.25 * Z3 - 0.1 * Z4
  p      <- stats::plogis(lin_ps)
  w      <- stats::rbinom(N, size = 1L, prob = p)

  mu0 <- 210 + 27.4 * Z1 + 13.7 * Z2 + 13.7 * Z3 + 13.7 * Z4
  eps <- stats::rnorm(N, mean = 0, sd = 1)

  y0 <- mu0 + eps
  y1 <- y0

  tau <- rep(0, N)

  list(
    y0 = y0, y1 = y1, w = w,
    X1 = X1, X2 = X2, X3 = X3, X4 = X4,
    p = p, structural_te = tau
  )
}

cs_simulate_synth_hd_sparse_plm <- function(N) {
  p_hd <- 50L
  idx <- seq_len(p_hd)
  Sigma <- outer(idx, idx, function(i, j) 0.5 ^ abs(i - j))
  L <- chol(Sigma)
  Z <- matrix(stats::rnorm(N * p_hd), nrow = N)
  X <- Z %*% L

  beta_y <- c(rep(1, 5), rep(0, p_hd - 5))
  mu0    <- as.numeric(X %*% beta_y)
  eps    <- stats::rnorm(N, mean = 0, sd = 1)
  y0     <- mu0 + eps

  tau <- rep(1, N)
  y1  <- y0 + tau

  gamma <- c(0.5, -0.5, 0.25, -0.25, 0.1, rep(0, p_hd - 5))
  lin_ps <- as.numeric(X %*% gamma)
  p      <- stats::plogis(lin_ps)
  w      <- stats::rbinom(N, size = 1L, prob = p)

  list(
    y0 = y0, y1 = y1, w = w,
    p = p, structural_te = tau,
    X = X
  )
}
