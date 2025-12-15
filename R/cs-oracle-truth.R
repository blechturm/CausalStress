#' Oracle QST computation with caching
#'
#' Computes oracle quantile treatment effects by calling the registered DGP
#' generators. Results are cached to avoid repeated simulations.
#' @keywords internal

.cs_oracle_qst_cache <- new.env(parent = emptyenv())
ORACLE_SEED <- 99999L

cs_get_oracle_qst <- function(dgp_id,
                              version  = "1.3.0",
                              tau_grid = cs_tau_oracle,
                              N_oracle = 1e6L) {
  N_oracle <- getOption("causalstress.N_oracle", N_oracle)
  key <- paste(dgp_id, version, N_oracle, sep = "_")

  if (exists(key, envir = .cs_oracle_qst_cache, inherits = FALSE)) {
    val <- get(key, envir = .cs_oracle_qst_cache, inherits = FALSE)
    if (is.data.frame(val)) return(val)
    if (is.character(val) && identical(val, "CALCULATING")) {
      return(tibble::tibble(tau = tau_grid, value = rep(NA_real_, length(tau_grid))))
    }
  }

  # recursion guard
  assign(key, "CALCULATING", envir = .cs_oracle_qst_cache)
  on.exit({
    cur <- get(key, envir = .cs_oracle_qst_cache, inherits = FALSE)
    if (is.character(cur) && identical(cur, "CALCULATING")) {
      remove(list = key, envir = .cs_oracle_qst_cache)
    }
  }, add = TRUE)

  # preserve global RNG state
  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      remove(list = ".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  # Optimizations for simple cases
  if (startsWith(dgp_id, "synth_placebo")) {
    truth <- tibble::tibble(tau = tau_grid, value = rep(0, length(tau_grid)))
    assign(key, truth, envir = .cs_oracle_qst_cache)
    return(truth)
  }
  if (dgp_id == "synth_hd_sparse_plm") {
    truth <- tibble::tibble(tau = tau_grid, value = rep(1, length(tau_grid)))
    assign(key, truth, envir = .cs_oracle_qst_cache)
    return(truth)
  }

  # General case: simulate via registered DGP
  dgp_desc <- cs_get_dgp(dgp_id, version = version)
  gen <- dgp_desc$generator[[1]]
  sim <- gen(n = N_oracle, seed = ORACLE_SEED)
  df <- if (!is.null(sim$df)) sim$df else sim

  w  <- df$w
  y0 <- df$y0
  y1 <- df$y1

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
