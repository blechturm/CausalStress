#' Oracle QST computation with caching
#'
#' Computes oracle quantile treatment effects by calling the registered DGP
#' generators. Results are cached to avoid repeated simulations.
#' @keywords internal

.cs_oracle_qst_cache <- new.env(parent = emptyenv())
ORACLE_SEED <- 99999L
ORACLE_N <- 1e6L

cs_get_oracle_qst <- function(dgp_id,
                              version  = "1.3.0",
                              tau_grid = cs_tau_oracle,
                              N_oracle = ORACLE_N) {
  # Constitution: canonical tau grid and immutable oracle MC size.
  if (!identical(cs_tau_id(tau_grid), cs_tau_id(cs_tau_oracle))) {
    rlang::abort(
      "`tau_grid` must equal the canonical cs_tau_oracle() grid for oracle truth.",
      class = "causalstress_oracle_error"
    )
  }

  N_oracle <- as.integer(N_oracle)
  if (!identical(N_oracle, as.integer(ORACLE_N))) {
    rlang::abort(
      glue::glue("Oracle truth MC size is immutable (N_oracle must be {ORACLE_N})."),
      class = "causalstress_oracle_error"
    )
  }

  key <- paste(dgp_id, version, sep = "_")

  if (exists(key, envir = .cs_oracle_qst_cache, inherits = FALSE)) {
    val <- get(key, envir = .cs_oracle_qst_cache, inherits = FALSE)
    if (is.data.frame(val)) return(val)
    if (is.character(val) && identical(val, "CALCULATING")) {
      return(tibble::tibble(
        tau_id = cs_tau_id(tau_grid),
        tau = tau_grid,
        value = rep(NA_real_, length(tau_grid))
      ))
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
    truth <- tibble::tibble(
      tau_id = cs_tau_id(tau_grid),
      tau = tau_grid,
      value = rep(0, length(tau_grid))
    )
    assign(key, truth, envir = .cs_oracle_qst_cache)
    return(truth)
  }
  if (dgp_id == "synth_hd_sparse_plm") {
    truth <- tibble::tibble(
      tau_id = cs_tau_id(tau_grid),
      tau = tau_grid,
      value = rep(1, length(tau_grid))
    )
    assign(key, truth, envir = .cs_oracle_qst_cache)
    return(truth)
  }

  # General case: simulate from treated population distribution X|W=1.
  # We obtain N_oracle treated draws by sampling batches from the DGP and
  # retaining treated units until we reach N_oracle.
  dgp_desc <- cs_get_dgp(dgp_id, version = version)
  gen <- dgp_desc$generator[[1]]

  cs_set_rng(ORACLE_SEED)

  chunk_n <- 200000L
  y0_t <- numeric(0L)
  y1_t <- numeric(0L)
  while (length(y0_t) < N_oracle) {
    sim <- gen(n = chunk_n, seed = NULL, include_truth = FALSE, oracle_only = TRUE)
    df <- if (!is.null(sim$df)) sim$df else sim

    w  <- df$w
    y0 <- df$y0
    y1 <- df$y1

    idx <- w == 1L
    if (any(idx)) {
      y0_t <- c(y0_t, y0[idx])
      y1_t <- c(y1_t, y1[idx])
    }
  }

  y0_t <- y0_t[seq_len(N_oracle)]
  y1_t <- y1_t[seq_len(N_oracle)]

  truth <- tibble::tibble(
    tau_id = cs_tau_id(tau_grid),
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
