#' Oracle QST computation with caching
#'
#' Computes oracle quantile treatment effects by calling the registered DGP
#' generators. Results are cached to avoid repeated simulations.
#' @keywords internal

.cs_oracle_qst_guard <- new.env(parent = emptyenv())
ORACLE_SEED <- 99999L
ORACLE_N <- 1e6L

cs_oracle_algorithm_descriptor <- function(dgp_id, version, tau_grid, N_oracle) {
  list(
    oracle_algorithm_version = "1.0.0",
    oracle_seed = ORACLE_SEED,
    oracle_n = as.integer(N_oracle),
    chunk_n = 200000L,
    tau_id = cs_tau_id(tau_grid),
    quantile_type = 7L,
    retention_rule = "retain_treated_until_N_oracle",
    package_version = as.character(utils::packageVersion("CausalStress")),
    dgp_id = dgp_id,
    dgp_version = version
  )
}

cs_oracle_algorithm_fingerprint <- function(descriptor) {
  digest::digest(descriptor, algo = "sha256")
}

cs_oracle_cache_file <- function(cache_dir, dgp_id, version, oracle_algorithm_fingerprint) {
  file.path(
    cache_dir,
    paste0("truth_", dgp_id, "_", version, "_oracle_", oracle_algorithm_fingerprint, ".rds")
  )
}

cs_validate_oracle_cache_payload <- function(payload,
                                             cache_file,
                                             expected_fingerprint,
                                             expected_descriptor) {
  if (!is.list(payload) || !is.list(expected_descriptor)) {
    rlang::abort(
      glue::glue("Oracle RDS cache has invalid structure or identity: {cache_file}"),
      class = "causalstress_oracle_error"
    )
  }
  valid_truth <- is.data.frame(payload$truth) &&
    all(c("tau_id", "tau", "value") %in% names(payload$truth)) &&
    identical(as.character(payload$truth$tau_id), expected_descriptor$tau_id) &&
    is.numeric(payload$truth$tau) &&
    is.numeric(payload$truth$value) &&
    nrow(payload$truth) == length(expected_descriptor$tau_id)
  if (!identical(payload$oracle_algorithm_fingerprint, expected_fingerprint) ||
      !identical(payload$oracle_algorithm_descriptor, expected_descriptor) ||
      !isTRUE(valid_truth)) {
    rlang::abort(
      glue::glue("Oracle RDS cache has invalid structure or identity: {cache_file}"),
      class = "causalstress_oracle_error"
    )
  }
  invisible(payload)
}

cs_oracle_cache_write <- function(payload, cache_file) {
  expected_fingerprint <- payload$oracle_algorithm_fingerprint
  expected_descriptor <- payload$oracle_algorithm_descriptor
  cs_write_rds_atomic(
    payload,
    cache_file,
    validate = function(candidate, candidate_path) {
      cs_validate_oracle_cache_payload(
        candidate,
        candidate_path,
        expected_fingerprint,
        expected_descriptor
      )
    },
    error_class = "causalstress_oracle_error"
  )
}

cs_get_oracle_qst <- function(dgp_id,
                              version  = "1.3.0",
                              tau_grid = cs_tau_oracle,
                              N_oracle = ORACLE_N,
                              cache_dir = tools::R_user_dir("CausalStress", "cache")) {
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

  oracle_descriptor <- cs_oracle_algorithm_descriptor(dgp_id, version, tau_grid, N_oracle)
  oracle_fp <- cs_oracle_algorithm_fingerprint(oracle_descriptor)
  key <- paste(dgp_id, version, oracle_fp, sep = "_")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- cs_oracle_cache_file(cache_dir, dgp_id, version, oracle_fp)

  if (file.exists(cache_file)) {
    payload <- cs_read_rds(cache_file, error_class = "causalstress_oracle_error")
    cs_validate_oracle_cache_payload(
      payload,
      cache_file,
      oracle_fp,
      oracle_descriptor
    )
    return(payload$truth)
  }

  # recursion guard
  if (exists(key, envir = .cs_oracle_qst_guard, inherits = FALSE)) {
    if (identical(get(key, envir = .cs_oracle_qst_guard, inherits = FALSE), "CALCULATING")) {
      return(NULL)
    }
  }
  assign(key, "CALCULATING", envir = .cs_oracle_qst_guard)
  on.exit({
    if (exists(key, envir = .cs_oracle_qst_guard, inherits = FALSE)) {
      cur <- get(key, envir = .cs_oracle_qst_guard, inherits = FALSE)
      if (is.character(cur) && identical(cur, "CALCULATING")) {
        remove(list = key, envir = .cs_oracle_qst_guard)
      }
    }
  }, add = TRUE)

  rng_state <- cs_rng_state_capture()
  on.exit(cs_rng_state_restore(rng_state), add = TRUE)

  # Optimizations for simple cases
  if (startsWith(dgp_id, "synth_placebo")) {
    truth <- tibble::tibble(
      tau_id = cs_tau_id(tau_grid),
      tau = tau_grid,
      value = rep(0, length(tau_grid))
    )
    cs_oracle_cache_write(
      list(
        oracle_algorithm_fingerprint = oracle_fp,
        oracle_algorithm_descriptor = oracle_descriptor,
        truth = truth
      ),
      cache_file
    )
    return(truth)
  }
  if (dgp_id == "synth_hd_sparse_plm") {
    truth <- tibble::tibble(
      tau_id = cs_tau_id(tau_grid),
      tau = tau_grid,
      value = rep(1, length(tau_grid))
    )
    cs_oracle_cache_write(
      list(
        oracle_algorithm_fingerprint = oracle_fp,
        oracle_algorithm_descriptor = oracle_descriptor,
        truth = truth
      ),
      cache_file
    )
    return(truth)
  }

  # General case: simulate from treated population distribution X|W=1.
  # We obtain N_oracle treated draws by sampling batches from the DGP and
  # retaining treated units until we reach N_oracle.
  dgp_desc <- cs_get_dgp(dgp_id, version = version, quiet = TRUE)
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

  cs_oracle_cache_write(
    list(
      oracle_algorithm_fingerprint = oracle_fp,
      oracle_algorithm_descriptor = oracle_descriptor,
      truth = truth
    ),
    cache_file
  )
  truth
}
