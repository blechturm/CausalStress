#' Run a DGP × estimator grid over multiple seeds
#'
#' @param dgp_ids Character vector of DGP IDs (must exist in cs_dgp_registry()).
#' @param estimator_ids Character vector of estimator IDs (must exist in cs_estimator_registry()).
#' @param n Integer, sample size per run (same for all combinations).
#' @param seeds Integer vector of seeds to run for each (DGP, estimator) pair.
#' @param tau Optional numeric vector of quantile levels (forwarded to cs_run_single()).
#' @param version Optional DGP version string; forwarded to [cs_get_dgp()].
#' @param status Optional DGP status filter; forwarded to [cs_get_dgp()].
#' @param config Optional named list of configuration settings (forwarded to cs_run_single()).
#' @param bootstrap Logical, whether to run bootstrap for ATT CIs.
#' @param B Integer, number of bootstrap replicates.
#' @param board Optional pins board for persistence.
#' @param skip_existing Logical, whether to resume from existing pins.
#' @param force Logical, whether to overwrite existing pins (alias for
#'   `skip_existing = FALSE`).
#' 
#' @return A tibble with one row per (dgp_id, estimator_id, seed) run, including
#'   all columns from cs_run_single(), and bindable into cs_summarise_runs().
#' @export
cs_run_grid <- function(dgp_ids,
                        estimator_ids,
                        n,
                        seeds,
                        tau = NULL,
                        version = NULL,
                        status = "stable",
                        bootstrap = FALSE,
                        B = 200L,
                        config = list(),
                        board = NULL,
                        skip_existing = FALSE,
                        force = FALSE,
                        max_runtime = Inf,
                        parallel = FALSE) {
  cs_chk_scalar_numeric(n, "n")
  if (n <= 0) {
    rlang::abort("`n` must be a positive scalar.", class = "causalstress_contract_error")
  }

  if (!is.character(dgp_ids) || length(dgp_ids) == 0L) {
    rlang::abort("`dgp_ids` must be a non-empty character vector.", class = "causalstress_contract_error")
  }
  if (!is.character(estimator_ids) || length(estimator_ids) == 0L) {
    rlang::abort("`estimator_ids` must be a non-empty character vector.", class = "causalstress_contract_error")
  }
  if (!(is.numeric(seeds) || is.integer(seeds)) || length(seeds) == 0L) {
    rlang::abort("`seeds` must be a non-empty numeric/integer vector.", class = "causalstress_contract_error")
  }

  lapply(dgp_ids, cs_get_dgp, version = version, status = status, quiet = FALSE)
  lapply(estimator_ids, cs_get_estimator)

  grid <- expand.grid(
    dgp_id = dgp_ids,
    estimator_id = estimator_ids,
    stringsAsFactors = FALSE
  )

  runs <- lapply(seq_len(nrow(grid)), function(i) {
    message(glue::glue("Running batch: {grid$dgp_id[i]} x {grid$estimator_id[i]}"))
    if (is.null(tau)) {
      cs_run_seeds(
        dgp_id       = grid$dgp_id[i],
        estimator_id = grid$estimator_id[i],
        n            = n,
        seeds        = seeds,
        version      = version,
        status       = status,
        bootstrap    = bootstrap,
        B            = B,
        config       = config,
        board        = board,
        skip_existing = skip_existing,
        force         = force,
        show_progress = FALSE,
        quiet         = TRUE,
        max_runtime   = max_runtime,
        parallel      = parallel
      )
    } else {
      cs_run_seeds(
        dgp_id       = grid$dgp_id[i],
        estimator_id = grid$estimator_id[i],
        n            = n,
        seeds        = seeds,
        version      = version,
        status       = status,
        tau          = tau,
        bootstrap    = bootstrap,
        B            = B,
        config       = config,
        board        = board,
        skip_existing = skip_existing,
        force         = force,
        show_progress = FALSE,
        quiet         = TRUE,
        max_runtime   = max_runtime,
        parallel      = parallel
      )
    }
  })

  tibble::as_tibble(do.call(rbind, runs))
}
