#' Run a DGP × estimator grid over multiple seeds
#'
#' @param dgp_ids Character vector of DGP IDs (must exist in cs_dgp_registry()).
#' @param estimator_ids Character vector of estimator IDs (must exist in cs_estimator_registry()).
#' @param n Integer, sample size per run (same for all combinations).
#' @param seeds Integer vector of seeds to run for each (DGP, estimator) pair.
#' @param tau Optional numeric vector of quantile levels (forwarded to cs_run_single()).
#' @param config Optional named list of configuration settings (forwarded to cs_run_single()).
#'
#' @return A tibble with one row per (dgp_id, estimator_id, seed) run, including
#'   all columns from cs_run_single(), and bindable into cs_summarise_runs().
#' @export
cs_run_grid <- function(dgp_ids,
                        estimator_ids,
                        n,
                        seeds,
                        tau = NULL,
                        bootstrap = FALSE,
                        B = 200L,
                        config = list(),
                        board = NULL,
                        skip_existing = FALSE) {
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

  lapply(dgp_ids, cs_get_dgp)
  lapply(estimator_ids, cs_get_estimator)

  grid <- expand.grid(
    dgp_id = dgp_ids,
    estimator_id = estimator_ids,
    stringsAsFactors = FALSE
  )

  runs <- lapply(seq_len(nrow(grid)), function(i) {
    if (is.null(tau)) {
      cs_run_seeds(
        dgp_id       = grid$dgp_id[i],
        estimator_id = grid$estimator_id[i],
        n            = n,
        seeds        = seeds,
        bootstrap    = bootstrap,
        B            = B,
        config       = config,
        board        = board,
        skip_existing = skip_existing
      )
    } else {
      cs_run_seeds(
        dgp_id       = grid$dgp_id[i],
        estimator_id = grid$estimator_id[i],
        n            = n,
        seeds        = seeds,
        tau          = tau,
        bootstrap    = bootstrap,
        B            = B,
        config       = config,
        board        = board,
        skip_existing = skip_existing
      )
    }
  })

  tibble::as_tibble(do.call(rbind, runs))
}
