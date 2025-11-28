#' Run a single DGP × estimator combination
#'
#' This helper looks up a DGP and an estimator via their registries,
#' generates a synthetic dataset, runs the estimator, and returns a
#' one-row tibble summarizing truth and estimates.
#'
#' @param dgp_id Character scalar, identifier of the DGP (e.g., "synth_baseline").
#' @param estimator_id Character scalar, identifier of the estimator (e.g., "oracle_att").
#' @param n Integer, number of observations to generate.
#' @param seed Optional integer seed passed to the DGP generator.
#' @param tau Numeric vector of quantile levels. Passed to the estimator
#'   for interface compatibility. Default is `cs_tau_oracle`.
#' @param config List of estimator-specific configuration options.
#'
#' @return A tibble with one row and columns including:
#'   - dgp_id, estimator_id
#'   - n, seed
#'   - oracle, supports_qst
#'   - true_att, est_att
#'   - att_error, att_abs_error
#'
#' @export
cs_run_single <- function(
  dgp_id,
  estimator_id,
  n,
  seed   = NULL,
  tau    = cs_tau_oracle,
  config = list()
) {
  if (!is.character(dgp_id) || length(dgp_id) != 1L) {
    rlang::abort(
      message = "`dgp_id` must be a character scalar.",
      class   = "causalstress_runner_error"
    )
  }
  if (!is.character(estimator_id) || length(estimator_id) != 1L) {
    rlang::abort(
      message = "`estimator_id` must be a character scalar.",
      class   = "causalstress_runner_error"
    )
  }
  if (!is.numeric(n) || length(n) != 1L || n <= 0) {
    rlang::abort(
      message = "`n` must be a positive integer scalar.",
      class   = "causalstress_runner_error"
    )
  }

  dgp_desc <- cs_get_dgp(dgp_id)
  if (!is.function(dgp_desc$generator)) {
    rlang::abort(
      message = "`generator` in DGP descriptor must be a function.",
      class   = "causalstress_runner_error"
    )
  }

  dgp <- dgp_desc$generator(n = n, seed = seed)
  cs_check_dgp_synthetic(dgp)

  est_desc <- cs_get_estimator(estimator_id)
  est_fn   <- est_desc$fn

  df <- dgp$df
  est <- est_fn(df = df, tau = tau, config = config)

  cs_check_estimator_output(
    res         = est,
    require_qst = est_desc$supports_qst,
    tau         = tau
  )

  true_att <- dgp$true_att
  est_att  <- est$att$estimate
  att_error <- est_att - true_att
  att_abs_error <- abs(att_error)

  tibble::tibble(
    dgp_id       = dgp_id,
    estimator_id = estimator_id,
    n            = as.integer(n),
    seed         = if (is.null(seed)) NA_integer_ else as.integer(seed),
    oracle       = est_desc$oracle,
    supports_qst = est_desc$supports_qst,
    true_att     = true_att,
    est_att      = est_att,
    att_error    = att_error,
    att_abs_error = att_abs_error
  )
}

#' Run a DGP × estimator combination over multiple seeds
#'
#' This function repeatedly calls [cs_run_single()] for a given DGP and
#' estimator, using a vector of seeds. It returns a tibble with one row
#' per seed and the same columns as [cs_run_single()].
#'
#' @param dgp_id Character scalar, identifier of the DGP (e.g., "synth_baseline").
#' @param estimator_id Character scalar, identifier of the estimator (e.g., "oracle_att").
#' @param n Integer, number of observations to generate per seed.
#' @param seeds Integer vector of seeds to use. Each seed produces one row
#'   in the output tibble.
#' @param tau Numeric vector of quantile levels. Passed through to the
#'   estimator via [cs_run_single()]. Default is [cs_tau_oracle].
#' @param config List of estimator-specific configuration options. Passed
#'   through to the estimator via [cs_run_single()].
#'
#' @return A tibble with one row per seed and at least the columns returned
#'   by [cs_run_single()], including `dgp_id`, `estimator_id`, `n`, `seed`,
#'   `oracle`, `supports_qst`, `true_att`, `est_att`, `att_error`,
#'   `att_abs_error`.
#'
#' @export
cs_run_seeds <- function(
  dgp_id,
  estimator_id,
  n,
  seeds,
  tau    = cs_tau_oracle,
  config = list()
) {
  if (length(seeds) == 0L) {
    rlang::abort(
      message = "`seeds` must have length >= 1.",
      class   = "causalstress_runner_error"
    )
  }
  if (!is.numeric(seeds) && !is.integer(seeds)) {
    rlang::abort(
      message = "`seeds` must be numeric or integer.",
      class   = "causalstress_runner_error"
    )
  }
  if (any(!is.finite(seeds))) {
    rlang::abort(
      message = "`seeds` must be finite.",
      class   = "causalstress_runner_error"
    )
  }
  seeds <- as.integer(seeds)

  rows <- lapply(seeds, function(s) {
    cs_run_single(
      dgp_id       = dgp_id,
      estimator_id = estimator_id,
      n            = n,
      seed         = s,
      tau          = tau,
      config       = config
    )
  })

  tibble::as_tibble(do.call(rbind, rows))
}
