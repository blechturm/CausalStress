#' Validate a synthetic DGP for schema, determinism, and sanity
#'
#' The generator must return the canonical synthetic-DGP contract, including
#' uppercase, consecutive, one-based covariates named `X1`, ..., `Xk`.
#' Structural contract violations abort with class `causalstress_dgp_error`.
#'
#' @param dgp_fn Synthetic DGP generator. Must accept `n` and `seed`.
#' @param n Integer sample size for validation runs.
#' @param seeds Integer vector of seeds to probe stability/sanity.
#' @param verbose Logical; if TRUE, messages are emitted.
#'
#' @return A list with elements:
#'   - `valid` (logical): TRUE if schema and determinism checks pass.
#'   - `cv_true_att` (numeric): coefficient of variation of true ATT across seeds.
#'   - `mean_naive_bias` (numeric): average naive difference-in-means bias.
#'   - `checks` (named logical vector): individual check outcomes.
#' @export
cs_validate_dgp <- function(dgp_fn, n = 1000, seeds = 1:50, verbose = TRUE) {
  rng_state <- cs_rng_state_capture()
  on.exit(cs_rng_state_restore(rng_state), add = TRUE)

  stopifnot(is.function(dgp_fn))
  if (length(seeds) == 0L) {
    rlang::abort(
      "`seeds` must contain at least one seed.",
      class = "causalstress_contract_error"
    )
  }

  # Validate every configured seed once while computing stability statistics.
  att_values <- numeric(length(seeds))
  naive_bias <- numeric(length(seeds))
  for (i in seq_along(seeds)) {
    sim <- dgp_fn(n = n, seed = seeds[[i]])
    cs_check_dgp_synthetic(sim)
    df_i <- sim$df
    true_att_i <- sim$true_att
    att_values[[i]] <- true_att_i
    naive_est <- mean(df_i$y[df_i$w == 1]) - mean(df_i$y[df_i$w == 0])
    naive_bias[[i]] <- naive_est - true_att_i
  }

  # Determinism: two independent runs at a fixed seed must be identical.
  run_one <- dgp_fn(n = n, seed = 123)
  run_two <- dgp_fn(n = n, seed = 123)
  cs_check_dgp_synthetic(run_one)
  cs_check_dgp_synthetic(run_two)

  checks <- c(
    schema = TRUE,
    determinism = identical(run_one, run_two)
  )

  cv_true_att <- stats::sd(att_values, na.rm = TRUE) / abs(mean(att_values, na.rm = TRUE))
  mean_naive_bias <- mean(naive_bias, na.rm = TRUE)

  valid <- all(checks)

  if (verbose) {
    msg <- glue::glue(
      "Schema: {checks['schema']}; Determinism: {checks['determinism']}; CV(true_att)={round(cv_true_att, 4)}; mean naive bias={round(mean_naive_bias, 4)}"
    )
    message(msg)
  }

  list(
    valid = valid,
    cv_true_att = cv_true_att,
    mean_naive_bias = mean_naive_bias,
    checks = checks
  )
}
