#' Validate a DGP for schema, determinism, and sanity
#'
#' @param dgp_fn Function to generate the DGP. Must accept `n` and `seed`.
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
  stopifnot(is.function(dgp_fn))

  checks <- c(schema = FALSE, determinism = FALSE)

  run_once <- dgp_fn(n = n, seed = seeds[[1]])
  df <- if (is.data.frame(run_once)) run_once else run_once$df

  required_cols <- c("y", "w")
  has_required <- all(required_cols %in% names(df))

  # locate truth components
  true_att_scalar <- if (!is.null(run_once$true_att)) run_once$true_att else {
    if ("true_att" %in% names(df)) df$true_att else NA_real_
  }
  true_y0 <- df$true_y0 %||% df$y0
  true_y1 <- df$true_y1 %||% df$y1

  schema_ok <- is.data.frame(df) &&
    has_required &&
    is.numeric(df$y) &&
    is.numeric(df$w) &&
    !any(is.na(true_att_scalar)) &&
    !any(is.na(true_y0)) &&
    !any(is.na(true_y1))

  checks["schema"] <- schema_ok

  # determinism: same seed -> identical output
  run_one <- dgp_fn(n = n, seed = 123)
  run_two <- dgp_fn(n = n, seed = 123)
  determ_ok <- identical(run_one, run_two)
  checks["determinism"] <- determ_ok

  # stability across seeds
  att_values <- numeric(length(seeds))
  naive_bias <- numeric(length(seeds))
  for (i in seq_along(seeds)) {
    sim <- dgp_fn(n = n, seed = seeds[[i]])
    df_i <- if (is.data.frame(sim)) sim else sim$df
    true_att_i <- if (!is.null(sim$true_att)) sim$true_att else {
      if ("true_att" %in% names(df_i)) mean(df_i$true_att) else NA_real_
    }
    att_values[[i]] <- true_att_i
    naive_est <- mean(df_i$y[df_i$w == 1]) - mean(df_i$y[df_i$w == 0])
    naive_bias[[i]] <- naive_est - true_att_i
  }
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
