#' @noRd
cs_chk_scalar_numeric <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a finite numeric scalar."),
      class   = "causalstress_contract_error"
    )
  }
}

#' @noRd
cs_check_dgp_synthetic <- function(dgp) {
  if (!is.list(dgp)) {
    rlang::abort(
      message = "`dgp` must be a list returned by a DGP generator.",
      class   = "causalstress_dgp_error"
    )
  }

  required <- c("df", "true_att", "true_qst", "meta")
  missing <- setdiff(required, names(dgp))
  if (length(missing) > 0) {
    rlang::abort(
      message = glue::glue(
        "DGP output is missing required components: {toString(missing)}"
      ),
      class   = "causalstress_dgp_error"
    )
  }

  # df checks
  if (!is.data.frame(dgp$df)) {
    rlang::abort(
      message = "`df` must be a data.frame (or tibble).",
      class   = "causalstress_dgp_error"
    )
  }

  required_cols <- c("y", "w", "y0", "y1", "structural_te")
  missing_cols <- setdiff(required_cols, names(dgp$df))
  if (length(missing_cols) > 0) {
    rlang::abort(
      message = glue::glue(
        "DGP data frame is missing required columns: {toString(missing_cols)}"
      ),
      class   = "causalstress_dgp_error"
    )
  }

  if (anyNA(dgp$df[required_cols])) {
    rlang::abort(
      message = "Core DGP columns `y`, `w`, `y0`, `y1`, `structural_te` must not contain NA.",
      class   = "causalstress_dgp_error"
    )
  }

  w_vals <- dgp$df$w
  if (!(is.numeric(w_vals) || is.integer(w_vals)) || !all(w_vals %in% c(0, 1))) {
    rlang::abort(
      message = "`w` must be numeric/integer and contain only 0/1.",
      class   = "causalstress_dgp_error"
    )
  }

  # true_att
  cs_chk_scalar_numeric(dgp$true_att, "true_att")

  # true_qst
  if (!tibble::is_tibble(dgp$true_qst)) {
    rlang::abort(
      message = "`true_qst` must be a tibble with columns `tau` and `value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!all(c("tau", "value") %in% names(dgp$true_qst))) {
    rlang::abort(
      message = "`true_qst` must have columns `tau` and `value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.numeric(dgp$true_qst$tau) ||
      length(dgp$true_qst$tau) != length(dgp$true_qst$value)) {
    rlang::abort(
      message = "`true_qst$tau` must be numeric and match length of `true_qst$value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!all(dgp$true_qst$tau %in% cs_tau_oracle)) {
    rlang::abort(
      message = "`true_qst$tau` must be a subset of the oracle tau grid.",
      class   = "causalstress_dgp_error"
    )
  }

  # meta
  if (!is.list(dgp$meta)) {
    rlang::abort(
      message = "`meta` must be a list.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.character(dgp$meta$dgp_id) || length(dgp$meta$dgp_id) != 1L) {
    rlang::abort(
      message = "`meta$dgp_id` must be a character scalar.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!identical(dgp$meta$type, "synthetic")) {
    rlang::abort(
      message = "`meta$type` must be \"synthetic\" for synthetic DGPs.",
      class   = "causalstress_dgp_error"
    )
  }

  invisible(TRUE)
}

#' @noRd
cs_check_estimator_output <- function(res, require_qst = FALSE, tau = NULL) {
  if (!is.list(res)) {
    rlang::abort(
      message = "`res` must be a list returned by an estimator.",
      class   = "causalstress_estimator_error"
    )
  }

  required <- c("att", "qst", "meta")
  missing <- setdiff(required, names(res))
  if (length(missing) > 0) {
    rlang::abort(
      message = glue::glue(
        "Estimator output is missing required components: {toString(missing)}"
      ),
      class   = "causalstress_estimator_error"
    )
  }

  # meta
  if (!is.list(res$meta)) {
    rlang::abort(
      message = "`meta` must be a list.",
      class   = "causalstress_estimator_error"
    )
  }
  if (!is.character(res$meta$estimator_id) || length(res$meta$estimator_id) != 1L) {
    rlang::abort(
      message = "`meta$estimator_id` must be a character scalar.",
      class   = "causalstress_estimator_error"
    )
  }

  # att
  att <- res$att
  if (is.list(att) && !is.data.frame(att)) {
    est <- att$estimate
  } else if (is.data.frame(att)) {
    est <- att[["estimate"]]
  } else {
    rlang::abort(
      message = "`att` must be a list or data frame with an `estimate` component.",
      class   = "causalstress_estimator_error"
    )
  }
  cs_chk_scalar_numeric(est, "att$estimate")

  # qst
  if (require_qst) {
    if (is.null(res$qst)) {
      rlang::abort(
        message = "Estimator must return `qst` when `require_qst = TRUE`.",
        class   = "causalstress_estimator_error"
      )
    }
    if (!tibble::is_tibble(res$qst) ||
        !all(c("tau", "value") %in% names(res$qst))) {
      rlang::abort(
        message = "`qst` must be a tibble with columns `tau` and `value`.",
        class   = "causalstress_estimator_error"
      )
    }
    if (!is.null(tau) && !all(res$qst$tau %in% tau)) {
      rlang::abort(
        message = "`qst$tau` must be a subset of the requested tau grid.",
        class   = "causalstress_estimator_error"
      )
    }
  } else {
    if (!is.null(res$qst)) {
      if (!tibble::is_tibble(res$qst) ||
          !all(c("tau", "value") %in% names(res$qst))) {
        rlang::abort(
          message = "`qst` must be a tibble with columns `tau` and `value` when provided.",
          class   = "causalstress_estimator_error"
        )
      }
    }
  }

  invisible(TRUE)
}
