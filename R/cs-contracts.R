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
  # top-level structure
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
  df <- dgp$df
  if (!is.data.frame(df)) {
    rlang::abort(
      message = "`df` must be a data.frame (or tibble).",
      class   = "causalstress_dgp_error"
    )
  }

  n <- nrow(df)

  # required core columns per registry: y, w, y0, y1, p
  required_cols <- c("y", "w", "y0", "y1", "p")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    rlang::abort(
      message = glue::glue(
        "DGP data frame is missing required columns: {toString(missing_cols)}"
      ),
      class   = "causalstress_dgp_error"
    )
  }

  if (anyNA(df[required_cols])) {
    rlang::abort(
      message = "Core DGP columns `y`, `w`, `y0`, `y1`, `p` must not contain NA.",
      class   = "causalstress_dgp_error"
    )
  }

  # structural_te column is optional but, if present, must be non-NA
  if ("structural_te" %in% names(df)) {
    if (anyNA(df[["structural_te"]])) {
      rlang::abort(
        message = "Column `structural_te` must not contain NA when present.",
        class   = "causalstress_dgp_error"
      )
    }
  }

  # treatment indicator well-formed
  w_vals <- df$w
  if (!(is.numeric(w_vals) || is.integer(w_vals)) || !all(w_vals %in% c(0, 1))) {
    rlang::abort(
      message = "`w` must be numeric/integer and contain only 0/1.",
      class   = "causalstress_dgp_error"
    )
  }

  # true_att
  cs_chk_scalar_numeric(dgp$true_att, "true_att")

  # true_qst
  tq <- dgp$true_qst
  if (!tibble::is_tibble(tq)) {
    rlang::abort(
      message = "`true_qst` must be a tibble with columns `tau` and `value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!all(c("tau", "value") %in% names(tq))) {
    rlang::abort(
      message = "`true_qst` must have columns `tau` and `value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.numeric(tq$tau) ||
      length(tq$tau) != length(tq$value)) {
    rlang::abort(
      message = "`true_qst$tau` must be numeric and match length of `true_qst$value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!identical(cs_tau_id(tq$tau), cs_tau_id(cs_tau_oracle))) {
    rlang::abort(
      message = "`true_qst$tau` must equal the canonical cs_tau_oracle() grid.",
      class   = "causalstress_dgp_error"
    )
  }

  # meta
  meta <- dgp$meta
  if (!is.list(meta)) {
    rlang::abort(
      message = "`meta` must be a list.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.character(meta$dgp_id) || length(meta$dgp_id) != 1L) {
    rlang::abort(
      message = "`meta$dgp_id` must be a character scalar.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!identical(meta$type, "synthetic")) {
    rlang::abort(
      message = "`meta$type` must be \"synthetic\" for synthetic DGPs.",
      class   = "causalstress_dgp_error"
    )
  }

  # meta$structural_te is required for synthetic DGPs
  if (is.null(meta$structural_te)) {
    rlang::abort(
      message = "`meta$structural_te` must be provided for synthetic DGPs.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.numeric(meta$structural_te)) {
    rlang::abort(
      message = "`meta$structural_te` must be a numeric vector.",
      class   = "causalstress_dgp_error"
    )
  }
  if (length(meta$structural_te) != n) {
    rlang::abort(
      message = "`meta$structural_te` must have length equal to nrow(df).",
      class   = "causalstress_dgp_error"
    )
  }
  if (anyNA(meta$structural_te)) {
    rlang::abort(
      message = "`meta$structural_te` must not contain NA.",
      class   = "causalstress_dgp_error"
    )
  }

  # if df also has structural_te, enforce consistency
  if ("structural_te" %in% names(df)) {
    if (!isTRUE(all.equal(meta$structural_te, df$structural_te))) {
      rlang::abort(
        message = "`meta$structural_te` must match `df$structural_te` when both are present.",
        class   = "causalstress_dgp_error"
      )
    }
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
  normalize_qst <- function(qst) {
    if (!tibble::is_tibble(qst) || !"tau" %in% names(qst)) {
      rlang::abort(
        message = "`qst` must be a tibble with a `tau` column.",
        class   = "causalstress_estimator_error"
      )
    }
    if ("estimate" %in% names(qst)) return(qst)
    if ("value" %in% names(qst)) return(dplyr::rename(qst, estimate = value))
    rlang::abort(
      message = "`qst` must contain an `estimate` (or legacy `value`) column.",
      class   = "causalstress_estimator_error"
    )
  }

  if (require_qst && is.null(res$qst)) {
    rlang::abort(
      message = "Estimator must return `qst` when `require_qst = TRUE`.",
      class   = "causalstress_estimator_error"
    )
  }

  if (!is.null(res$qst)) {
    qst_tbl <- normalize_qst(res$qst)
    if (!is.numeric(qst_tbl$tau) || !is.numeric(qst_tbl$estimate)) {
      rlang::abort(
        message = "`qst$tau` and `qst$estimate` must be numeric.",
        class   = "causalstress_estimator_error"
      )
    }
    if (anyNA(qst_tbl$tau) || anyNA(qst_tbl$estimate)) {
      rlang::abort(
        message = "`qst$tau` and `qst$estimate` must not contain NA.",
        class   = "causalstress_estimator_error"
      )
    }
    if (!is.null(tau)) {
      if (!identical(cs_tau_id(qst_tbl$tau), cs_tau_id(tau))) {
        rlang::abort(
          message = "`qst$tau` must match the requested tau grid exactly (same values and order).",
          class   = "causalstress_estimator_error"
        )
      }
    }
  }

  invisible(TRUE)
}
