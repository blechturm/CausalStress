cs_result_to_row <- function(result) {
  att  <- result$att %||% list()
  meta <- result$meta %||% list()
  prov <- result$provenance %||% list()

  collapsed_flag <- NA
  if (!is.null(meta$collapsed) && length(meta$collapsed) > 0L) {
    collapsed_flag <- meta$collapsed[[1L]]
  }

  qst_collapsed_flag <- NA
  if (!is.null(meta$qst_ci_collapsed) && length(meta$qst_ci_collapsed) > 0L) {
    qst_collapsed_flag <- meta$qst_ci_collapsed[[1L]]
  }

  tibble::tibble(
    success       = meta$success %||% NA,
    error         = meta$error %||% NA_character_,
    dgp_id         = meta$dgp_id %||% NA_character_,
    estimator_id   = meta$estimator_id %||% NA_character_,
    n              = meta$n %||% NA_integer_,
    seed           = meta$seed %||% NA_integer_,
    oracle         = meta$oracle %||% NA,
    supports_qst   = meta$supports_qst %||% NA,
    true_att       = att$true %||% NA_real_,
    est_att        = att$estimate %||% NA_real_,
    att_error      = att$error %||% NA_real_,
    att_abs_error  = att$abs_error %||% NA_real_,
    att_ci_lo      = att$ci_lo %||% NA_real_,
    att_ci_hi      = att$ci_hi %||% NA_real_,
    att_covered    = att$boot_covered %||% NA,
    att_ci_width   = att$ci_width %||% NA_real_,
    att_ci_method  = meta$ci_method %||% NA_character_,
    att_ci_type    = meta$ci_type %||% NA_character_,
    att_ci_level   = meta$ci_level %||% NA_real_,
    att_ci_valid   = meta$ci_valid %||% NA,
    att_ci_fail_code = meta$ci_fail_code %||% NA_character_,
    att_ci_collapsed = collapsed_flag,
    att_ci_valid_by_dim = list(meta$ci_valid_by_dim %||% logical(0)),
    qst_ci_method    = meta$qst_ci_method %||% NA_character_,
    qst_ci_type      = meta$qst_ci_type %||% NA_character_,
    qst_ci_level     = meta$qst_ci_level %||% NA_real_,
    qst_ci_valid     = meta$qst_ci_valid %||% NA,
    qst_ci_fail_code = meta$qst_ci_fail_code %||% NA_character_,
    qst_ci_collapsed = qst_collapsed_flag,
    qst_ci_valid_by_dim = list(meta$qst_ci_valid_by_dim %||% logical(0)),
    run_time_dgp   = prov$run_time_dgp %||% meta$run_time_dgp %||% NA_real_,
    run_time_est   = prov$run_time_est %||% meta$run_time_est %||% NA_real_,
    run_time_total = prov$run_time_total %||% meta$run_time_total %||% NA_real_,
    n_boot_ok      = meta$n_boot_ok %||% 0L,
    n_boot_fail    = meta$n_boot_fail %||% 0L,
    estimator_pkgs = meta$estimator_pkgs %||% NA_character_,
    log            = meta$log %||% NA_character_,
    qst            = list(result$qst %||% NULL)
  )
}
