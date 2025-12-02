cs_result_to_row <- function(result) {
  att  <- result$att %||% list()
  meta <- result$meta %||% list()

  tibble::tibble(
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
    run_time_dgp   = meta$run_time_dgp %||% NA_real_,
    run_time_est   = meta$run_time_est %||% NA_real_,
    run_time_total = meta$run_time_total %||% NA_real_,
    n_boot_ok      = meta$n_boot_ok %||% 0L,
    estimator_pkgs = meta$estimator_pkgs %||% NA_character_,
    log            = meta$log %||% NA_character_
  )
}
