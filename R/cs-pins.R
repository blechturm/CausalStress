cs_pin_write <- function(board, result) {
  meta <- result$meta
  att <- result$att %||% list()
  prov <- result$provenance %||% list()

  name <- glue::glue(
    "results__dgp={meta$dgp_id}__est={meta$estimator_id}__n={meta$n}__seed={meta$seed}"
  )

  git_hash <- tryCatch(
    system("git rev-parse HEAD", intern = TRUE, ignore.stderr = TRUE),
    warning = function(w) NA_character_,
    error   = function(e) NA_character_
  )

  ts <- prov$timestamp %||% prov$run_timestamp %||% Sys.time()
  ts_num <- as.numeric(ts)

  suppressMessages(
    pins::pin_write(
      board   = board,
      x       = result,
      name    = name,
      type    = "qs",
      metadata = list(
        success      = meta$success %||% NA,
        error        = meta$error %||% NA_character_,
        dgp_id       = meta$dgp_id,
        dgp_version  = meta$dgp_version %||% NA_character_,
        dgp_status   = meta$dgp_status %||% NA_character_,
        dgp_design_spec = meta$dgp_design_spec %||% NA_character_,
        estimator_id = meta$estimator_id,
        estimator_version = meta$estimator_version %||% NA_character_,
        n            = meta$n,
        seed         = meta$seed,
        oracle       = meta$oracle %||% NA,
        supports_qst = meta$supports_qst %||% NA,
        true_att     = att$true %||% NA_real_,
        est_att      = att$estimate %||% NA_real_,
        att_error    = att$error %||% NA_real_,
        att_abs_error = att$abs_error %||% NA_real_,
        att_ci_lo    = att$ci_lo %||% NA_real_,
        att_ci_hi    = att$ci_hi %||% NA_real_,
        att_ci_method = meta$ci_method %||% NA_character_,
        att_ci_type   = meta$ci_type %||% NA_character_,
        att_ci_level  = meta$ci_level %||% NA_real_,
        att_ci_valid  = meta$ci_valid %||% NA,
        att_ci_fail_code = meta$ci_fail_code %||% NA_character_,
        n_boot_ok    = meta$n_boot_ok %||% 0L,
        n_boot_fail  = meta$n_boot_fail %||% 0L,
        estimator_pkgs = meta$estimator_pkgs %||% NA_character_,
        config_fingerprint = meta$config_fingerprint %||% NA_character_,
        git_hash     = git_hash,
        session_info = list(utils::sessionInfo()),
        timestamp    = ts_num
      )
    )
  )

  invisible(name)
}

cs_pin_exists <- function(board, dgp_id, estimator_id, n, seed) {
  name <- glue::glue(
    "results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}"
  )
  pins::pin_exists(board, name)
}
