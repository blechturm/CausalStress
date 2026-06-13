cs_result_pin_name <- function(dgp_id, estimator_id, n, seed, dgp_version = NULL) {
  if (!is.null(dgp_version) && !is.na(dgp_version) && nzchar(dgp_version)) {
    return(glue::glue(
      "results__dgp={dgp_id}__dgpver={dgp_version}__est={estimator_id}__n={n}__seed={seed}"
    ))
  }
  glue::glue("results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}")
}

cs_result_pin_name_legacy <- function(dgp_id, estimator_id, n, seed) {
  glue::glue("results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}")
}

cs_find_result_pin <- function(board, dgp_id, estimator_id, n, seed, dgp_version = NULL,
                               include_legacy = TRUE) {
  if (is.null(dgp_version) || is.na(dgp_version) || !nzchar(dgp_version)) {
    pins_vec <- pins::pin_list(board)
    all_names <- if (is.data.frame(pins_vec)) pins_vec$name else pins_vec
    pattern <- glue::glue(
      "^results__dgp={dgp_id}__(dgpver=[^_]+__)?est={estimator_id}__n={n}__seed={seed}$"
    )
    matches <- all_names[grepl(pattern, all_names)]
    if (length(matches) > 0L) {
      versioned <- matches[grepl("__dgpver=", matches)]
      if (length(versioned) > 0L) return(versioned[[1L]])
      if (isTRUE(include_legacy)) return(matches[[1L]])
    }
    return(NA_character_)
  }
  candidates <- cs_result_pin_name(dgp_id, estimator_id, n, seed, dgp_version)
  if (isTRUE(include_legacy)) {
    candidates <- unique(c(candidates, cs_result_pin_name_legacy(dgp_id, estimator_id, n, seed)))
  }
  for (name in candidates) {
    if (pins::pin_exists(board, name)) {
      return(name)
    }
  }
  NA_character_
}

cs_pin_write <- function(board, result) {
  meta <- result$meta
  att <- result$att %||% list()
  prov <- result$provenance %||% list()

  name <- cs_result_pin_name(
    dgp_id = meta$dgp_id,
    dgp_version = meta$dgp_version %||% NA_character_,
    estimator_id = meta$estimator_id,
    n = meta$n,
    seed = meta$seed
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
        config_fingerprint_schema = meta$config_fingerprint_schema %||% NA_integer_,
        git_hash     = git_hash,
        session_info = list(utils::sessionInfo()),
        timestamp    = ts_num
      )
    )
  )

  invisible(name)
}

cs_pin_exists <- function(board, dgp_id, estimator_id, n, seed, dgp_version = NULL,
                          include_legacy = TRUE) {
  name <- cs_find_result_pin(
    board = board,
    dgp_id = dgp_id,
    estimator_id = estimator_id,
    n = n,
    seed = seed,
    dgp_version = dgp_version,
    include_legacy = include_legacy
  )
  !is.na(name)
}
