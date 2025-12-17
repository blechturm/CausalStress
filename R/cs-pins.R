cs_pin_write <- function(board, result) {
  meta <- result$meta
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
