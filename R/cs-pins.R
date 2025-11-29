cs_pin_write <- function(board, result) {
  meta <- result$meta

  name <- glue::glue(
    "results__dgp={meta$dgp_id}__est={meta$estimator_id}__n={meta$n}__seed={meta$seed}"
  )

  suppressMessages(
    pins::pin_write(
      board   = board,
      x       = result,
      name    = name,
      type    = "qs",
      metadata = list(
        dgp_id       = meta$dgp_id,
        estimator_id = meta$estimator_id,
        n            = meta$n,
        seed         = meta$seed
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
