cs_pin_write <- function(board, result) {
  meta <- result$meta

  name <- glue::glue(
    "results/{meta$dgp_id}/{meta$estimator_id}/n={meta$N}/seed={meta$seed}"
  )

  pins::pin_write(
    board = board,
    x     = result,
    name  = name,
    type  = "qs"
  )

  invisible(name)
}
