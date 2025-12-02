#' Delete a single result pin
#' @export
cs_delete_result <- function(board, dgp_id, estimator_id, n, seed) {
  pin_name <- glue::glue(
    "results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}"
  )

  if (pins::pin_exists(board, pin_name)) {
    pins::pin_delete(board, pin_name)
    message(glue::glue("Deleted pin: {pin_name}"))
    return(TRUE)
  } else {
    warning(glue::glue("Pin not found: {pin_name}"))
    return(FALSE)
  }
}

#' Delete all persisted results for a DGP/estimator pair
#'
#' Removes every run for a given DGP and estimator from the provided pins board.
#'
#' @param board A pins board.
#' @param dgp_id Character scalar identifying the DGP.
#' @param estimator_id Character scalar identifying the estimator.
#'
#' @return Invisibly, the names of pins deleted.
#' @export
cs_delete_campaign <- function(board, dgp_id, estimator_id) {
  pins_vec <- pins::pin_list(board)
  pin_names <- if (is.data.frame(pins_vec)) pins_vec$name else pins_vec

  pattern <- glue::glue(
    "^results__dgp={dgp_id}__est={estimator_id}__.*"
  )
  matches <- pin_names[grepl(pattern, pin_names)]

  if (length(matches) == 0L) {
    warning(glue::glue("No pins found for {dgp_id} x {estimator_id}"))
    return(invisible(character(0)))
  }

  for (nm in matches) {
    pins::pin_delete(board, nm)
  }

  invisible(matches)
}
