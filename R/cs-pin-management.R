#' Delete a single result pin
#'
#' @param board A pins board.
#' @param dgp_id Character scalar identifying the DGP.
#' @param estimator_id Character scalar identifying the estimator.
#' @param n Integer sample size.
#' @param seed Integer simulation seed.
#' @param version Optional DGP version. When supplied, only the exact versioned
#'   result pin is deleted; when omitted, legacy and versioned pins matching the
#'   tuple are deleted.
#'
#' @return Logical scalar indicating whether any pin was deleted.
#' @export
cs_delete_result <- function(board, dgp_id, estimator_id, n, seed, version = NULL) {
  pin_names <- if (!is.null(version)) {
    cs_result_pin_name(dgp_id, estimator_id, n, seed, dgp_version = version)
  } else {
    pins_vec <- pins::pin_list(board)
    all_names <- if (is.data.frame(pins_vec)) pins_vec$name else pins_vec
    pattern <- glue::glue(
      "^results__dgp={dgp_id}__(dgpver=[^_]+__)?est={estimator_id}__n={n}__seed={seed}$"
    )
    all_names[grepl(pattern, all_names)]
  }

  pin_names <- pin_names[pins::pin_exists(board, pin_names)]
  if (length(pin_names) > 0L) {
    for (pin_name in pin_names) {
      pins::pin_delete(board, pin_name)
      message(glue::glue("Deleted pin: {pin_name}"))
    }
    return(TRUE)
  } else {
    warning("Pin not found.")
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
    "^results__dgp={dgp_id}__(dgpver=[^_]+__)?est={estimator_id}__.*"
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
