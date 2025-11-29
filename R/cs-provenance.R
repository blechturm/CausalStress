#' Audit pinned CausalStress results on a board
#'
#' Reads metadata from pins created by `cs_run_single()` (via persistence) and
#' returns a tidy tibble of provenance fields.
#'
#' @param board A pins board containing results pins.
#' @return A tibble with columns `pin_name`, `dgp_id`, `estimator_id`,
#'   `estimator_version`, `n`, `seed`, `git_hash`, `timestamp`, `oracle`,
#'   and `session_info` (list column where available).
#' @export
cs_audit <- function(board) {
  pin_list_obj <- pins::pin_list(board)
  pin_names <- if (is.character(pin_list_obj)) {
    pin_list_obj
  } else {
    pin_list_obj$name %||% as.character(pin_list_obj)
  }
  pin_names <- pin_names[grepl("^results__", pin_names)]

  if (length(pin_names) == 0L) {
    return(tibble::tibble(
      pin_name = character(),
      dgp_id = character(),
      estimator_id = character(),
      estimator_version = character(),
      n = integer(),
      seed = integer(),
      git_hash = character(),
      timestamp = as.POSIXct(character()),
      oracle = logical(),
      session_info = list()
    ))
  }

  rows <- lapply(pin_names, function(name) {
    meta <- pins::pin_meta(board, name)
    md <- meta$metadata %||% meta$user %||% list()

    tibble::tibble(
      pin_name          = name,
      dgp_id            = md$dgp_id %||% NA_character_,
      estimator_id      = md$estimator_id %||% NA_character_,
      estimator_version = md$estimator_version %||% NA_character_,
      n                 = md$n %||% NA_integer_,
      seed              = md$seed %||% NA_integer_,
      git_hash          = md$git_hash %||% NA_character_,
      timestamp         = md$timestamp %||% NA,
      oracle            = md$oracle %||% NA,
      session_info      = list(md$session_info %||% NULL)
    )
  })

  dplyr::bind_rows(rows)
}
