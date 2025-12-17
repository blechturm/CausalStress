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

  extract_ts <- function(...) {
    for (val in list(...)) {
      if (is.null(val)) next
      # unwrap single-element lists
      if (is.list(val) && length(val) == 1L) {
        val <- val[[1]]
      }
      if (inherits(val, "POSIXct") || is.numeric(val)) {
        return(val)
      }
    }
    NA
  }

  rows <- lapply(pin_names, function(name) {
    meta <- pins::pin_meta(board, name)
    pin_obj <- try(pins::pin_read(board, name), silent = TRUE)
    pin_meta <- if (!inherits(pin_obj, "try-error")) pin_obj$meta else list()
    pin_prov <- if (!inherits(pin_obj, "try-error")) (pin_obj$provenance %||% list()) else list()

    ts_val <- extract_ts(
      pin_prov$timestamp,
      pin_prov$run_timestamp,
      meta$metadata$timestamp,
      meta$user$timestamp,
      meta$timestamp,
      meta$created
    )
    if (is.numeric(ts_val)) ts_val <- as.POSIXct(ts_val, origin = "1970-01-01")

    tibble::tibble(
      pin_name          = name,
      dgp_id            = pin_meta$dgp_id %||% meta$metadata$dgp_id %||% meta$user$dgp_id %||% NA_character_,
      estimator_id      = pin_meta$estimator_id %||% meta$metadata$estimator_id %||% meta$user$estimator_id %||% NA_character_,
      estimator_version = pin_meta$estimator_version %||% meta$metadata$estimator_version %||% meta$user$estimator_version %||% NA_character_,
      n                 = pin_meta$n %||% meta$metadata$n %||% meta$user$n %||% NA_integer_,
      seed              = pin_meta$seed %||% meta$metadata$seed %||% meta$user$seed %||% NA_integer_,
      git_hash          = pin_meta$git_hash %||% meta$metadata$git_hash %||% meta$user$git_hash %||% NA_character_,
      timestamp         = ts_val,
      oracle            = pin_meta$oracle %||% meta$metadata$oracle %||% meta$user$oracle %||% NA,
      session_info      = list(pin_meta$session_info %||% meta$metadata$session_info %||% meta$user$session_info %||% NULL)
    )
  })

  dplyr::bind_rows(rows)
}
