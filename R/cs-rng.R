#' Set CausalStress RNG state
#'
#' Configures the random number generator to use a fixed backend and optionally seeds it.
#'
#' @param seed Optional seed passed to `set.seed()`.
#' @return Invisibly returns `NULL`.
#' @export
cs_set_rng <- function(seed = NULL) {
  suppressWarnings(
    RNGkind("Mersenne-Twister", "Inversion", "Rounding")
  )
  if (!is.null(seed)) set.seed(seed)
  invisible(NULL)
}

cs_rng_state_capture <- function() {
  list(
    kind = RNGkind(),
    has_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    seed = if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
  )
}

cs_rng_state_restore <- function(state) {
  suppressWarnings(do.call(RNGkind, as.list(state$kind)))
  if (isTRUE(state$has_seed)) {
    assign(".Random.seed", state$seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    remove(list = ".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

cs_with_preserved_rng <- function(expr) {
  state <- cs_rng_state_capture()
  on.exit(cs_rng_state_restore(state), add = TRUE)
  force(expr)
}

cs_with_mandated_rng <- function(seed = NULL, expr) {
  cs_with_preserved_rng({
    cs_set_rng(seed)
    force(expr)
  })
}

