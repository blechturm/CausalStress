#' Plan a batched campaign
#'
#' Builds a deterministic, globally shuffled task plan and groups tasks into
#' batches for staged execution.
#'
#' @param dgp_list Character vector of DGP ids.
#' @param estimator_list Character vector of estimator ids.
#' @param n_seeds Integer count or integer vector of seeds.
#' @param batch_size Integer batch size (tasks per batch).
#' @param campaign_seed Integer seed for deterministic shuffling.
#' @param strategy_map List of defaults and/or per-estimator overrides.
#'
#' @return A tibble with columns `batch_id` and `tasks` (list-column).
#' @export
cs_plan_campaign <- function(dgp_list,
                             estimator_list,
                             n_seeds,
                             batch_size = 50L,
                             campaign_seed = 1L,
                             strategy_map = list()) {
  if (length(dgp_list) < 1L) {
    stop("dgp_list must contain at least one id.")
  }
  if (length(estimator_list) < 1L) {
    stop("estimator_list must contain at least one id.")
  }
  if (length(n_seeds) < 1L) {
    stop("n_seeds must be a positive integer or vector of seeds.")
  }
  if (batch_size < 1L) {
    stop("batch_size must be >= 1.")
  }

  seeds <- if (length(n_seeds) > 1L) {
    as.integer(n_seeds)
  } else {
    seq_len(as.integer(n_seeds))
  }

  defaults <- list()
  overrides <- list()
  if (!is.null(strategy_map)) {
    if (!is.null(strategy_map$defaults)) {
      defaults <- strategy_map$defaults
    }
    if (!is.null(strategy_map$overrides)) {
      overrides <- strategy_map$overrides
    } else {
      overrides <- strategy_map[setdiff(names(strategy_map), "defaults")]
    }
  }

  resolve_config <- function(est_id) {
    cfg <- defaults
    if (!is.null(est_id) && est_id %in% names(overrides)) {
      cfg <- utils::modifyList(cfg, overrides[[est_id]])
    }
    cfg
  }

  grid <- tidyr::expand_grid(
    dgp_id = dgp_list,
    estimator_id = estimator_list,
    seed = seeds
  )

  grid$task_config <- lapply(grid$estimator_id, resolve_config)
  grid$resolved_config_hash <- vapply(
    grid$task_config,
    function(cfg) digest::digest(cfg, algo = "sha256"),
    character(1)
  )
  grid$task_fingerprint <- vapply(
    seq_len(nrow(grid)),
    function(i) {
      digest::digest(
        list(
          dgp_id = grid$dgp_id[[i]],
          estimator_id = grid$estimator_id[[i]],
          seed = grid$seed[[i]],
          task_config = grid$task_config[[i]]
        ),
        algo = "sha256"
      )
    },
    character(1)
  )
  grid$fingerprint_version <- 2L
  grid$config_fingerprint_schema <- 2L

  old_seed <- NULL
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- .GlobalEnv[[".Random.seed"]]
  }
  on.exit({
    if (!is.null(old_seed)) {
      .GlobalEnv[[".Random.seed"]] <- old_seed
    }
  }, add = TRUE)
  set.seed(campaign_seed)

  perm <- sample.int(nrow(grid))
  grid <- grid[perm, , drop = FALSE]
  grid$batch_id <- ceiling(seq_len(nrow(grid)) / batch_size)

  tasks_by_batch <- split(grid, grid$batch_id)
  tibble::tibble(
    batch_id = as.integer(names(tasks_by_batch)),
    tasks = lapply(
      tasks_by_batch,
      function(df) {
        dplyr::select(df, -batch_id)
      }
    )
  )
}
