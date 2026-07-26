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
#' @param strategy_map List of defaults and/or per-estimator overrides. Use
#'   `list(defaults = list(...), overrides = list(est_id = list(...)))`.
#'   The resolved config is attached to each task (defaults + per-estimator
#'   overrides via `modifyList`). Common fields include:
#'   \itemize{
#'     \item \code{n}: sample size per run (required by the worker; overrides any global \code{n}).
#'     \item \code{seed}: per-task RNG seed (usually not overridden; use \code{n_seeds} instead).
#'     \item \code{ci_method}: one of "none", "default", "bootstrap", "native" (see [cs_ci_methods]).
#'     \item \code{n_boot}: number of bootstrap draws.
#'     \item \code{tau}: custom quantile grid (numeric vector).
#'     \item \code{num_threads}: force single-threaded estimators.
#'     \item estimator-specific hyperparameters (e.g., \code{num_trees}, \code{n_draws}).
#'   }
#'
#' @return A tibble with columns `batch_id` and `tasks` (list-column).
#' @export
#'
#' @examples
#' plan <- cs_plan_campaign(
#'   dgp_list = c("synth_baseline"),
#'   estimator_list = c("lm_att", "ipw_att"),
#'   n_seeds = 1:4,
#'   batch_size = 2,
#'   campaign_seed = 123,
#'   strategy_map = list(
#'     defaults = list(n_boot = 200, ci_method = "bootstrap"),
#'     overrides = list(ipw_att = list(ci_method = "native"))
#'   )
#' )
#' plan
#' \dontrun{
#' # Run using the plan-based batching engine (v0.1.9)
#' cs_run_campaign(
#'   plan = plan,
#'   staging_dir = "staging_batches",
#'   workers = 2
#' )
#'
#' # Legacy grid runner (v0.1.8 and earlier)
#' cs_run_campaign(
#'   dgp_ids = c("synth_baseline"),
#'   estimator_ids = c("lm_att", "ipw_att"),
#'   seeds = 1:4,
#'   n = 200,
#'   defaults = list(ci_method = "bootstrap")
#' )
#' }
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
  grid$n <- vapply(
    grid$task_config,
    function(cfg) as.integer(cfg$n %||% NA_integer_),
    integer(1)
  )
  dgp_versions <- vapply(
    grid$dgp_id,
    function(id) cs_get_dgp(id, quiet = TRUE)$version[[1L]],
    character(1)
  )
  estimator_versions <- vapply(
    grid$estimator_id,
    function(id) cs_get_estimator(id)$version,
    character(1)
  )
  invisible(lapply(seq_len(nrow(grid)), function(i) {
    cs_assert_wave1_targets_executable(
      config = grid$task_config[[i]],
      estimator_desc = cs_get_estimator(grid$estimator_id[[i]])
    )
  }))
  grid$dgp_version <- dgp_versions
  grid$estimator_version <- estimator_versions
  grid$resolved_config_hash <- vapply(
    grid$task_config,
    function(cfg) digest::digest(cs_fingerprint_config_payload(cfg), algo = "sha256"),
    character(1)
  )
  grid$task_fingerprint <- vapply(
    seq_len(nrow(grid)),
    function(i) {
      cfg <- grid$task_config[[i]]
      cs_build_task_fingerprint(
        dgp_id = grid$dgp_id[[i]],
        dgp_version = grid$dgp_version[[i]],
        estimator_id = grid$estimator_id[[i]],
        estimator_version = grid$estimator_version[[i]],
        n = grid$n[[i]],
        seed = grid$seed[[i]],
        config = cfg,
        tau = cfg$tau %||% cs_tau_oracle,
        bootstrap = identical(cfg$ci_method %||% NULL, "bootstrap"),
        B = cfg$B %||% cfg$n_boot %||% 0L
      )
    },
    character(1)
  )
  grid$fingerprint_version <- 4L
  grid$config_fingerprint_schema <- 4L

  if (!is.null(campaign_seed)) {
    perm <- cs_with_mandated_rng(as.integer(campaign_seed), sample.int(nrow(grid)))
    grid <- grid[perm, , drop = FALSE]
  }
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
