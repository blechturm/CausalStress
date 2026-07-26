#' Confidence interval methods (`ci_method`)
#'
#' CausalStress distinguishes between a user-facing CI *intent* (`ci_method`) and
#' the CI implementation chosen by a specific estimator.
#'
#' @details
#' **Allowed values**
#' \itemize{
#'   \item \code{"none"}: no confidence intervals are computed.
#'   \item \code{"default"}: use the best supported method for the estimator.
#'   \item \code{"bootstrap"}: compute bootstrap confidence intervals (requires a deterministic seed).
#'   \item \code{"native"}: use the estimator's native/Wald-style CI, when supported.
#' }
#'
#' **Precedence and defaults**
#' \itemize{
#'   \item In estimator functions (e.g., [est_lm_att()]), a missing
#'     \code{config$ci_method} is treated as \code{"none"} to avoid surprising,
#'     random, or expensive computation.
#'   \item Runner functions (e.g., [cs_run_single()]) may set \code{ci_method}
#'     when it is missing, based on runner flags such as \code{bootstrap=TRUE}.
#'     Runners also inject \code{config$seed} from the run's \code{seed}.
#' }
#'
#' **Estimator-specific mapping for \code{"default"}**
#' \itemize{
#'   \item Defaults to \code{"bootstrap"} for: \code{lm_att}, \code{ipw_att},
#'     \code{gengc}, \code{gengc_dr}.
#'   \item Defaults to \code{"native"} for: \code{grf_dr_att}, \code{bart_att},
#'     \code{tmle_att}.
#' }
#'
#' **Reproducibility rule (bootstrap)**
#'
#' Bootstrap confidence intervals require \code{config$seed}. If bootstrap CIs
#' are requested explicitly (\code{ci_method = "bootstrap"}) or indirectly
#' (\code{ci_method = "default"} mapping to bootstrap) and no seed is available,
#' CausalStress fails with a \code{"causalstress_config_error"}.
#'
#' **Recommendation**
#' \itemize{
#'   \item For one-off exploratory calls, either set \code{ci_method = "none"}
#'     (point estimate only) or provide a seed in \code{config}.
#'   \item For campaigns, prefer the runner APIs ([cs_run_seeds()],
#'     [cs_run_campaign()]) which handle seeds deterministically.
#' }
#'
#' **Introspection**
#'
#' Estimator outputs record the resolved CI method in \code{res$meta$ci_method}
#' and the origin of that choice in \code{res$meta$ci_method_source} (e.g.,
#' \code{"explicit"}, \code{"default_mapped"}, \code{"runner_bootstrap"}).
#'
#' @name cs_ci_methods
#' @rdname cs_ci_methods
#' @aliases cs_ci_methods
NULL
