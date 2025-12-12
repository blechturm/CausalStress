library(CausalStress)
library(dplyr)
library(future)

# -------------------------------------------------------------------
# Minimal normrank estimator (dev-only) + quick run on synth_baseline
# -------------------------------------------------------------------

# Core normrank + OLS helper (ATT only)
cs_estimator_normrank_ols <- function(data,
                                      treatment_col = "w",
                                      outcome_col = "y",
                                      covariate_cols = NULL,
                                      n_syn = 1e5,
                                      seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(covariate_cols)) {
    covariate_cols <- setdiff(names(data), c(treatment_col, outcome_col))
  }

  treated_idx   <- data[[treatment_col]] == 1
  treated_data  <- data[treated_idx, c(outcome_col, covariate_cols), drop = FALSE]
  control_data  <- data[!treated_idx, c(outcome_col, covariate_cols), drop = FALSE]

  syn_treated <- normrank_synthesize_internal(treated_data, outcome_col, n_syn, seed = seed)
  syn_control <- normrank_synthesize_internal(control_data, outcome_col, n_syn, seed = if (!is.null(seed)) seed + 1 else NULL)

  syn_treated[[treatment_col]] <- 1
  syn_control[[treatment_col]] <- 0
  syn_combined <- rbind(syn_treated, syn_control)

  formula_obj <- as.formula(paste0(outcome_col, " ~ ", treatment_col, " + ", paste(covariate_cols, collapse = " + ")))
  fit <- lm(formula_obj, data = syn_combined)
  att_hat <- coef(fit)[treatment_col]

  list(att = as.numeric(att_hat))
}

# Simple Gaussian copula synth
normrank_synthesize_internal <- function(data, outcome_col, n_syn, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(data)
  var_names <- names(data)
  syn <- data.frame(matrix(NA, nrow = n_syn, ncol = length(var_names)))
  names(syn) <- var_names
  syn[[1]] <- sample(data[[1]], n_syn, replace = TRUE)
  for (j in 2:length(var_names)) {
    y <- data[[j]]
    X <- data[, 1:(j - 1), drop = FALSE]
    X_syn <- syn[, 1:(j - 1), drop = FALSE]
    ranks <- rank(y, ties.method = "average", na.last = "keep")
    z <- qnorm(ranks / (n + 1))
    fit <- lm(z ~ ., data = cbind(z = z, X))
    sigma <- summary(fit)$sigma
    z_pred <- predict(fit, newdata = X_syn)
    z_syn <- z_pred + rnorm(n_syn, 0, sigma)
    q_syn <- pnorm(z_syn)
    sorted_y <- sort(y, na.last = TRUE)
    idx <- pmax(1, pmin(n, round(q_syn * (n + 1))))
    syn[[j]] <- sorted_y[idx]
  }
  syn
}

# Adapter to satisfy cs_run_* contract
est_normrank_ols_adapter <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  drop_cols <- intersect(c("y0", "y1", "p", "structural_te"), names(df))
  df_run <- df[, setdiff(names(df), drop_cols), drop = FALSE]
  fit <- cs_estimator_normrank_ols(
    data = df_run,
    treatment_col = "w",
    outcome_col = "y",
    covariate_cols = setdiff(names(df_run), c("y", "w")),
    n_syn = config$n_syn %||% 1e5,
    seed  = config$seed %||% NULL
  )
  res <- list(
    att  = list(estimate = fit$att),
    qst  = NULL,
    cf   = NULL,
    meta = list(estimator_id = "normrank_ols", oracle = FALSE, supports_qst = FALSE, config = config)
  )
  CausalStress:::cs_check_estimator_output(res, require_qst = FALSE)
  res
}

# Register dev estimator
register_normrank_estimator <- function() {
  CausalStress::cs_register_estimator(
    estimator_id  = "normrank_ols",
    type          = "gcomp",
    generator     = est_normrank_ols_adapter,
    oracle        = FALSE,
    supports_qst  = FALSE,
    description   = "Normrank stratified Gaussian copula + OLS (dev)",
    source        = "dev",
    requires_pkgs = character(0)
  )
  message("✓ Registered 'normrank_ols'")
}

# ------------------------------------------------------------
# Minimal demo: run over seeds on synth_baseline
# ------------------------------------------------------------

seeds <- 1:10
plan(multisession, workers = 4)
on.exit(plan(sequential), add = TRUE)

register_normrank_estimator()

results <- cs_run_seeds(
  dgp_id        = "synth_baseline",
  estimator_id  = "normrank_ols",
  n             = 1000,
  seeds         = seeds,
  bootstrap     = FALSE,
  parallel      = FALSE,
  show_progress = TRUE
)

cs_tidy(results) %>%
  select(dgp_id, estimator_id, seed, est_att, att_error) %>%
  print(n = Inf)


test <- cs_run_campaign(
    dgp_ids = "synth_baseline",
    estimator_ids = c("normrank_ols", c("lm_att", "grf_dr", "gengc")),
    seeds = 1,
    n = 2000,
    bootstrap = FALSE,
    parallel = FALSE,
    show_progress = TRUE
  )


test %>%
  cs_tidy() %>%
  select(dgp_id, estimator_id, seed, est_att, att_error) %>%
  print(n = Inf)


test_overlap <- cs_run_campaign(
  dgp_ids = "synth_placebo_kangschafer",
  estimator_ids = c("normrank_ols", "lm_att", "grf_dr", "gengc", "gengc_dr"),
  seeds = 1:10,
  n = 2000,
  bootstrap = FALSE,
  show_progress = TRUE,
  parallel = FALSE
)

test_overlap %>%
  cs_tidy() %>%
  group_by(estimator_id) %>%
  summarise(mean_error = mean(att_error), rmse = sqrt(mean(att_error^2)))


# barwert nach KMU aufteilen?
