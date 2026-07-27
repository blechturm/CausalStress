test_that("canonical estimand target descriptors and reason vocabulary are strict", {
  targets <- CausalStress:::cs_estimand_targets()

  expect_identical(
    targets,
    list(
      att = list(
        estimand_target_id = "att",
        truth_tier = "structural",
        target_level = "population-scalar",
        target_population = "treated",
        evaluation_policy = "observed-run-sample",
        metric_ids = "point_error",
        scoring_population_id = "treated"
      ),
      ate = list(
        estimand_target_id = "ate",
        truth_tier = "structural",
        target_level = "population-scalar",
        target_population = "all",
        evaluation_policy = "observed-run-sample",
        metric_ids = "point_error",
        scoring_population_id = "full_generated_run_sample"
      ),
      qst = list(
        estimand_target_id = "qst",
        truth_tier = "distributional",
        target_level = "distributional-curve",
        target_population = "treated",
        evaluation_policy = "runner-tau-grid",
        metric_ids = "point_error",
        scoring_population_id = "treated"
      ),
      cate = list(
        estimand_target_id = "cate",
        truth_tier = "structural",
        target_level = "unit-level",
        target_population = "held-out-eval",
        evaluation_policy = "held-out-eval",
        metric_ids = "point_error",
        scoring_population_id = "held_out_eval"
      )
    )
  )
  expect_identical(
    names(targets),
    unname(vapply(targets, `[[`, character(1), "estimand_target_id"))
  )

  reasons <- c(
    "estimator_not_produced",
    "truth_unavailable",
    "metric_invalid_for_regime",
    "ci_unavailable",
    "gate_unimplemented",
    "not_requested",
    "target_not_implemented"
  )
  for (reason in reasons) {
    expect_identical(CausalStress:::cs_check_non_comparable_reason(reason), reason)
  }

  reason_error <- expect_error(
    CausalStress:::cs_check_non_comparable_reason("truth_missing"),
    class = "causalstress_non_comparable_reason_error"
  )
  expect_identical(
    conditionMessage(reason_error),
    "Unknown non-comparable reason: truth_missing."
  )
  reason_shape_error <- expect_error(
    CausalStress:::cs_check_non_comparable_reason(""),
    class = "causalstress_non_comparable_reason_error"
  )
  expect_identical(
    conditionMessage(reason_shape_error),
    "`reason` must be a non-empty character scalar."
  )

  target_error <- expect_error(
    CausalStress:::cs_estimand_target("att_typo"),
    class = "causalstress_estimand_target_error"
  )
  expect_identical(conditionMessage(target_error), "Unknown estimand target id: att_typo.")
  target_shape_error <- expect_error(
    CausalStress:::cs_estimand_target(""),
    class = "causalstress_estimand_target_error"
  )
  expect_identical(
    conditionMessage(target_shape_error),
    "`target` must identify one estimand target."
  )
})

test_that("legacy estimator outputs normalize to typed ATT and QST outputs", {
  tau <- c(0.25, 0.75)
  legacy <- list(
    att = list(estimate = 1.5),
    qst = tibble::tibble(tau = tau, value = c(0.1, 0.2)),
    meta = list(estimator_id = "legacy")
  )

  outputs <- CausalStress:::cs_normalize_estimator_outputs(legacy, tau = tau)

  expect_named(outputs, c("att", "qst"))
  expect_equal(outputs$att$estimate, 1.5)
  expect_equal(outputs$qst$estimate, c(0.1, 0.2))
  expect_equal(outputs$qst$tau_id, cs_tau_id(tau))
})

test_that("legacy tabular and missing outputs normalize at the canonical boundary", {
  tabular <- list(
    att = tibble::tibble(estimate = 5),
    qst = tibble::tibble(tau = c(0.3, 0.4), estimate = c(3, 4))
  )
  outputs <- CausalStress:::cs_normalize_estimator_outputs(tabular)

  expect_identical(outputs$att$estimate, 5)
  expect_identical(outputs$att$estimand_target_id, "att")
  expect_equal(outputs$qst$estimate, c(3, 4))
  expect_equal(outputs$qst$tau_id, cs_tau_id(c(0.3, 0.4)))

  expect_identical(
    CausalStress:::cs_normalize_estimator_outputs(list(att = NULL, qst = NULL)),
    list()
  )
})

test_that("typed outputs normalize and do not cross-score ATT and ATE", {
  typed <- list(
    outputs = list(
      att = list(estimate = 1),
      ate = list(estimate = 99)
    ),
    meta = list(estimator_id = "typed")
  )
  outputs <- CausalStress:::cs_normalize_estimator_outputs(typed)
  dgp <- list(
    df = tibble::tibble(w = c(1, 0), y = c(1, 0)),
    true_att = 1,
    true_qst = NULL,
    meta = list(type = "synthetic", structural_te = c(1, 3))
  )

  scores <- CausalStress:::cs_build_score_surface(
    requested_targets = c("att", "ate"),
    outputs = outputs,
    dgp = dgp,
    att = list(estimate = 1, true = 1, error = 0, abs_error = 0),
    qst = NULL
  )

  att_row <- scores[scores$estimand_target_id == "att", ]
  ate_row <- scores[scores$estimand_target_id == "ate", ]
  expect_equal(att_row$truth, 1)
  expect_equal(att_row$estimate, 1)
  expect_equal(ate_row$truth, 2)
  expect_equal(ate_row$estimate, 99)
})

test_that("three-way scoring join emits branch-specific non-comparable reasons", {
  outputs <- list(att = list(estimate = 1, estimand_target_id = "att"))
  dgp <- list(
    df = tibble::tibble(w = c(1, 0), y = c(1, 0)),
    true_att = 1,
    true_qst = NULL,
    meta = list(type = "real", structural_te = NULL)
  )

  scores <- CausalStress:::cs_build_score_surface(
    requested_targets = c("att", "ate", "qst", "cate"),
    outputs = outputs,
    dgp = dgp,
    att = list(estimate = 1, true = 1, error = 0, abs_error = 0),
    qst = NULL
  )

  expect_equal(
    scores$score_status[match(c("att", "ate", "qst", "cate"), scores$estimand_target_id)],
    c("scored", "non_comparable", "non_comparable", "non_comparable")
  )
  expect_equal(
    scores$non_comparable_reason[match(c("ate", "qst", "cate"), scores$estimand_target_id)],
    c("estimator_not_produced", "estimator_not_produced", "target_not_implemented")
  )

  ate_output <- list(ate = list(estimate = 2, estimand_target_id = "ate"))
  real_scores <- CausalStress:::cs_build_score_surface(
    requested_targets = "ate",
    outputs = ate_output,
    dgp = dgp,
    att = NULL,
    qst = NULL
  )
  expect_equal(real_scores$score_status, "non_comparable")
  expect_equal(real_scores$non_comparable_reason, "truth_unavailable")
})

test_that("oracle_att emits typed ATT and ATE and runner scores ATE from scorer-owned truth", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "oracle_att",
    n = 100,
    seed = 42,
    config = list(estimand_targets = c("att", "ate"))
  )

  expect_named(res$outputs, c("att", "ate"))
  expect_equal(res$outputs$att$estimate, res$att$estimate)

  ate_row <- res$scores[res$scores$estimand_target_id == "ate", ]
  expect_equal(nrow(ate_row), 1L)
  expect_equal(ate_row$score_status, "scored")
  expect_equal(ate_row$estimate, ate_row$truth)
  expect_equal(ate_row$error, 0)
})

test_that("ordinary typed-path estimators do not receive truth columns while ATE truth is scorer-side", {
  spy_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    forbidden <- c("y0", "y1", "p", "structural_te")
    if (any(forbidden %in% names(df))) {
      stop("truth column reached ordinary estimator")
    }
    list(
      outputs = list(
        att = list(estimate = 0),
        ate = list(estimate = 0)
      ),
      meta = list(estimator_id = "typed_spy", oracle = FALSE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id = "typed_spy",
    type = "test",
    generator = spy_est,
    oracle = FALSE,
    supports_qst = FALSE,
    version = "0.0.0",
    description = "Typed path airlock spy",
    source = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "typed_spy",
    n = 50,
    seed = 4,
    config = list(estimand_targets = c("att", "ate"))
  )

  ate_row <- res$scores[res$scores$estimand_target_id == "ate", ]
  expect_equal(ate_row$score_status, "scored")
  expect_true(is.finite(ate_row$truth))
  expect_false(res$meta$oracle)
})

test_that("failed estimator runs emit estimator_error score rows, not estimator_not_produced", {
  failing_typed_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    stop("typed failure")
  }

  cs_register_estimator(
    estimator_id = "typed_failing_est",
    type = "test",
    generator = failing_typed_est,
    oracle = FALSE,
    supports_qst = FALSE,
    version = "0.0.0",
    description = "Typed scoring failure test estimator",
    source = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "typed_failing_est",
    n = 50,
    seed = 11,
    config = list(estimand_targets = c("att", "ate"))
  )

  expect_false(res$meta$success)
  expect_equal(res$scores$score_status, c("estimator_error", "estimator_error"))
  expect_true(all(is.na(res$scores$non_comparable_reason)))
  expect_true(all(is.na(res$scores$estimate)))
  expect_true(all(is.na(res$scores$truth)))
})
