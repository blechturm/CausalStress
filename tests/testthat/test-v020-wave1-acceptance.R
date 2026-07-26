test_that("CATE-only Wave 1 requests hard-reject before estimator execution", {
  estimator_called <- FALSE
  spy_estimator <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    estimator_called <<- TRUE
    stop("CATE-only staging did not stop estimator execution")
  }

  with_mocked_bindings(
    cs_get_estimator = function(estimator_id) {
      list(
        estimator_id = estimator_id,
        type = "test",
        generator = spy_estimator,
        oracle = FALSE,
        oracle_columns = character(0),
        oracle_default_columns = character(0),
        supports_qst = FALSE,
        version = "0.0.0",
        description = "CATE-only staging spy",
        source = "test",
        requires_pkgs = character(0)
      )
    },
    {
      expect_error(
        cs_run_single(
          dgp_id = "synth_baseline",
          estimator_id = "cate_only_staging_spy",
          n = 30,
          seed = 1206,
          config = list(estimand_targets = "cate")
        ),
        class = "causalstress_target_not_implemented_error"
      )
      expect_false(estimator_called)
    }
  )
})

test_that("mixed CATE requests emit target_not_implemented and score implemented targets", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "oracle_att",
    n = 60,
    seed = 1207,
    config = list(estimand_targets = c("att", "ate", "cate"))
  )

  expect_true(res$meta$success)
  expect_equal(sort(res$scores$estimand_target_id), c("ate", "att", "cate"))

  scored <- res$scores[res$scores$estimand_target_id %in% c("att", "ate"), ]
  expect_true(all(scored$score_status == "scored"))
  expect_true(all(is.finite(scored$estimate)))
  expect_true(all(is.finite(scored$truth)))

  cate_row <- res$scores[res$scores$estimand_target_id == "cate", ]
  expect_equal(nrow(cate_row), 1L)
  expect_equal(cate_row$score_status, "non_comparable")
  expect_equal(cate_row$non_comparable_reason, "target_not_implemented")
  expect_true(is.na(cate_row$estimate))
  expect_true(is.na(cate_row$truth))

  wave2_fields <- c(
    "seed_eval", "n_eval", "eval_derivation", "unit_id_digest",
    "prediction_digest", "transductive"
  )
  expect_true(all(wave2_fields %in% names(cate_row)))
  expect_true(all(vapply(cate_row[wave2_fields], function(x) all(is.na(x)), logical(1))))
})

test_that("CATE-only campaign planning rejects before creating executable tasks", {
  expect_error(
    cs_plan_campaign(
      dgp_list = "synth_baseline",
      estimator_list = "lm_att",
      n_seeds = 1L,
      batch_size = 1L,
      campaign_seed = 1208,
      strategy_map = list(defaults = list(n = 30L, estimand_targets = "cate"))
    ),
    class = "causalstress_target_not_implemented_error"
  )
})

test_that("legacy ATT and QST numerical values are stable modulo typed schema", {
  reference_tol <- 1e-8
  identity_tol <- 1e-12

  oracle <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "oracle_att",
    n = 60,
    seed = 1301,
    config = list(estimand_targets = c("att", "ate"))
  )
  oracle_att_score <- oracle$scores[oracle$scores$estimand_target_id == "att", ]
  oracle_ate_score <- oracle$scores[oracle$scores$estimand_target_id == "ate", ]

  # Absolute DGP/model reference values are cross-substrate-sensitive; identity
  # checks below remain tight because they compare values already computed in the
  # same process.
  expect_equal(oracle$att$estimate, 1.0229701591977549, tolerance = reference_tol)
  expect_equal(oracle$att$true, 1.0229701591977549, tolerance = reference_tol)
  expect_equal(oracle_att_score$estimate, oracle$att$estimate, tolerance = identity_tol)
  expect_equal(oracle_att_score$truth, oracle$att$true, tolerance = identity_tol)
  expect_equal(oracle_ate_score$estimate, 0.96209042612028262, tolerance = reference_tol)
  expect_equal(oracle_ate_score$truth, 0.96209042612028262, tolerance = reference_tol)

  lm <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 60,
    seed = 1302,
    config = list(estimand_targets = "att")
  )
  lm_att_score <- lm$scores[lm$scores$estimand_target_id == "att", ]

  expect_equal(lm$att$estimate, 0.85923944555651943, tolerance = reference_tol)
  expect_equal(lm$att$true, 1.0909537824436135, tolerance = reference_tol)
  expect_equal(lm$att$error, -0.23171433688709409, tolerance = reference_tol)
  expect_equal(lm_att_score$estimate, lm$att$estimate, tolerance = identity_tol)
  expect_equal(lm_att_score$truth, lm$att$true, tolerance = identity_tol)
  expect_equal(lm_att_score$error, lm$att$error, tolerance = identity_tol)

  est_id <- "legacy_qst_golden_v020"
  if (!est_id %in% CausalStress:::cs_estimator_registry()$estimator_id) {
    legacy_qst_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
      list(
        att = list(estimate = 0.125),
        qst = tibble::tibble(tau = tau, value = c(-0.2, 0, 0.2)),
        meta = list(estimator_id = est_id, oracle = FALSE, supports_qst = TRUE)
      )
    }
    cs_register_estimator(
      estimator_id = est_id,
      type = "test",
      generator = legacy_qst_est,
      oracle = FALSE,
      supports_qst = TRUE,
      version = "0.0.0",
      description = "Legacy ATT/QST golden regression estimator",
      source = "test",
      requires_pkgs = character(0)
    )
  }

  legacy <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = est_id,
    n = 60,
    seed = 1303,
    tau = c(0.25, 0.5, 0.75),
    config = list(estimand_targets = c("att", "qst"))
  )
  legacy_att_score <- legacy$scores[legacy$scores$estimand_target_id == "att", ]
  legacy_qst_scores <- legacy$scores[legacy$scores$estimand_target_id == "qst", ]

  expect_equal(legacy$att$estimate, 0.125, tolerance = identity_tol)
  expect_equal(legacy$att$true, 1.0597021813068508, tolerance = reference_tol)
  expect_equal(legacy$att$error, -0.93470218130685079, tolerance = reference_tol)
  expect_equal(legacy_att_score$estimate, legacy$att$estimate, tolerance = identity_tol)
  expect_equal(legacy_att_score$truth, legacy$att$true, tolerance = identity_tol)

  expect_equal(legacy$qst$estimate, c(-0.2, 0, 0.2), tolerance = identity_tol)
  expect_equal(
    legacy$qst$true,
    c(0.82726726467016054, 1.11306126932880201, 1.39767668699081304),
    tolerance = reference_tol
  )
  expect_equal(
    legacy$qst$error,
    c(-1.0272672646701606, -1.1130612693288020, -1.1976766869908131),
    tolerance = reference_tol
  )
  expect_equal(legacy_qst_scores$estimate, legacy$qst$estimate, tolerance = identity_tol)
  expect_equal(legacy_qst_scores$truth, legacy$qst$true, tolerance = identity_tol)
  expect_equal(legacy_qst_scores$error, legacy$qst$error, tolerance = identity_tol)
})
