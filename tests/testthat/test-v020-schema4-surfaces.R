test_that("schema-4 run results carry fit and score identities", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "oracle_att",
    n = 60,
    seed = 101,
    config = list(estimand_targets = c("att", "ate"))
  )

  expect_identical(res$meta$config_fingerprint_schema, 4L)
  expect_true(is.character(res$meta$fit_fingerprint))
  expect_true(is.character(res$meta$truth_version))
  expect_equal(length(unique(res$scores$fit_fingerprint)), 1L)
  expect_equal(unique(res$scores$fit_fingerprint), res$meta$fit_fingerprint)
  expect_equal(unique(res$scores$truth_version), res$meta$truth_version)
  expect_equal(length(res$meta$score_fingerprints), nrow(res$scores))
  expect_equal(sort(res$meta$score_fingerprints), sort(res$scores$score_fingerprint))
  expect_equal(length(unique(res$scores$score_fingerprint)), nrow(res$scores))
  expect_false(any(is.na(res$scores$scoring_population_id)))

  wave2_fields <- c(
    "seed_eval", "n_eval", "eval_derivation", "unit_id_digest",
    "prediction_digest", "transductive"
  )
  expect_true(all(wave2_fields %in% names(res$scores)))
  expect_true(all(is.na(res$scores$seed_eval)))
  expect_true(all(is.na(res$scores$n_eval)))
  expect_true(all(is.na(res$scores$eval_derivation)))
  expect_true(all(is.na(res$scores$unit_id_digest)))
  expect_true(all(is.na(res$scores$prediction_digest)))
  expect_true(all(is.na(res$scores$transductive)))
})

test_that("one fit can produce multiple score records without overwrite", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "oracle_att",
    n = 40,
    seed = 102,
    config = list(estimand_targets = c("att", "ate"))
  )

  expect_equal(nrow(res$scores), 2L)
  expect_equal(length(unique(res$scores$fit_fingerprint)), 1L)
  expect_equal(length(unique(res$scores$score_fingerprint)), 2L)
  expect_equal(sort(res$scores$estimand_target_id), c("ate", "att"))
})

test_that("typed collection returns long scored, non-comparable, and error rows", {
  qst_est_id <- "typed_qst_surface_est"
  if (!qst_est_id %in% CausalStress:::cs_estimator_registry()$estimator_id) {
    qst_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
      list(
        outputs = list(
          att = list(estimate = 0),
          qst = tibble::tibble(tau = tau, estimate = seq_along(tau))
        ),
        meta = list(estimator_id = qst_est_id, oracle = FALSE, supports_qst = TRUE)
      )
    }
    cs_register_estimator(
      estimator_id = qst_est_id,
      type = "test",
      generator = qst_est,
      oracle = FALSE,
      supports_qst = TRUE,
      version = "0.0.0",
      description = "Typed QST surface test estimator",
      source = "test",
      requires_pkgs = character(0)
    )
  }

  scored <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = qst_est_id,
    n = 50,
    seed = 103,
    tau = c(0.25, 0.5, 0.75),
    config = list(estimand_targets = c("att", "qst", "ate"))
  )
  failing_est_id <- "typed_failing_surface_est"
  if (!failing_est_id %in% CausalStress:::cs_estimator_registry()$estimator_id) {
    failing_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
      stop("typed surface failure")
    }
    cs_register_estimator(
      estimator_id = failing_est_id,
      type = "test",
      generator = failing_est,
      oracle = FALSE,
      supports_qst = FALSE,
      version = "0.0.0",
      description = "Typed surface failure test estimator",
      source = "test",
      requires_pkgs = character(0)
    )
  }
  failed <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = failing_est_id,
    n = 50,
    seed = 104,
    config = list(estimand_targets = c("att", "ate"))
  )

  tidy <- cs_tidy(list(scored, failed))
  scores <- cs_collect_scores(tidy)

  expect_s3_class(scores, "tbl_df")
  expect_true(all(c(
    "estimand_target_id", "score_status", "non_comparable_reason",
    "fit_fingerprint", "score_fingerprint", "truth_version",
    "scoring_population_id", "tau", "tau_index"
  ) %in% names(scores)))
  expect_true(any(scores$score_status == "scored"))
  expect_true(any(scores$score_status == "non_comparable"))
  expect_true(any(scores$score_status == "estimator_error"))

  qst_rows <- scores[scores$estimand_target_id == "qst" & scores$score_status == "scored", ]
  expect_equal(nrow(qst_rows), 3L)
  expect_equal(qst_rows$tau_index, 1:3)

  ate_missing <- scores[
    scores$estimator_id == qst_est_id & scores$estimand_target_id == "ate",
  ]
  expect_equal(ate_missing$score_status, "non_comparable")
  expect_equal(ate_missing$non_comparable_reason, "estimator_not_produced")
})

test_that("science payload and meta flatten preserve typed score identity", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "oracle_att",
    n = 40,
    seed = 105,
    config = list(estimand_targets = c("att", "ate"))
  )

  payload <- cs_science_payload(res)
  flat <- cs_meta_flatten(res)

  expect_s3_class(payload$scores, "tbl_df")
  expect_true(all(c("estimand_target_id", "score_status", "non_comparable_reason") %in% names(payload$scores)))
  expect_equal(payload$meta$fit_fingerprint, res$meta$fit_fingerprint)
  expect_equal(payload$meta$truth_version, res$meta$truth_version)
  expect_equal(flat$fit_fingerprint, res$meta$fit_fingerprint)
  expect_equal(flat$truth_version, res$meta$truth_version)
  expect_equal(flat$score_fingerprints[[1]], res$meta$score_fingerprints)
})

test_that("schema 1-3 pins fail closed as resume targets", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()
  dgp_id <- "synth_baseline"
  estimator_id <- "lm_att"
  n <- 30L
  seed <- 1L

  pins::pin_write(
    board = board,
    x = list(
      att = list(estimate = 999, true = 0, error = 999, abs_error = 999),
      qst = NULL,
      meta = list(
        success = TRUE,
        dgp_id = dgp_id,
        estimator_id = estimator_id,
        n = n,
        seed = seed,
        config_fingerprint_schema = 3L,
        config_fingerprint = "historical"
      )
    ),
    name = CausalStress:::cs_result_pin_name(dgp_id, estimator_id, n, seed, "1.6.0"),
    type = "qs",
    metadata = list(
      dgp_id = dgp_id,
      dgp_version = "1.6.0",
      estimator_id = estimator_id,
      n = n,
      seed = seed,
      config_fingerprint_schema = 3L,
      config_fingerprint = "historical"
    )
  )

  expect_error(
    cs_run_seeds(
      dgp_id = dgp_id,
      estimator_id = estimator_id,
      n = n,
      seeds = seed,
      board = board,
      skip_existing = TRUE,
      show_progress = FALSE,
      quiet = TRUE
    ),
    class = "causalstress_schema_migration_error"
  )
})
