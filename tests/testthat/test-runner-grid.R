test_that("cs_run_grid returns expected rows and structure", {
  dgp_ids       <- c("synth_baseline", "synth_heavytail")
  estimator_ids <- c("lm_att", "ipw_att")
  seeds         <- 1:3

  runs <- cs_run_grid(
    dgp_ids       = dgp_ids,
    estimator_ids = estimator_ids,
    n             = 200,
    seeds         = seeds
  )

  expect_s3_class(runs, "tbl_df")
  expect_equal(
    nrow(runs),
    length(dgp_ids) * length(estimator_ids) * length(seeds)
  )

  expect_true(all(runs$dgp_id %in% dgp_ids))
  expect_true(all(runs$estimator_id %in% estimator_ids))
  expect_true(all(runs$seed %in% seeds))

  expected_cols <- c(
    "dgp_id", "estimator_id", "n", "seed",
    "oracle", "supports_qst",
    "true_att", "est_att", "att_error", "att_abs_error",
    "att_ci_lo", "att_ci_hi", "att_covered", "att_ci_width", "n_boot_ok",
    "estimator_pkgs", "log"
  )
  expect_true(all(expected_cols %in% names(runs)))
})

test_that("cs_run_grid fails for unknown DGP IDs", {
  expect_error(
    cs_run_grid(
      dgp_ids       = c("synth_baseline", "does_not_exist"),
      estimator_ids = "lm_att",
      n             = 100,
      seeds         = 1:2
    ),
    class = "causalstress_registry_error"
  )
})

test_that("cs_run_grid fails for unknown estimator IDs", {
  expect_error(
    cs_run_grid(
      dgp_ids       = "synth_baseline",
      estimator_ids = c("lm_att", "does_not_exist"),
      n             = 100,
      seeds         = 1:2
    ),
    class = "causalstress_registry_error"
  )
})

test_that("cs_run_grid output works with cs_summarise_runs", {
  runs <- cs_run_grid(
    dgp_ids       = c("synth_baseline", "synth_heavytail"),
    estimator_ids = c("lm_att", "ipw_att"),
    n             = 200,
    seeds         = 1:5
  )

  summary <- cs_summarise_runs(runs)

  expect_s3_class(summary, "tbl_df")
  expect_true(all(c("dgp_id", "estimator_id", "n", "n_runs") %in% names(summary)))
  expect_true(all(summary$n_runs == 5L))
})

test_that("cs_run_grid preserves omitted, NULL, default, and custom tau contracts", {
  estimator_id <- "runner_grid_tau_characterization"
  registry_env <- get(
    ".causalstress_estimator_registry_extra",
    envir = asNamespace("CausalStress")
  )
  registry_before <- registry_env$tbl
  withr::defer(registry_env$tbl <- registry_before)

  cs_register_estimator(
    estimator_id = estimator_id,
    type = "test",
    generator = function(df, config = list(), tau = cs_tau_oracle, ...) {
      list(
        att = list(estimate = as.numeric(config$level %||% 0)),
        qst = tibble::tibble(tau = tau, value = seq_along(tau)),
        meta = list(
          estimator_id = "runner_grid_tau_characterization",
          oracle = FALSE,
          supports_qst = TRUE
        )
      )
    },
    oracle = FALSE,
    supports_qst = TRUE,
    version = "0.0.0-test",
    description = "Grid tau characterization estimator",
    source = "test",
    requires_pkgs = character(0)
  )

  args <- list(
    dgp_ids = "synth_baseline",
    estimator_ids = estimator_id,
    n = 30L,
    seeds = c(3L, 1L),
    config = list(
      level = 0.25,
      ci_method = "none",
      estimand_targets = c("att", "qst")
    ),
    show_progress = FALSE
  )

  omitted <- suppressMessages(do.call(cs_run_grid, args))
  explicit_null <- suppressMessages(do.call(cs_run_grid, c(args, list(tau = NULL))))
  explicit_default <- suppressMessages(do.call(
    cs_run_grid,
    c(args, list(tau = cs_tau_oracle))
  ))
  custom_tau <- c(0.2, 0.8)
  custom <- suppressMessages(do.call(cs_run_grid, c(args, list(tau = custom_tau))))

  expect_null(formals(cs_run_grid)$tau)
  expect_identical(omitted$seed, c(1L, 3L))
  for (result in list(explicit_null, explicit_default)) {
    expect_identical(result$seed, omitted$seed)
    expect_identical(result$qst, omitted$qst)
    expect_identical(result$scores, omitted$scores)
    expect_identical(result$config_fingerprint, omitted$config_fingerprint)
    expect_identical(result$fit_fingerprint, omitted$fit_fingerprint)
    expect_identical(result$truth_version, omitted$truth_version)
    expect_identical(result$score_fingerprints, omitted$score_fingerprints)
    expect_identical(
      result$score_row_fingerprints,
      omitted$score_row_fingerprints
    )
  }

  expect_identical(custom$seed, c(1L, 3L))
  for (qst in custom$qst) {
    expect_identical(qst$tau, custom_tau)
    expect_identical(qst$tau_id, cs_tau_id(custom_tau))
  }
  for (scores in custom$scores) {
    qst_scores <- scores[scores$estimand_target_id == "qst", ]
    expect_identical(qst_scores$tau, custom_tau)
    expect_identical(qst_scores$tau_id, cs_tau_id(custom_tau))
  }
  expect_false(identical(custom$config_fingerprint, omitted$config_fingerprint))
  expect_false(identical(custom$fit_fingerprint, omitted$fit_fingerprint))
  expect_false(identical(custom$score_fingerprints, omitted$score_fingerprints))
})
