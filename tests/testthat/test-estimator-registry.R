test_that("cs_estimator_registry has required columns and sane types", {
  reg <- CausalStress:::cs_estimator_registry()

  required_cols <- c(
    "estimator_id", "type", "generator",
    "oracle", "supports_qst",
    "version", "description",
    "source", "requires_pkgs"
  )
  expect_true(all(required_cols %in% names(reg)))

  expect_true(is.character(reg$estimator_id))
  expect_true(is.character(reg$type))
  expect_true(is.character(reg$version))
  expect_true(is.character(reg$description))
  expect_true(is.character(reg$source))

  expect_true(all(vapply(reg$generator, is.function, logical(1))))
  expect_true(is.logical(reg$oracle))
  expect_true(is.logical(reg$supports_qst))
  expect_true(all(vapply(reg$requires_pkgs, is.character, logical(1))))
})

test_that("core estimators are present and marked as source = 'core'", {
  reg <- CausalStress:::cs_estimator_registry()

  core_ids <- c("oracle_att", "lm_att", "ipw_att")
  expect_true(all(core_ids %in% reg$estimator_id))

  core <- reg[reg$estimator_id %in% core_ids, , drop = FALSE]
  expect_true(all(core$source == "core"))
})

test_that("cs_get_estimator returns a valid descriptor for oracle_att", {
  desc <- cs_get_estimator("oracle_att")

  expect_type(desc, "list")
  expect_equal(desc$estimator_id, "oracle_att")
  expect_true(is.function(desc$generator))
  expect_true(desc$oracle)
  expect_false(desc$supports_qst)
  expect_type(desc$version, "character")
  expect_type(desc$description, "character")
  expect_equal(desc$source, "core")

  dgp_desc <- cs_get_dgp("synth_baseline")
  dgp      <- dgp_desc$generator(n = 200, seed = 123L)
  df <- dgp$df

  est <- desc$generator(df = df, tau = cs_tau_oracle, config = list())
  expect_invisible(cs_check_estimator_output(est, require_qst = FALSE))

  att_structural <- cs_true_att(
    structural_te = df$structural_te,
    w             = df$w
  )

  expect_equal(est$att$estimate, att_structural)
})

test_that("cs_get_estimator fails for unknown estimator IDs", {
  expect_error(
    cs_get_estimator("does_not_exist"),
    class = "causalstress_registry_error"
  )
})

test_that("cs_get_estimator returns a valid descriptor for ipw_att", {
  desc <- cs_get_estimator("ipw_att")

  expect_equal(desc$estimator_id, "ipw_att")
  expect_false(desc$oracle)
  expect_false(desc$supports_qst)
  expect_true(is.function(desc$generator))
  expect_equal(desc$source, "core")

  dgp_desc <- cs_get_dgp("synth_baseline")
  dgp      <- dgp_desc$generator(n = 200, seed = 42L)
  df <- dgp$df

  est <- desc$generator(df = df, tau = cs_tau_oracle, config = list())
  expect_invisible(cs_check_estimator_output(est, require_qst = FALSE))
})

test_that("cs_register_estimator adds a new estimator and prevents duplicates", {
  dummy_fun <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    list(
      att  = list(estimate = 0),
      qst  = NULL,
      meta = list(
        estimator_id = "dummy_ext",
        oracle       = FALSE,
        supports_qst = FALSE
      )
    )
  }

  cs_register_estimator(
    estimator_id  = "dummy_ext",
    type          = "external",
    generator     = dummy_fun,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.1",
    description   = "Dummy external estimator for testing.",
    source        = "external",
    requires_pkgs = c("somePackage")
  )

  reg <- CausalStress:::cs_estimator_registry()
  expect_true("dummy_ext" %in% reg$estimator_id)

  desc <- cs_get_estimator("dummy_ext")
  expect_equal(desc$estimator_id, "dummy_ext")
  expect_equal(desc$source, "external")
  expect_equal(desc$requires_pkgs, c("somePackage"))

  expect_error(
    cs_register_estimator(
      estimator_id = "dummy_ext",
      type         = "external",
      generator    = dummy_fun
    ),
    class = "causalstress_registry_error"
  )
})
