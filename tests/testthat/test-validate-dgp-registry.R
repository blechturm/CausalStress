withr::local_options(warn = 1)

make_dummy_reg <- function(status = "stable", version = "1.0.0", design_spec = "1.0.0", rationale = "Initial") {
  tibble::tibble(
    dgp_id = "foo",
    type = "synthetic",
    generator = list(function(n, seed = NULL) {
      if (!is.null(seed)) cs_set_rng(seed)
      list(
        df = tibble::tibble(y0 = rep(0, n), y1 = rep(0, n), p = rep(0.5, n), structural_te = rep(0, n)),
        true_att = 0,
        true_qst = tibble::tibble(tau = cs_tau_oracle, value = rep(0, length(cs_tau_oracle))),
        meta = list()
      )
    }),
    version = version,
    description = "dummy",
    status = status,
    rationale = rationale,
    date_status_changed = NA_character_,
    design_spec = design_spec
  )
}

test_that("real registry validates (with informational warnings)", {
  res <- cs_validate_dgp_registry(strict = FALSE)
  expect_s3_class(res, "tbl_df")
})

test_that("registry validation catches structural violations", {
  base_reg <- make_dummy_reg()

  with_mocked_bindings(
    cs_dgp_registry = function() {
      reg <- base_reg
      reg$status <- "bad"
      reg
    },
    expect_error(cs_validate_dgp_registry(strict = TRUE), "unsupported status", ignore.case = TRUE)
  )

  with_mocked_bindings(
    cs_dgp_registry = function() {
      rbind(base_reg, base_reg)
    },
    expect_error(cs_validate_dgp_registry(strict = TRUE), "duplicate", ignore.case = TRUE)
  )

  with_mocked_bindings(
    cs_dgp_registry = function() {
      reg <- base_reg
      reg$version <- "1.a"
      reg
    },
    expect_error(cs_validate_dgp_registry(strict = TRUE), "malformed", ignore.case = TRUE)
  )

  with_mocked_bindings(
    cs_dgp_registry = function() {
      reg <- base_reg
      reg$status <- "deprecated"
      reg$rationale <- ""
      reg
    },
    expect_error(cs_validate_dgp_registry(strict = TRUE), "rationale", ignore.case = TRUE)
  )

  with_mocked_bindings(
    cs_dgp_registry = function() {
      reg <- base_reg
      reg$generator[[1]] <- 1
      reg
    },
    expect_error(cs_validate_dgp_registry(strict = TRUE), "non-function", ignore.case = TRUE)
  )
})

test_that("registry validation enforces single stable per dgp_id", {
  reg_two_stable <- make_dummy_reg()
  reg_two_stable <- rbind(reg_two_stable, reg_two_stable)
  reg_two_stable$version[2] <- "1.1.0"

  with_mocked_bindings(
    cs_dgp_registry = function() reg_two_stable,
    expect_error(cs_validate_dgp_registry(strict = TRUE), "more than one stable", ignore.case = TRUE)
  )
})
