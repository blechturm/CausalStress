make_resolve_reg <- function() {
  tibble::tibble(
    dgp_id = c("foo", "foo", "foo", "bar"),
    type = "synthetic",
    generator = list(
      function(n, seed = NULL) list(),
      function(n, seed = NULL) list(),
      function(n, seed = NULL) list(),
      function(n, seed = NULL) list()
    ),
    version = c("1.0.0", "1.1.0", "0.9.0", "1.0.0"),
    description = "desc",
    status = c("stable", "experimental", "deprecated", "stable"),
    rationale = c("", "", "old", ""),
    date_status_changed = NA_character_,
    design_spec = "1.0.0"
  )
}

test_that("cs_resolve_dgp selects stable by default", {
  reg <- make_resolve_reg()
  res <- cs_resolve_dgp(reg, dgp_id = "foo", quiet = TRUE)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$version[[1]], "1.0.0")
})

test_that("cs_resolve_dgp falls back to experimental with warning", {
  reg <- make_resolve_reg()
  reg$status[reg$dgp_id == "foo" & reg$version == "1.0.0"] <- "experimental"
  expect_warning(res <- cs_resolve_dgp(reg, dgp_id = "foo", quiet = FALSE), "experimental")
  expect_equal(res$version[[1]], "1.1.0")
})

test_that("cs_resolve_dgp errors when no suitable versions", {
  reg <- make_resolve_reg()
  reg <- reg[reg$dgp_id == "bar", , drop = FALSE]
  reg$status <- "deprecated"
  expect_error(cs_resolve_dgp(reg, dgp_id = "bar", quiet = TRUE), "No stable or experimental")
})

test_that("cs_resolve_dgp respects explicit version and warns on deprecated", {
  reg <- make_resolve_reg()
  expect_warning(
    res <- cs_resolve_dgp(reg, dgp_id = "foo", version = "0.9.0", quiet = FALSE),
    "deprecated"
  )
  expect_equal(res$version[[1]], "0.9.0")
})

test_that("cs_resolve_dgp errors on semver ties or malformed versions", {
  reg <- make_resolve_reg()
  reg$version[reg$dgp_id == "foo" & reg$version == "1.1.0"] <- "bad"
  expect_error(cs_resolve_dgp(reg, dgp_id = "foo", status = "experimental", quiet = TRUE), "Malformed")

  tie_reg <- tibble::tibble(
    dgp_id = c("foo", "foo"),
    type = "synthetic",
    generator = list(function(n, seed = NULL) list(), function(n, seed = NULL) list()),
    version = c("1.0.0", "1.0.0"),
    description = "desc",
    status = c("stable", "stable"),
    rationale = c("", ""),
    date_status_changed = NA_character_,
    design_spec = "1.0.0"
  )
  expect_error(
    cs_resolve_dgp(tie_reg, dgp_id = "foo", status = "stable", quiet = TRUE),
    regexp = "Multiple candidates",
    class = "causalstress_registry_error"
  )
})
