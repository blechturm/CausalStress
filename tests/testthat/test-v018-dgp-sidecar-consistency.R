test_that("YAML sidecars match cs_dgp_executable_meta() (noise/effect)", {
  reg <- cs_dgp_registry()
  expect_gt(nrow(reg), 0L)

  for (i in seq_len(nrow(reg))) {
    id <- reg$dgp_id[[i]]
    ver <- reg$version[[i]]

    exec <- cs_dgp_executable_meta(id, ver)
    expect_true(is.list(exec))
    expect_true(all(c("noise_family", "effect_type") %in% names(exec)))

    path <- system.file("dgp_meta", paste0(id, ".yml"), package = "CausalStress")
    if (path == "") path <- file.path("inst", "dgp_meta", paste0(id, ".yml"))
    expect_true(file.exists(path))

    yml <- yaml::read_yaml(path)
    expect_identical(as.character(yml$stress_profile$noise), as.character(exec$noise_family))
    expect_identical(as.character(yml$stress_profile$effect), as.character(exec$effect_type))
  }
})

test_that("known sidecar regressions are fixed (noise/effect truth-in-labeling)", {
  expect_identical(cs_dgp_executable_meta("synth_qte1", "1.3.0")$noise_family, "heavy")
  expect_identical(cs_dgp_executable_meta("synth_heavytail", "1.3.0")$effect_type, "linear")
  expect_identical(cs_dgp_executable_meta("synth_overlap_stressed", "1.3.0")$effect_type, "linear")
  expect_identical(cs_dgp_executable_meta("synth_tilt_mild", "1.3.0")$effect_type, "linear")
  expect_identical(cs_dgp_executable_meta("synth_nonlinear_heteroskedastic", "1.3.0")$effect_type, "constant")
})

