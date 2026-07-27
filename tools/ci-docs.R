# Verify the governed documentation toolchain and Quarto-only source policy.

description <- read.dcf("DESCRIPTION")
required <- c(
  quarto_cli = unname(description[1L, "Config/CausalStress/QuartoCLI"]),
  quarto_r = unname(description[1L, "Config/CausalStress/QuartoR"]),
  pkgdown = unname(description[1L, "Config/CausalStress/pkgdown"])
)

installed <- c(
  quarto_r = if (requireNamespace("quarto", quietly = TRUE)) {
    as.character(utils::packageVersion("quarto"))
  } else {
    NA_character_
  },
  pkgdown = if (requireNamespace("pkgdown", quietly = TRUE)) {
    as.character(utils::packageVersion("pkgdown"))
  } else {
    NA_character_
  }
)
if (!identical(unname(installed), unname(required[c("quarto_r", "pkgdown")]))) {
  stop(
    "Documentation R-package pins do not match: required ",
    paste(required[c("quarto_r", "pkgdown")], collapse = "/"),
    "; found ", paste(installed, collapse = "/"), ".",
    call. = FALSE
  )
}
if (!quarto::quarto_available() ||
    !identical(as.character(quarto::quarto_version()), required[["quarto_cli"]])) {
  found <- if (quarto::quarto_available()) as.character(quarto::quarto_version()) else "unavailable"
  stop(
    "Quarto CLI ", required[["quarto_cli"]], " is required; found ", found, ".",
    call. = FALSE
  )
}

imports <- description[1L, "Imports"]
if (grepl("(^|[,[:space:]])(quarto|pkgdown)([,([:space:]]|$)", imports)) {
  stop("quarto and pkgdown must remain outside runtime Imports.", call. = FALSE)
}
if (!identical(unname(description[1L, "VignetteBuilder"]), "quarto")) {
  stop("DESCRIPTION must declare VignetteBuilder: quarto.", call. = FALSE)
}

forbidden_rmd <- c(
  if (file.exists("README.Rmd")) "README.Rmd" else character(),
  list.files("vignettes", pattern = "[.]Rmd$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("inst", "dgp_meta"), pattern = "[.]Rmd$", recursive = TRUE, full.names = TRUE)
)
if (length(forbidden_rmd)) {
  stop("Current long-form sources must be QMD, not Rmd: ", paste(forbidden_rmd, collapse = ", "), call. = FALSE)
}

article_qmd <- list.files("vignettes", pattern = "[.]qmd$", recursive = FALSE, full.names = TRUE)
dossier_qmd <- list.files(file.path("inst", "dgp_meta"), pattern = "[.]qmd$", recursive = FALSE, full.names = TRUE)
if (length(article_qmd) != 4L || length(dossier_qmd) != 12L || !file.exists("README.qmd")) {
  stop("Expected README.qmd, four Quarto articles, and 12 Quarto DGP dossiers.", call. = FALSE)
}

article_headers <- vapply(article_qmd, function(path) {
  paste(utils::head(readLines(path, warn = FALSE, encoding = "UTF-8"), 12L), collapse = "\n")
}, character(1))
if (any(!grepl("VignetteEngine[{]quarto::html[}]", article_headers, fixed = FALSE))) {
  stop("Every package article must use the quarto::html vignette engine.", call. = FALSE)
}

readme_source <- paste(readLines("README.qmd", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
heavytail_source <- paste(
  readLines(file.path("inst", "dgp_meta", "synth_heavytail.qmd"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
protected_readme <- c(
  "Its `true_att` is the governed finite-sample structural signal anchor",
  "potential-outcome ATT, which does not exist under its Cauchy-mixture noise.",
  "RMSE, coverage, or ranking for an ATT shootout. Use QST for valid distributional"
)
protected_heavytail <- c(
  "not a conventional superpopulation mean potential-outcome ATT in",
  "QST/quantile targets:** well-defined under the Cauchy mixture and the valid",
  "This reduces Monte Carlo variance of the QST contrast; it does not eliminate empirical-quantile sampling uncertainty."
)
if (any(!vapply(protected_readme, grepl, logical(1), x = readme_source, fixed = TRUE)) ||
    any(!vapply(protected_heavytail, grepl, logical(1), x = heavytail_source, fixed = TRUE))) {
  stop("CS-1229 protected heavy-tail or oracle-uncertainty prose drifted.", call. = FALSE)
}

message(
  "Documentation substrate verified: Quarto CLI ", required[["quarto_cli"]],
  ", quarto R ", required[["quarto_r"]], ", pkgdown ", required[["pkgdown"]],
  "; README + 4 articles + 12 dossiers are Quarto-only."
)
