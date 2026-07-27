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
  list.files(file.path("inst", "dgp_meta"), pattern = "[.]Rmd$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("inst", "templates"), pattern = "[.]Rmd$", recursive = TRUE, full.names = TRUE)
)
if (length(forbidden_rmd)) {
  stop("Current long-form sources must be QMD, not Rmd: ", paste(forbidden_rmd, collapse = ", "), call. = FALSE)
}

article_qmd <- list.files("vignettes", pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
dossier_qmd <- list.files(file.path("inst", "dgp_meta"), pattern = "[.]qmd$", recursive = FALSE, full.names = TRUE)
required_articles <- file.path(
  "vignettes",
  paste0(
    c(
      "canonical-workflow", "estimator-extension", "native-dgp-contribution",
      "benchmarking-with-suites", "caching-and-resume",
      "distributional_intelligence", "from-run-to-history"
    ),
    ".qmd"
  )
)
if (!setequal(article_qmd, required_articles) ||
    length(dossier_qmd) != 12L ||
    !file.exists("README.qmd") ||
    !file.exists(file.path("inst", "templates", "dgp_dossier_v1.qmd"))) {
  stop(
    "Expected README.qmd, seven named Quarto articles, the Quarto dossier template, and 12 Quarto DGP dossiers.",
    call. = FALSE
  )
}

article_headers <- vapply(article_qmd, function(path) {
  paste(utils::head(readLines(path, warn = FALSE, encoding = "UTF-8"), 12L), collapse = "\n")
}, character(1))
if (any(!grepl("VignetteEngine[{]quarto::html[}]", article_headers, fixed = FALSE))) {
  stop("Every package article must use the quarto::html vignette engine.", call. = FALSE)
}

site_config <- yaml::read_yaml("_pkgdown.yml")
indexed_articles <- unlist(
  lapply(site_config$articles, function(group) {
    if (is.null(group$contents)) character() else group$contents
  }),
  use.names = FALSE
)
required_article_ids <- tools::file_path_sans_ext(basename(required_articles))
if (!identical(sort(indexed_articles), sort(required_article_ids)) ||
    anyDuplicated(indexed_articles)) {
  stop("Every committed package article must be indexed exactly once in _pkgdown.yml.", call. = FALSE)
}

dossier_ids <- tools::file_path_sans_ext(basename(dossier_qmd))
dossier_menu <- site_config$navbar$components$dgps$menu
indexed_dossiers <- vapply(dossier_menu, `[[`, character(1), "href")
expected_dossiers <- file.path("dgp", paste0(dossier_ids, ".html"))
if (!setequal(indexed_dossiers, expected_dossiers) ||
    length(indexed_dossiers) != length(expected_dossiers) ||
    anyDuplicated(indexed_dossiers)) {
  stop("Every DGP dossier must be indexed exactly once in _pkgdown.yml.", call. = FALSE)
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

article_sources <- vapply(article_qmd, function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}, character(1))
rd_files <- list.files("man", pattern = "[.]Rd$", full.names = TRUE)
rd_sources <- vapply(rd_files, function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}, character(1))
public_sources <- c(readme_source, article_sources, rd_sources)
stale_public_claims <- c(
  "v0.1.", "gate$verdict", "gate$culprits", "results are never just returned",
  "0 seconds", "Map-Reduce", "Universal converter", "fn: function",
  "qs::", "qsave(", "qread("
)
for (claim in stale_public_claims) {
  if (any(grepl(claim, public_sources, fixed = TRUE))) {
    stop("Known stale public documentation claim remains: ", claim, call. = FALSE)
  }
}

canonical_source <- article_sources[[file.path("vignettes", "canonical-workflow.qmd")]]
estimator_source <- article_sources[[file.path("vignettes", "estimator-extension.qmd")]]
dgp_source <- article_sources[[file.path("vignettes", "native-dgp-contribution.qmd")]]
required_canonical <- c(
  "cs_run_single()", "cs_run_grid()", "cs_collect_scores()",
  "Targets are never silently cross-scored", "CATE scoring is not implemented",
  "skip_existing = TRUE", "cs_audit(board)"
)
required_estimator <- c(
  "cs_register_estimator()", "process-local", "typed `outputs`",
  "requires_pkgs", "causalstress_airlock_error"
)
required_dgp <- c(
  "public runtime DGP-registration API", "`X1`, ..., `Xk`",
  "real-data DGPs", "user-defined families", "cs_validate_dgp()"
)
if (any(!vapply(required_canonical, grepl, logical(1), x = canonical_source, fixed = TRUE)) ||
    any(!vapply(required_estimator, grepl, logical(1), x = estimator_source, fixed = TRUE)) ||
    any(!vapply(required_dgp, grepl, logical(1), x = dgp_source, fixed = TRUE))) {
  stop("A required Batch 3 contract statement is missing from the new guides.", call. = FALSE)
}
if (grepl("cs_register_dgp[(]", paste(public_sources, collapse = "\n"))) {
  stop("Documentation must not invent a public runtime DGP-registration function.", call. = FALSE)
}
if (!grepl("optional-qst-status", canonical_source, fixed = TRUE) ||
    !grepl("optional-gengc-status", article_sources[[file.path("vignettes", "distributional_intelligence.qmd")]], fixed = TRUE)) {
  stop("Optional estimator examples must expose their skipped state.", call. = FALSE)
}

message(
  "Documentation substrate verified: Quarto CLI ", required[["quarto_cli"]],
  ", quarto R ", required[["quarto_r"]], ", pkgdown ", required[["pkgdown"]],
  "; README + 7 articles + dossier template + 12 dossiers are Quarto-only."
)
