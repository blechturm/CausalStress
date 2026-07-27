# Verify the assembled pkgdown site after its articles and DGP reports render.

if (!requireNamespace("xml2", quietly = TRUE)) {
  stop("Package 'xml2' is required for site validation.", call. = FALSE)
}
if (!dir.exists("docs") || !file.exists(file.path("docs", "index.html"))) {
  stop("Build the pkgdown site before running tools/ci-site.R.", call. = FALSE)
}

expected_library <- Sys.getenv("CAUSALSTRESS_DOC_LIB")
if (!nzchar(expected_library)) {
  stop("CAUSALSTRESS_DOC_LIB must identify the clean installation library.", call. = FALSE)
}
expected_library <- normalizePath(expected_library, winslash = "/", mustWork = TRUE)
installed_package <- normalizePath(find.package("CausalStress"), winslash = "/", mustWork = TRUE)
if (!identical(dirname(installed_package), expected_library)) {
  stop(
    "Documentation resolved CausalStress from ", installed_package,
    "; expected the clean installation in ", expected_library, ".",
    call. = FALSE
  )
}

source_version <- unname(read.dcf("DESCRIPTION")[1L, "Version"])
if (!identical(as.character(utils::packageVersion("CausalStress")), source_version)) {
  stop("The installed package version does not match the source DESCRIPTION.", call. = FALSE)
}

registry <- get("cs_dgp_registry", envir = asNamespace("CausalStress"))()
dgp_ids <- unique(registry$dgp_id)
expected_dossiers <- paste0(dgp_ids, ".html")
rendered_dossiers <- list.files(file.path("docs", "dgp"), pattern = "[.]html$")
if (length(dgp_ids) != 12L || anyDuplicated(dgp_ids) ||
    !setequal(rendered_dossiers, expected_dossiers) ||
    length(rendered_dossiers) != 12L) {
  stop("The assembled site must contain exactly 12 registry-keyed DGP reports.", call. = FALSE)
}

site_config <- yaml::read_yaml("_pkgdown.yml")
configured_dossiers <- vapply(
  site_config$navbar$components$dgps$menu,
  `[[`,
  character(1),
  "href"
)
if (!setequal(configured_dossiers, file.path("dgp", expected_dossiers)) ||
    length(configured_dossiers) != 12L || anyDuplicated(configured_dossiers)) {
  stop("The site navigation must index each registry-keyed DGP report exactly once.", call. = FALSE)
}

site_root <- normalizePath("docs", winslash = "/", mustWork = TRUE)
html_files <- list.files(site_root, pattern = "[.]html$", recursive = TRUE, full.names = TRUE)
site_path <- sub("^https?://[^/]+", "", site_config$url)
site_path <- paste0("/", trimws(site_path, whitespace = "/"), "/")
id_cache <- new.env(parent = emptyenv())

document_ids <- function(path) {
  key <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!exists(key, envir = id_cache, inherits = FALSE)) {
    document <- xml2::read_html(key)
    ids <- c(
      xml2::xml_attr(xml2::xml_find_all(document, "//*[@id]"), "id"),
      xml2::xml_attr(xml2::xml_find_all(document, "//a[@name]"), "name")
    )
    assign(key, unique(ids[!is.na(ids)]), envir = id_cache)
  }
  get(key, envir = id_cache, inherits = FALSE)
}

broken <- character()
for (source in html_files) {
  document <- xml2::read_html(source)
  hrefs <- unique(xml2::xml_attr(xml2::xml_find_all(document, "//a[@href]"), "href"))
  hrefs <- hrefs[!is.na(hrefs) & nzchar(hrefs)]

  for (href in hrefs) {
    if (startsWith(href, site_config$url)) {
      href <- paste0(site_path, substring(href, nchar(site_config$url) + 1L))
    }
    if (grepl("^(?:[A-Za-z][A-Za-z0-9+.-]*:|//)", href, perl = TRUE)) {
      next
    }

    location <- strsplit(href, "#", fixed = TRUE)[[1L]]
    relative <- sub("[?].*$", "", location[[1L]])
    fragment <- if (grepl("#", href, fixed = TRUE)) {
      utils::URLdecode(sub("^[^#]*#", "", href))
    } else {
      ""
    }

    if (startsWith(relative, "/")) {
      if (!startsWith(relative, site_path)) {
        broken <- c(broken, paste(source, href, "outside configured site path"))
        next
      }
      relative <- substring(relative, nchar(site_path) + 1L)
      target <- file.path(site_root, relative)
    } else if (!nzchar(relative)) {
      target <- source
    } else {
      target <- file.path(dirname(source), utils::URLdecode(relative))
    }

    if (endsWith(relative, "/") || !nzchar(relative)) {
      if (dir.exists(target)) target <- file.path(target, "index.html")
    }
    target <- normalizePath(target, winslash = "/", mustWork = FALSE)
    if (!startsWith(target, paste0(site_root, "/")) && !identical(target, site_root)) {
      broken <- c(broken, paste(source, href, "escapes site root"))
      next
    }
    if (!file.exists(target)) {
      broken <- c(broken, paste(source, href, "missing target"))
      next
    }
    if (nzchar(fragment) && grepl("[.]html$", target, ignore.case = TRUE) &&
        !fragment %in% document_ids(target)) {
      broken <- c(broken, paste(source, href, "missing fragment"))
    }
  }
}

if (length(broken)) {
  stop(
    "Broken internal site links:\n", paste(unique(broken), collapse = "\n"),
    call. = FALSE
  )
}

message(
  "Integrated site verified from ", installed_package, ": ",
  length(html_files), " HTML pages, 12 indexed DGP reports, and no broken internal links."
)
