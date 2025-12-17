# Bootstrap utilities (native CI)

#' Derive a deterministic seed from a base seed and salt string
#' @noRd
cs_derive_seed <- function(base_seed, salt_string) {
  if (is.null(base_seed) || is.na(base_seed)) stop("Invalid base_seed")
  base_num <- as.numeric(base_seed[1])
  if (!is.finite(base_num)) stop("Invalid base_seed")
  # bring into a safe range before integer conversion to avoid overflow to NA
  base_mod <- (abs(base_num) %% 2147483646) + 1
  h_num <- as.numeric(base_mod)
  if (is.na(h_num)) {
    stop(paste0("Invalid derived seed: base_seed=", base_seed, " base_mod=", base_mod))
  }
  chars <- utf8ToInt(salt_string)
  for (c in chars) {
    h_num <- (h_num * 31 + c) %% 2147483647
  }
  if (h_num <= 0) h_num <- h_num + 2147483647
  as.integer(h_num)
}

#' Validate CI bounds (per-dimension)
#' @noRd
cs_validate_ci <- function(lo, hi) {
  valid_by_dim <- is.finite(lo) & is.finite(hi) & (lo <= hi)
  collapsed <- valid_by_dim & ((hi - lo) < 1e-8)
  list(valid_by_dim = valid_by_dim, collapsed = collapsed)
}

#' Bootstrap percentile CI with gating and validation
#' @noRd
cs_bootstrap_ci <- function(stat_fn, df, n_boot = 200, seed, alpha = 0.05) {
  cs_set_rng(seed)

  # Probe for first successful statistic to establish k
  k <- NULL
  for (i in seq_len(10L)) {
    idx <- sample.int(nrow(df), size = nrow(df), replace = TRUE)
    boot_df <- df[idx, , drop = FALSE]
    ok <- FALSE
    val <- try(stat_fn(boot_df), silent = TRUE)
    if (!inherits(val, "try-error") && is.numeric(val) && length(val) > 0L) {
      k <- length(val)
      ok <- TRUE
    }
    if (ok) break
  }

  if (is.null(k)) {
    return(list(
      ci_lo = numeric(0),
      ci_hi = numeric(0),
      meta = list(
        n_boot_ok      = integer(0),
        n_boot_fail    = integer(0),
        ci_valid_by_dim = logical(0),
        collapsed      = logical(0),
        ci_valid       = FALSE,
        ci_fail_code   = "initial_failure",
        ci_method      = "bootstrap",
        ci_type        = "percentile",
        ci_level       = 1 - alpha
      )
    ))
  }

  mat <- matrix(NA_real_, nrow = n_boot, ncol = k)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(nrow(df), size = nrow(df), replace = TRUE)
    boot_df <- df[idx, , drop = FALSE]
    val <- try(stat_fn(boot_df), silent = TRUE)
    if (!inherits(val, "try-error") && is.numeric(val) && length(val) == k) {
      mat[b, ] <- val
    }
  }

  n_boot_ok <- colSums(!is.na(mat))
  n_boot_fail <- n_boot - n_boot_ok
  dim_gated_ok <- n_boot_ok >= (n_boot * 0.9)

  ci_lo <- rep(NA_real_, k)
  ci_hi <- rep(NA_real_, k)
  for (j in seq_len(k)) {
    if (dim_gated_ok[j]) {
      col_vals <- mat[, j]
      col_vals <- col_vals[!is.na(col_vals)]
      ci_lo[j] <- stats::quantile(col_vals, alpha / 2, na.rm = TRUE)
      ci_hi[j] <- stats::quantile(col_vals, 1 - alpha / 2, na.rm = TRUE)
    }
  }

  val_res <- cs_validate_ci(ci_lo, ci_hi)
  ci_valid <- any(dim_gated_ok) && all(val_res$valid_by_dim[dim_gated_ok])

  ci_fail_code <- NA_character_
  if (any(!dim_gated_ok)) {
    ci_fail_code <- "low_boot_success"
  } else if (!ci_valid) {
    ci_fail_code <- "invalid_bounds"
  }

  list(
    ci_lo = ci_lo,
    ci_hi = ci_hi,
    meta = list(
      n_boot_ok       = n_boot_ok,
      n_boot_fail     = n_boot_fail,
      ci_valid_by_dim = val_res$valid_by_dim,
      collapsed       = val_res$collapsed,
      ci_valid        = ci_valid,
      ci_fail_code    = ci_fail_code,
      ci_method       = "bootstrap",
      ci_type         = "percentile",
      ci_level        = 1 - alpha
    )
  )
}
