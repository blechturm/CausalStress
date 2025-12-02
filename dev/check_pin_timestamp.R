#!/usr/bin/env Rscript

# Simple smoke test to check whether pin timestamps advance on recompute.
# Usage: Rscript dev/check_pin_timestamp.R

ts_to_char <- function(ts) {
  if (is.null(ts) || is.na(ts)) return("NA")
  if (is.list(ts)) ts <- ts[[1]]
  if (inherits(ts, "POSIXct")) return(format(ts, "%H:%M:%OS3"))
  as.character(ts)
}

options(width = 80, cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)

devtools::load_all(quiet = TRUE)

library(glue)
library(pins)
library(dplyr)

board <- pins::board_temp()
dgp   <- "synth_baseline"
est   <- "lm_att"
n     <- 100
seed  <- 1

name <- glue("results__dgp={dgp}__est={est}__n={n}__seed={seed}")

get_ts_meta <- function() {
  pm <- pins::pin_meta(board, name)
  pm$metadata$timestamp %||% pm$timestamp
}

get_ts_obj <- function() {
  obj <- pins::pin_read(board, name)
  obj$meta$timestamp %||% obj$meta$run_timestamp
}

run_once <- function(skip = FALSE, force = FALSE) {
  invisible(cs_run_seeds(
    dgp, est, n = n, seeds = seed,
    bootstrap = FALSE,
    board = board,
    skip_existing = skip,
    force = force,
    show_progress = FALSE
  ))
}

cat("== Initial run ==\n")
run_once()
ts1_meta <- get_ts_meta()
ts1_obj  <- get_ts_obj()
cat("pin_meta timestamp:", ts_to_char(ts1_meta), "\n")
cat("pin_read meta timestamp:", ts_to_char(ts1_obj), "\n\n")

Sys.sleep(1.2)
cat("== Re-run with skip_existing = FALSE ==\n")
run_once(skip = FALSE)
ts2_meta <- get_ts_meta()
ts2_obj  <- get_ts_obj()
cat("pin_meta timestamp:", ts_to_char(ts2_meta), "\n")
cat("pin_read meta timestamp:", ts_to_char(ts2_obj), "\n\n")

Sys.sleep(1.2)
cat("== Re-run with force = TRUE ==\n")
run_once(force = TRUE)
ts3_meta <- get_ts_meta()
ts3_obj  <- get_ts_obj()
cat("pin_meta timestamp:", ts_to_char(ts3_meta), "\n")
cat("pin_read meta timestamp:", ts_to_char(ts3_obj), "\n\n")

cat("Summary (meta):\n")
cat("t1:", ts_to_char(ts1_meta), "\n")
cat("t2:", ts_to_char(ts2_meta), "\n")
cat("t3:", ts_to_char(ts3_meta), "\n")
