remove.packages("GenGC")
unlink(file.path(.libPaths()[1], "GenGC"), recursive = TRUE, force = TRUE)
devtools::install_local("C:/Users/maxth/Documents/GitHub/GenGC", force=TRUE)

# in a fresh R session
remove.packages("CausalStress")
unlink(file.path(.libPaths()[1], "CausalStress"), recursive = TRUE, force = TRUE)

#install.packages("grf")

# reinstall
devtools::install(upgrade = "never")


library(CausalStress)
library(pins)
library(future)
library(dplyr)

# Pick estimators you want; keep only those that are registered/installed
candidates <- c("lm_att", "ipw_att", "grf_dr", "gengc", "gengc_dr")
estimator_ids <- candidates[sapply(candidates, function(id) {
  tryCatch({ cs_get_estimator(id); TRUE }, error = function(...) FALSE)
})]
message("Estimators included: ", paste(estimator_ids, collapse = ", "))

# Infra (absolute paths recommended on Windows)
board_path  <- normalizePath("_experiments/pins_board_multi", mustWork = FALSE)
staging_dir <- normalizePath("_experiments/staging_multi", mustWork = FALSE)
dir.create(board_path,  recursive = TRUE, showWarnings = FALSE)
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
board <- board_folder(board_path, versioned = TRUE)

# Parallel backend
plan(multisession, workers = 20)
on.exit(plan(sequential), add = TRUE)

library(CausalStress)

suite_ids <- cs_suite_registry()$suite_id
dgp_ids   <- unique(unlist(lapply(suite_ids, cs_get_suite), use.names = FALSE))

results <- cs_run_campaign(
  dgp_ids       = dgp_ids,
  estimator_ids = estimator_ids,
  seeds         = 1:200,
  n             = 5000,
  bootstrap     = FALSE,  # B ignored when bootstrap = FALSE
  B             = 0,
  board         = board,
  staging_dir   = staging_dir,
  parallel      = TRUE,
  show_progress = TRUE,
  skip_existing = FALSE
)


# Tidy and optional gatekeeper summary
tidy_res <- cs_tidy(results)
tidy_res %>% 
  select(dgp_id, estimator_id, seed, est_att, att_error) %>% 
  arrange(dgp_id, att_error) %>% 
  print(n  = 1000)


tidy_res <- cs_tidy(results)

summary_mc <- tidy_res %>%
  group_by(dgp_id, estimator_id) %>%
  summarise(
    n_seeds   = n(),
    mean_att  = mean(est_att),
    mean_bias = mean(att_error),
    rmse      = sqrt(mean(att_error^2)),
    sd_error  = sd(att_error),
    .groups   = "drop"
) %>%
  arrange(dgp_id, abs(mean_bias))

summary_mc %>%
  arrange(dgp_id, rmse) %>%
  print(n = 1000)


tidy_res %>% 
  filter(estimator_id == "gengc") %>%
  cs_collect_qst() %>%
  group_by(dgp_id, tau) %>%
  summarise(
    n_seeds   = n(),
    mean_att  = mean(estimate),
    mean_bias = mean(abs_error),
    rmse      = sqrt(mean(abs_error^2)),
    sd_error  = sd(abs_error),
    .groups   = "drop"
) %>%
  print(n = 1000)



library(dplyr)

qst_summary <- tidy_res %>%
  filter(estimator_id == "gengc") %>%
  cs_collect_qst() %>%
  group_by(dgp_id, estimator_id, n, tau) %>%
  summarise(
    mean_bias = mean(estimate - true, na.rm = TRUE),
    rmse      = sqrt(mean((estimate - true)^2, na.rm = TRUE)),
    mae       = mean(abs_error, na.rm = TRUE),
    n_seeds   = n(),
    .groups   = "drop"
  ) 

library(ggplot2)
library(dplyr)

# ensure estimator_id is a factor for consistent ordering
qst_summary <- qst_summary %>%
  mutate(estimator_id = factor(estimator_id))

ggplot(qst_summary, aes(x = tau, y = rmse, fill = estimator_id)) +
  geom_col(position = "dodge", width = 0.08, alpha = 0.9) +
  facet_wrap(vars(dgp_id), ncol = 2) +
  scale_fill_brewer(palette = "Dark2", name = "Estimator") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "QST RMSE by Quantile",
    subtitle = "Per DGP, across 200 seeds",
    x = "Quantile (tau)",
    y = "RMSE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Assuming you have the summary object from the previous step
ggplot(qst_summary, aes(x = tau, y = mean_bias)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(color = "red") +
  facet_wrap(~dgp_id) +
  theme_minimal() +
  labs(title = "Bias over Quantiles")

tidy_res <- cs_tidy(results)

# QST summary over seeds per DGP/estimator/tau
qst_summary <- cs_summarise_qst(tidy_res)
print(qst_summary)



library(ggplot2)

# Assuming 'qst_summary' is already created from the previous step
ggplot(qst_summary, aes(x = tau, y = mean_bias)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # The Bias Line (Red)
  geom_line(color = "#D55E00", linewidth = 1) +
  
  # Optional: Add the MAE ribbon to show the "Variance Envelope" around the bias
  # If the ribbon is wide but the line is flat, it's pure variance.
  geom_ribbon(aes(ymin = mean_bias - mae, ymax = mean_bias + mae), 
              alpha = 0.2, fill = "#D55E00") +
  
  facet_wrap(~dgp_id, scales = "free_y") +
  labs(
    title = "QST Bias Decomposition",
    subtitle = "Red Line = Systematic Bias | Shaded Band = Mean Absolute Error (Variance Proxy)",
    x = "Quantile (tau)",
    y = "Error"
  ) +
  theme_minimal()


# Check all heavy-tail conditions
qst_summary %>%
  filter(grepl("heavytail", dgp_id)) %>%
  filter(tau == 0.5) %>%
  select(dgp_id, mean_bias, rmse, mae)

summary_mc %>%
  filter(grepl("heavytail", dgp_id)) 



# ============================================================================
# IQR Stability Analysis
# ============================================================================

library(dplyr)
library(ggplot2)

# ----------------------------------------------------------------------------
# 1. Check IQR (25th, 50th, 75th percentiles) across all DGPs
# ----------------------------------------------------------------------------

iqr_stability <- qst_summary %>%
  filter(tau %in% c(0.25, 0.5, 0.75)) %>%
  select(dgp_id, tau, mean_bias, rmse, mae, n_seeds) %>%
  arrange(dgp_id, tau)

print("=== IQR Stability Across All DGPs ===")
print(iqr_stability, n = 100)

# Summary: Average RMSE by quantile position
iqr_summary <- iqr_stability %>%
  group_by(tau) %>%
  summarise(
    mean_rmse = mean(rmse),
    median_rmse = median(rmse),
    max_rmse = max(rmse),
    n_dgps = n()
  )

print("=== Average RMSE by Quantile (Across All DGPs) ===")
print(iqr_summary)

# ----------------------------------------------------------------------------
# 2. Heavy-tail specific: Full quantile range
# ----------------------------------------------------------------------------

heavytail_quantiles <- qst_summary %>%
  filter(dgp_id == "synth_heavytail") %>%
  select(tau, mean_bias, rmse, mae) %>%
  arrange(tau)

print("=== Heavy-Tail: RMSE Across All Quantiles ===")
print(heavytail_quantiles, n = 50)

# ----------------------------------------------------------------------------
# 3. Compare IQR vs Tails for heavy-tail DGPs
# ----------------------------------------------------------------------------

heavytail_comparison <- qst_summary %>%
  filter(grepl("heavytail", dgp_id)) %>%
  mutate(
    quantile_region = case_when(
      tau < 0.25 ~ "Lower Tail (0-25%)",
      tau >= 0.25 & tau <= 0.75 ~ "IQR (25-75%)",
      tau > 0.75 ~ "Upper Tail (75-100%)"
    )
  ) %>%
  group_by(dgp_id, quantile_region) %>%
  summarise(
    mean_rmse = mean(rmse),
    median_rmse = median(rmse),
    max_rmse = max(rmse),
    mean_bias = mean(abs(mean_bias)),
    .groups = "drop"
  )

print("=== Heavy-Tail: IQR vs Tail Performance ===")
print(heavytail_comparison)

# ----------------------------------------------------------------------------
# 4. Visualization: RMSE by quantile distance from median
# ----------------------------------------------------------------------------

# Plot 1: RMSE vs distance from median (heavy-tail)
p1 <- qst_summary %>%
  filter(dgp_id == "synth_heavytail") %>%
  mutate(distance_from_median = abs(tau - 0.5)) %>%
  ggplot(aes(x = distance_from_median, y = rmse)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue", alpha = 0.5) +
  scale_y_log10() +
  labs(
    title = "QST RMSE Increases Away from Median",
    subtitle = "Heavy-tailed noise (Cauchy): Central quantiles stable, tails degrade",
    x = "Distance from Median (|τ - 0.5|)",
    y = "RMSE (log scale)",
    caption = "Blue line: RMSE = 1 threshold"
  ) +
  theme_minimal()

print(p1)

# Plot 2: IQR RMSE across all DGPs
p2 <- qst_summary %>%
  filter(tau %in% c(0.25, 0.5, 0.75)) %>%
  mutate(quantile_label = factor(tau, 
                                  levels = c(0.25, 0.5, 0.75),
                                  labels = c("25th", "Median", "75th"))) %>%
  ggplot(aes(x = dgp_id, y = rmse, fill = quantile_label)) +
  geom_col(position = "dodge") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "IQR Stability Across All DGPs",
    subtitle = "25th, 50th, 75th percentile RMSE (log scale)",
    x = "DGP",
    y = "RMSE (log scale)",
    fill = "Quantile"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# Plot 3: Heatmap of RMSE across quantiles and DGPs
p3 <- qst_summary %>%
  ggplot(aes(x = tau, y = dgp_id, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "white", 
    mid = "orange", 
    high = "red",
    midpoint = 0.5,
    trans = "log10",
    name = "RMSE\n(log scale)"
  ) +
  labs(
    title = "QST RMSE Heatmap",
    subtitle = "Stability across quantiles (x-axis) and DGPs (y-axis)",
    x = "Quantile (τ)",
    y = "DGP"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

print(p3)

# ----------------------------------------------------------------------------
# 5. Key metrics table for paper
# ----------------------------------------------------------------------------

paper_table <- qst_summary %>%
  filter(tau %in% c(0.25, 0.5, 0.75)) %>%
  group_by(tau) %>%
  summarise(
    `Mean RMSE` = sprintf("%.3f", mean(rmse)),
    `Median RMSE` = sprintf("%.3f", median(rmse)),
    `Max RMSE` = sprintf("%.3f", max(rmse)),
    `% DGPs RMSE < 0.1` = sprintf("%.1f%%", 100 * mean(rmse < 0.1)),
    `% DGPs RMSE < 0.5` = sprintf("%.1f%%", 100 * mean(rmse < 0.5))
  ) %>%
  mutate(Quantile = case_when(
    tau == 0.25 ~ "25th (Q1)",
    tau == 0.5 ~ "Median (Q2)",
    tau == 0.75 ~ "75th (Q3)"
  )) %>%
  select(Quantile, everything(), -tau)

print("=== Table: IQR Performance Summary (For Paper) ===")
print(paper_table)

# ----------------------------------------------------------------------------
# 6. Specific comparison: Heavy-tail ATT vs IQR
# ----------------------------------------------------------------------------

heavytail_att_vs_iqr <- list(
  ATT = summary_mc %>%
    filter(dgp_id == "synth_heavytail") %>%
    select(estimator_id, att_rmse = rmse),
  
  IQR = qst_summary %>%
    filter(dgp_id == "synth_heavytail", tau %in% c(0.25, 0.5, 0.75)) %>%
    select(tau, qst_rmse = rmse) %>%
    mutate(quantile = case_when(
      tau == 0.25 ~ "Q1 (25th)",
      tau == 0.5 ~ "Median",
      tau == 0.75 ~ "Q3 (75th)"
    ))
)

print("=== Heavy-Tail: ATT vs IQR Comparison ===")
print("ATT RMSE (all methods fail):")
print(heavytail_att_vs_iqr$ATT)
print("\nGenGC IQR RMSE (stable):")
print(heavytail_att_vs_iqr$IQR)

# Calculate improvement ratio
att_worst <- max(heavytail_att_vs_iqr$ATT$att_rmse)
iqr_best <- min(heavytail_att_vs_iqr$IQR$qst_rmse)
improvement <- att_worst / iqr_best

cat(sprintf("\n=== Improvement Factor ===\n"))
cat(sprintf("Worst ATT RMSE: %.1f\n", att_worst))
cat(sprintf("Best IQR RMSE: %.3f\n", iqr_best))
cat(sprintf("Improvement: %.0f× more stable\n", improvement))

# ----------------------------------------------------------------------------
# 7. Export for paper
# ----------------------------------------------------------------------------

# Save the key table
write.csv(paper_table, "_experiments/iqr_stability_table.csv", row.names = FALSE)

# Save the plots
ggsave("_experiments/iqr_distance_plot.png", p1, width = 8, height = 6, dpi = 300)
ggsave("_experiments/iqr_by_dgp_plot.png", p2, width = 10, height = 8, dpi = 300)
ggsave("_experiments/iqr_heatmap.png", p3, width = 10, height = 8, dpi = 300)

cat("\n=== Saved outputs ===\n")
cat("Table: _experiments/iqr_stability_table.csv\n")
cat("Plots: _experiments/iqr_*.png\n")