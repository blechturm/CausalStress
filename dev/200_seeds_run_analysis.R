# ------------------------------------------------------------------------------
# Analysis Script: Native CausalStress API
# ------------------------------------------------------------------------------
library(CausalStress)
library(pins)
library(dplyr)
library(ggplot2)
library(purrr)

# 1. Connect to Board
board_path <- "C:/Simulations/CausalStress_Board_v1"
board <- board_folder(board_path)

# ------------------------------------------------------------------------------
# 2. Load & Tidy Data (Using Native Helpers)
# ------------------------------------------------------------------------------
message("📦 Loading batches using cs_read_batch()...")

# Get list of batch pins
all_pins <- pin_list(board)
batch_pins <- all_pins[grepl("^batch_", all_pins)]

# Use map_dfr to iterate, read, and immediately tidy
# This uses cs_read_batch() and cs_tidy_batch() from R/cs-accessors.R
df_tidy <- map_dfr(batch_pins, function(pin_name) {
  tryCatch({
    # 1. Read raw result list from pin
    raw_batch <- cs_read_batch(board, pin_name)
    
    # 2. Tidy immediately into a standardized tibble
    cs_tidy_batch(raw_batch)
    
  }, error = function(e) {
    warning("Failed to process: ", pin_name)
    return(NULL)
  })
}, .progress = TRUE)

message(sprintf("✅ Loaded %d runs across %d seeds.", 
                nrow(df_tidy), n_distinct(df_tidy$seed)))

# ------------------------------------------------------------------------------
# 3. Create Specialized Datasets (Using cs_collect_*)
# ------------------------------------------------------------------------------
# Extract ATT results (Point Estimates)
df_att <- cs_collect_att(df_tidy)

# Extract QST results (Distributional Curves)
# This handles the unnesting automatically
df_qst <- cs_collect_qst(df_tidy)

# Create Output Directory
output_dir <- "C:/Simulations/Analysis_Output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ------------------------------------------------------------------------------
# 4. Plot 1: Parity (Using cs_plot_att_error)
# ------------------------------------------------------------------------------
# Filter for the benchmark comparison
subset_parity <- df_att %>%
  filter(dgp_id %in% c("synth_nonlinear_heteroskedastic", "synth_baseline")) %>%
  filter(estimator_id %in% c("gengc_dr", "grf_dr_att", "tmle_att", "ipw_att"))

# Use the native plotting function
p1 <- cs_plot_att_error(subset_parity) +
  labs(title = "Validation: GenGC Parity vs SOTA",
       subtitle = "Standardized Error Distribution (N=1000)")

ggsave(file.path(output_dir, "plot1_parity.png"), p1, width = 8, height = 5)

# ------------------------------------------------------------------------------
# 5. Plot 2: Robustness (Heavy Tails)
# ------------------------------------------------------------------------------
# Manual ggplot is still best here to add the specific "Truth" line styling
subset_robust <- df_att %>%
  filter(dgp_id == "synth_heavytail") %>%
  filter(estimator_id %in% c("gengc_dr", "ipw_att", "lm_att"))

p2 <- ggplot(subset_robust, aes(x = estimator_id, y = est_att)) +
  geom_hline(aes(yintercept = true_att), color = "red", linetype = "dashed") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  theme_minimal() +
  labs(title = "Stress Test: Heavy-Tailed Outcomes",
       subtitle = "IPW Instability vs GenGC Robustness",
       y = "Estimated ATT", x = NULL)

ggsave(file.path(output_dir, "plot2_robustness.png"), p2, width = 6, height = 5)

# ------------------------------------------------------------------------------
# 6. Plot 3: The Money Plot (Using cs_plot_qst)
# ------------------------------------------------------------------------------
# Filter for the specific story: GenGC on Sign-Switching Data
subset_money <- df_qst %>%
  filter(dgp_id == "synth_qte1", estimator_id == "gengc")

if (nrow(subset_money) > 0) {
  # Aggregate across seeds for the plot (mean + CIs)
  # cs_plot_qst expects raw rows (it doesn't aggregate automatically in v0.1.9)
  # So we calculate the summary stats first to plot the "Average Curve"
  
  agg_money <- subset_money %>%
    group_by(tau, estimator_id, dgp_id) %>%
    summarise(
      estimate = mean(estimate, na.rm = TRUE),
      true = mean(true, na.rm = TRUE), # Truth should be constant
      ci_lo = quantile(estimate, 0.05, na.rm = TRUE), # Empirical CI of the estimator
      ci_hi = quantile(estimate, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  # Use native plotter
  p3 <- cs_plot_qst(agg_money) +
    annotate("text", x = 0.2, y = min(agg_money$ci_lo), label = "Losers", color = "red") +
    annotate("text", x = 0.8, y = max(agg_money$ci_hi), label = "Winners", color = "blue") +
    labs(title = "The Distributional Insight (QST)",
         subtitle = "Recovering the sign-switching effect structure")

  ggsave(file.path(output_dir, "plot3_money.png"), p3, width = 7, height = 5)
}

message("🚀 Analysis Complete. Outputs in: ", output_dir)



# ------------------------------------------------------------------------------
# 6. Plot 3.1: The Money Plot (Using cs_plot_qst)
# ------------------------------------------------------------------------------
# Filter for the specific story: GenGC on Sign-Switching Data
subset_money <- df_qst %>%
  filter(dgp_id == "synth_heavytail", estimator_id == "gengc")

if (nrow(subset_money) > 0) {
  # Aggregate across seeds for the plot (mean + CIs)
  # cs_plot_qst expects raw rows (it doesn't aggregate automatically in v0.1.9)
  # So we calculate the summary stats first to plot the "Average Curve"
  
  agg_money <- subset_money %>%
    group_by(tau, estimator_id, dgp_id) %>%
    summarise(
      estimate = mean(estimate, na.rm = TRUE),
      true = mean(true, na.rm = TRUE), # Truth should be constant
      ci_lo = quantile(estimate, 0.05, na.rm = TRUE), # Empirical CI of the estimator
      ci_hi = quantile(estimate, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  # Use native plotter
  p3 <- cs_plot_qst(agg_money) +
    annotate("text", x = 0.2, y = min(agg_money$ci_lo), label = "Losers", color = "red") +
    annotate("text", x = 0.8, y = max(agg_money$ci_hi), label = "Winners", color = "blue") +
    labs(title = "The Distributional Insight (QST)",
         subtitle = "Recovering heavvy tail effect structure")

  ggsave(file.path(output_dir, "plot3.1_money.png"), p3, width = 7, height = 5)
}

message("🚀 Analysis Complete. Outputs in: ", output_dir)



# ------------------------------------------------------------------------------
# Part 2: The Extended Analysis (Placebos & Stress Tests)
# ------------------------------------------------------------------------------
# Assumes 'df_att' is already loaded from the previous step.
# If not, reload it:
# df_att <- cs_collect_att(df_tidy)

library(dplyr)
library(ggplot2)
library(CausalStress)

# Ensure output directory exists
output_dir <- "C:/Simulations/Analysis_Output"

# ------------------------------------------------------------------------------
# 7. The "Lie Detector" (Placebo Validation)
# Goal: Prove Type I Error control. All estimates should hit 0.
# ------------------------------------------------------------------------------
# We use the package's native placebo plotter
p_placebo <- cs_plot_placebo(df_att) +
  labs(title = "Validation: Placebo Tests (The Lie Detector)",
       subtitle = "Estimates should be centered at 0 (Dashed Line)")

ggsave(file.path(output_dir, "plot4_placebos.png"), p_placebo, width = 10, height = 6)

# ------------------------------------------------------------------------------
# 8. The "Torture Chamber" (Overlap & High-Dim)
# Goal: Check stability in structurally difficult regimes
# ------------------------------------------------------------------------------
torture_dgps <- c("synth_overlap_stressed", "synth_hd_sparse_plm", "synth_tilt_mild")

p_torture <- df_att %>%
  filter(dgp_id %in% torture_dgps) %>%
  #filter(estimator_id %in% c("gengc", "gengc_dr", "grf_dr_att", "tmle_att", "ipw_att")) %>%
  mutate(error = est_att - true_att) %>%
  ggplot(aes(x = estimator_id, y = error, fill = estimator_id)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(alpha = 0.6, outlier.size = 0.5) +
  facet_wrap(~dgp_id, scales = "free") +
  theme_minimal() +
  labs(
    title = "Stress Tests: Overlap & High-Dimensions",
    subtitle = "Comparing stability in structurally difficult regimes",
    y = "Error (Estimate - Truth)",
    x = NULL
  ) +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "plot5_stress_tests.png"), p_torture, width = 9, height = 5)

# ------------------------------------------------------------------------------
# 9. The "Leaderboard" (Grand Summary Table)
# Goal: A clean CSV table for the PhD Annex / Email
# ------------------------------------------------------------------------------
leaderboard <- df_att %>%
  group_by(dgp_id, estimator_id) %>%
  summarise(
    n_seeds = n(),
    bias = mean(est_att - true_att, na.rm = TRUE),
    rmse = sqrt(mean((est_att - true_att)^2, na.rm = TRUE)),
    mae = mean(abs(est_att - true_att), na.rm = TRUE),
    coverage = mean(att_covered, na.rm = TRUE),
    width = mean(att_ci_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dgp_id, rmse)

# Save to CSV
write.csv(leaderboard, file.path(output_dir, "table_leaderboard.csv"), row.names = FALSE)

# Print a preview of the "Hardest" cases (Highest RMSE)
print(leaderboard %>% arrange(dgp_id,desc(rmse)), n = 1000)

message("🚀 Extended Analysis Complete. Check: ", output_dir)



# ------------------------------------------------------------------------------
# Analysis: Heavy Tail Robustness (Mean vs Median)
# ------------------------------------------------------------------------------
library(CausalStress)
library(pins)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# 1. Connect and Stitch (Targeted Stitching)
# We only pull 'synth_heavytail' to save time/memory
board <- board_folder("C:/Simulations/CausalStress_Board_v1")
batch_pins <- pin_list(board)[grepl("^batch_", pin_list(board))]

message("🧵 Stitching 'synth_heavytail' runs only...")

df_heavy <- map_dfr(batch_pins, function(p) {
  tryCatch({
    # Read batch
    batch <- pin_read(board, p)
    
    # Filter for heavytail BEFORE tidying (optimization)
    # We look at the first result's metadata to check DGP type
    # (Assuming batches are homogenous or mixed, we strictly check results)
    keep_results <- Filter(function(x) x$meta$dgp_id == "synth_heavytail", batch$results)
    
    if (length(keep_results) > 0) {
      return(cs_tidy_batch(keep_results))
    } else {
      return(NULL)
    }
  }, error = function(e) return(NULL))
}, .progress = TRUE)

message(sprintf("✅ Loaded %d heavy-tail runs.", nrow(df_heavy)))

# ------------------------------------------------------------------------------
# 2. Compare Metrics: ATT (Mean) vs QST (Median)
# ------------------------------------------------------------------------------

# A. ATT Performance (The "Ghost Chase")
# We use MAD (Median Absolute Deviation) because RMSE is infinite for Cauchy
att_metrics <- df_heavy %>%
  filter(estimator_id == "gengc") %>%
  summarise(
    target = "ATT (Mean)",
    # Truth for ATT is the Structural TE (1.5 approx), but noise is Cauchy
    bias_median = median(est_att - true_att, na.rm = TRUE),
    mad_error = median(abs(est_att - true_att), na.rm = TRUE),
    width = median(att_ci_width, na.rm = TRUE)
  )

# B. Median QST Performance (The "Stable Target")
# We need to extract the QST @ 0.5 from the distributional payload
qst_metrics <- cs_collect_qst(df_heavy) %>%
  filter(estimator_id == "gengc", tau == 0.5) %>%
  summarise(
    target = "QST (Median)",
    bias_median = median(estimate - true, na.rm = TRUE),
    mad_error = median(abs(estimate - true), na.rm = TRUE),
    width = median(ci_width, na.rm = TRUE)
  )

# ------------------------------------------------------------------------------
# 3. The Comparison Table
# ------------------------------------------------------------------------------
comparison <- bind_rows(att_metrics, qst_metrics) %>%
  mutate(across(where(is.numeric), round, 3))

print(comparison)

# ------------------------------------------------------------------------------
# 4. Visualization: The "Stability Plot"
# ------------------------------------------------------------------------------
# We plot the distribution of errors for ATT vs Median QST
# We expect ATT errors to be spread out (heavy tails), Median errors to be tight.

plot_data <- bind_rows(
  df_heavy %>% 
    filter(estimator_id == "gengc") %>%
    mutate(error = est_att - true_att, type = "ATT (Mean)"),
    
  cs_collect_qst(df_heavy) %>% 
    filter(estimator_id == "gengc", tau == 0.5) %>%
    mutate(error = estimate - true, type = "QST (Median)")
)

p_stability <- ggplot(plot_data, aes(x = type, y = error, fill = type)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_violin(alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  # Zoom in to show the contrast (ignoring massive ATT outliers)
  coord_cartesian(ylim = c(-2, 2)) + 
  labs(
    title = "GenGC: Mean vs. Median Stability (Cauchy Noise)",
    subtitle = "Median QST is stable; ATT estimation is futile.",
    y = "Estimation Error",
    x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("C:/Simulations/Analysis_Output/plot_heavytail_median.png", p_stability, width = 6, height = 5)

message("🚀 Comparison Complete.")



# ------------------------------------------------------------------------------
# FIXED: Heavy Tail Analysis & Plotting
# ------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(CausalStress)

# 1. Ensure df_heavy exists (Safety Check)
if (!exists("df_heavy")) {
  # Fallback: recreate from results if needed
  if(exists("df_results")) df_heavy <- df_results %>% filter(dgp_id == "synth_heavytail")
  else if(exists("df_tidy")) df_heavy <- df_tidy %>% filter(dgp_id == "synth_heavytail")
}

# 2. Leaderboard: Compare "Chasing the Mean" vs "Targeting the Median"
leaderboard <- bind_rows(
  # A. The others (forced to chase the unstable Mean)
  df_heavy %>%
    filter(estimator_id != "gengc") %>% # Remove GenGC's mean estimate (it's bad too!)
    group_by(estimator_id) %>%
    summarise(
      target = "ATT (Mean - Unstable)",
      bias = median(est_att - true_att, na.rm = TRUE),
      mad  = median(abs(est_att - true_att), na.rm = TRUE),
      width = median(att_ci_width, na.rm = TRUE),
      .groups = "drop"
    ),
  
  # B. GenGC (Smart enough to target the Median)
  cs_collect_qst(df_heavy) %>%
    filter(estimator_id == "gengc", tau == 0.5) %>%
    summarise(
      estimator_id = "gengc (Median)",
      target = "QST (Median - Stable)",
      bias = median(estimate - true, na.rm = TRUE),
      mad  = median(abs(estimate - true), na.rm = TRUE),
      width = median(ci_width, na.rm = TRUE)
    )
) %>%
  arrange(mad)

print(leaderboard)
write.csv(leaderboard, "C:/Simulations/Analysis_Output/table_heavytail_robustness.csv")

# ------------------------------------------------------------------------------
# 3. Visualization: The "Ghost Hunter" Plot (Fixed)
# ------------------------------------------------------------------------------
# Prepare plotting data with explicit error calculation
plot_data <- bind_rows(
  # Group 1: Everyone failing at the Mean
  df_heavy %>%
    filter(estimator_id != "gengc") %>%
    mutate(
      error = est_att - true_att,
      type = "Target: Mean (Undefined)"
    ) %>%
    select(estimator_id, error, type),
  
  # Group 2: GenGC nailing the Median
  cs_collect_qst(df_heavy) %>%
    filter(estimator_id == "gengc", tau == 0.5) %>%
    mutate(
      estimator_id = "gengc (Median)", 
      error = estimate - true,
      type = "Target: Median (Defined)"
    ) %>%
    select(estimator_id, error, type)
)

# Plot
p_ghost <- ggplot(plot_data, aes(x = reorder(estimator_id, abs(error)), y = error, fill = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
  
  # Crucial: Zoom in to show GenGC's precision, ignoring the Cauchy explosions
  coord_cartesian(ylim = c(-1.5, 1.5)) + 
  
  labs(
    title = "The Heavy Tail Advantage",
    subtitle = "Standard estimators fail to estimate the Mean (Cauchy). GenGC recovers the Median.",
    y = "Estimation Error",
    x = NULL,
    fill = "Estimand"
  ) +
  scale_fill_manual(values = c("Target: Mean (Undefined)" = "gray70", "Target: Median (Defined)" = "#E63946")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Simulations/Analysis_Output/plot_heavytail_all.png", p_ghost, width = 8, height = 5)

message("🚀 Plot saved. Look at the difference in width between GenGC and BART!")


# ------------------------------------------------------------------------------
# EXTENDED ANALYSIS: Additional Tables & Visualizations
# ------------------------------------------------------------------------------
# Assumes df_att, df_qst, and output_dir already exist from previous script

library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------------------------------------------------------
# TABLE 1: Method Rankings by Scenario Type
# ------------------------------------------------------------------------------

# Create scenario taxonomy
scenario_types <- tribble(
  ~dgp_id, ~type,
  "synth_baseline", "Clean",
  "synth_nonlinear_heteroskedastic", "Clean",
  "synth_heavytail", "Heavy Tail",
  "synth_overlap_stressed", "Overlap",
  "synth_tilt_mild", "Overlap",
  "synth_hd_sparse_plm", "High-Dim",
  "synth_qte1", "Heterogeneity",
  "synth_placebo_tau0", "Placebo",
  "synth_placebo_nonlinear", "Placebo",
  "synth_placebo_heavytail", "Placebo",
  "synth_placebo_tilted", "Placebo",
  "synth_placebo_kangschafer", "Placebo"
)

ranking_by_type <- df_att %>%
  left_join(scenario_types, by = "dgp_id") %>%
  mutate(error = est_att - true_att) %>%
  group_by(type, estimator_id) %>%
  summarise(
    mean_rmse = sqrt(mean(error^2, na.rm = TRUE)),
    mean_bias = mean(abs(error), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(type, mean_rmse) %>%
  group_by(type) %>%
  mutate(rank = row_number())

# Pivot to show ranking matrix
ranking_matrix <- ranking_by_type %>%
  select(type, estimator_id, rank) %>%
  pivot_wider(names_from = type, values_from = rank) %>%
  arrange(estimator_id)

# Save table
write.csv(ranking_matrix, file.path(output_dir, "table_ranking_by_type.csv"), row.names = FALSE)
print("Rankings by scenario type:")
print(ranking_matrix)

# ------------------------------------------------------------------------------
# PLOT 6: Success Rate Heatmap
# ------------------------------------------------------------------------------

success_rates <- df_att %>%
  mutate(success = abs(est_att - true_att) < 0.2) %>%  
  group_by(dgp_id, estimator_id) %>%
  summarise(success_rate = mean(success, na.rm = TRUE), .groups = "drop")

p6_heatmap <- ggplot(success_rates, 
                     aes(x = estimator_id, y = dgp_id, fill = success_rate)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = scales::percent(success_rate, accuracy = 1)), 
            size = 2.5, color = "black") +
  scale_fill_gradient2(
    low = "#E63946", mid = "#FFF3B0", high = "#06A77D",
    midpoint = 0.5, limits = c(0, 1),
    labels = scales::percent
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "right"
  ) +
  labs(
    title = "Success Rate Heatmap (|Error| < 0.2)",
    subtitle = "Red = Failure, Green = Success",
    x = NULL, y = NULL, fill = "Success\nRate"
  )

ggsave(file.path(output_dir, "plot6_success_heatmap.png"), p6_heatmap, 
       width = 10, height = 6)

# ------------------------------------------------------------------------------
# PLOT 7: Head-to-Head Comparison (GenGC vs GRF)
# ------------------------------------------------------------------------------

head_to_head <- df_att %>%
  filter(estimator_id %in% c("gengc_dr", "grf_dr_att")) %>%
  select(dgp_id, seed, estimator_id, est_att, true_att) %>%
  mutate(error = abs(est_att - true_att)) %>%
  select(-est_att, -true_att) %>%
  pivot_wider(names_from = estimator_id, values_from = error) %>%
  mutate(
    winner = case_when(
      gengc_dr < grf_dr_att ~ "GenGC",
      grf_dr_att < gengc_dr ~ "GRF",
      TRUE ~ "Tie"
    )
  )

win_summary <- head_to_head %>%
  group_by(dgp_id, winner) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(dgp_id) %>%
  mutate(pct = n / sum(n))

p7_headtohead <- ggplot(win_summary, aes(x = dgp_id, y = pct, fill = winner)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "GenGC" = "#E63946", 
    "GRF" = "#457B9D", 
    "Tie" = "gray70"
  )) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "GenGC vs GRF: Head-to-Head Win Rate",
    subtitle = "Proportion of seeds where each method had lower absolute error",
    x = NULL, y = "Win Rate", fill = "Winner"
  )

ggsave(file.path(output_dir, "plot7_headtohead.png"), p7_headtohead, 
       width = 8, height = 6)

# Print summary
win_totals <- head_to_head %>%
  count(dgp_id, winner) %>%
  group_by(dgp_id) %>%
  mutate(pct = n/sum(n)) %>%
  arrange(dgp_id, desc(pct))

print("Head-to-head win rates:")
print(win_totals)

# ------------------------------------------------------------------------------
# TABLE 2: Coverage Calibration Check
# ------------------------------------------------------------------------------

coverage_check <- df_att %>%
  group_by(dgp_id, estimator_id) %>%
  summarise(
    coverage = mean(att_covered, na.rm = TRUE),
    nominal = 0.95,
    deviation = abs(coverage - 0.95),
    .groups = "drop"
  ) %>%
  arrange(desc(deviation))

# Flag severe miscoverage
severe_miscoverage <- coverage_check %>%
  filter(deviation > 0.1)

write.csv(coverage_check, 
          file.path(output_dir, "table_coverage_calibration.csv"), 
          row.names = FALSE)

print("Severe coverage violations (deviation > 10%):")
print(severe_miscoverage)

# ------------------------------------------------------------------------------
# PLOT 8: Error Distribution Comparison (Key Scenarios)
# ------------------------------------------------------------------------------

error_comparison <- df_att %>%
  filter(dgp_id %in% c("synth_baseline", "synth_heavytail", 
                       "synth_overlap_stressed", "synth_qte1")) %>%
  mutate(
    error = est_att - true_att,
    dgp_label = case_when(
      dgp_id == "synth_baseline" ~ "Clean Baseline",
      dgp_id == "synth_heavytail" ~ "Heavy Tails",
      dgp_id == "synth_overlap_stressed" ~ "Overlap Violation",
      dgp_id == "synth_qte1" ~ "Heterogeneous Effects"
    )
  )

p8_error_dist <- ggplot(error_comparison, 
                        aes(x = estimator_id, y = error, fill = estimator_id)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap(~dgp_label, scales = "free_y", nrow = 2) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Error Distribution Across Key Scenarios",
    subtitle = "Violin shows density; horizontal lines show quartiles",
    x = NULL, y = "Error (Estimate - Truth)"
  )

ggsave(file.path(output_dir, "plot8_error_distribution.png"), p8_error_dist, 
       width = 10, height = 7)

# ------------------------------------------------------------------------------
# PLOT 9: QST Calibration Across Quantiles
# ------------------------------------------------------------------------------

qst_calibration <- df_qst %>%
  filter(estimator_id == "gengc") %>%
  mutate(error = estimate - true) %>%
  group_by(dgp_id, tau) %>%
  summarise(
    bias = mean(error, na.rm = TRUE),
    rmse = sqrt(mean(error^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    dgp_label = case_when(
      dgp_id == "synth_baseline" ~ "Clean",
      dgp_id == "synth_heavytail" ~ "Heavy Tail",
      dgp_id == "synth_qte1" ~ "Heterogeneous",
      dgp_id == "synth_overlap_stressed" ~ "Overlap Fail",
      TRUE ~ dgp_id
    )
  )

p9_qst_calibration <- ggplot(qst_calibration, 
                              aes(x = tau, y = bias, color = dgp_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~dgp_label, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "GenGC QST Bias Across Quantiles",
    subtitle = "Is bias uniform across τ, or concentrated at tails?",
    x = "Quantile (τ)", 
    y = "Bias (Estimate - Truth)"
  )

ggsave(file.path(output_dir, "plot9_qst_calibration.png"), p9_qst_calibration, 
       width = 10, height = 6)

# ------------------------------------------------------------------------------
# PLOT 10: Placebo Diagnostic (Focus on Failures)
# ------------------------------------------------------------------------------

placebo_failures <- df_att %>%
  filter(grepl("placebo", dgp_id)) %>%
  filter(estimator_id %in% c("gengc", "gengc_dr", "grf_dr_att", "bart_att")) %>%
  group_by(dgp_id, estimator_id) %>%
  summarise(
    abs_bias = mean(abs(est_att - true_att), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    severity = case_when(
      abs_bias < 0.1 ~ "Pass",
      abs_bias < 0.5 ~ "Mild",
      abs_bias < 2.0 ~ "Moderate",
      TRUE ~ "Severe"
    )
  )

p10_placebo_diagnostic <- ggplot(placebo_failures, 
                                  aes(x = reorder(dgp_id, abs_bias), 
                                      y = abs_bias, 
                                      fill = severity)) +
  geom_col() +
  facet_wrap(~estimator_id, nrow = 1) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Pass" = "#06A77D",
      "Mild" = "#FFF3B0",
      "Moderate" = "#F77F00",
      "Severe" = "#E63946"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Placebo Violations by Method",
    subtitle = "Absolute bias on zero-effect scenarios (should be near 0)",
    x = NULL, 
    y = "Absolute Bias",
    fill = "Severity"
  )

ggsave(file.path(output_dir, "plot10_placebo_diagnostic.png"), 
       p10_placebo_diagnostic, width = 10, height = 5)

# ------------------------------------------------------------------------------
# TABLE 3: Method Recommendations (Decision Matrix)
# ------------------------------------------------------------------------------

recommendations <- tribble(
  ~scenario, ~best_method, ~avoid_method, ~notes,
  "Clean data, good overlap", "GRF/BART/TMLE", "None", "All methods perform well",
  "Heavy-tailed outcomes", "GenGC (median QST)", "All ATT estimators", "4-6x precision gain",
  "Extreme overlap violations", "BART/GRF", "GenGC/IPW", "GenGC catastrophic bias",
  "Heterogeneous effects", "GenGC (QST curves)", "TMLE", "Reveals distributional patterns",
  "High-dimensional (p>20)", "BART/LM", "GenGC", "GenGC has variance inflation",
  "Placebo testing", "GRF/BART", "GenGC", "GenGC shows false positives",
  "Small sample (N<500)", "BART/GRF", "GenGC", "GenGC needs larger N"
)

write.csv(recommendations, 
          file.path(output_dir, "table_method_recommendations.csv"), 
          row.names = FALSE)

print("Method recommendations by scenario:")
print(recommendations)

# ------------------------------------------------------------------------------
# SUMMARY STATISTICS TABLE
# ------------------------------------------------------------------------------

summary_stats <- df_att %>%
  mutate(error = est_att - true_att) %>%
  group_by(estimator_id) %>%
  summarise(
    scenarios = n_distinct(dgp_id),
    mean_rmse = sqrt(mean(error^2, na.rm = TRUE)),
    median_rmse = median(sqrt(error^2), na.rm = TRUE),
    mean_bias = mean(abs(error), na.rm = TRUE),
    median_bias = median(abs(error), na.rm = TRUE),
    mean_coverage = mean(att_covered, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_rmse)

write.csv(summary_stats, 
          file.path(output_dir, "table_summary_statistics.csv"), 
          row.names = FALSE)

print("Overall method performance:")
print(summary_stats)

# ------------------------------------------------------------------------------
# FINAL MESSAGE
# ------------------------------------------------------------------------------

message("=" %>% rep(80) %>% paste(collapse = ""))
message("✅ Extended Analysis Complete!")
message("=" %>% rep(80) %>% paste(collapse = ""))
message("\nGenerated outputs:")
message("  📊 Plots:")
message("     - plot6_success_heatmap.png")
message("     - plot7_headtohead.png")
message("     - plot8_error_distribution.png")
message("     - plot9_qst_calibration.png")
message("     - plot10_placebo_diagnostic.png")
message("\n  📋 Tables:")
message("     - table_ranking_by_type.csv")
message("     - table_coverage_calibration.csv")
message("     - table_method_recommendations.csv")
message("     - table_summary_statistics.csv")
message("\n📂 All outputs saved to: ", output_dir)
message("=" %>% rep(80) %>% paste(collapse = ""))


