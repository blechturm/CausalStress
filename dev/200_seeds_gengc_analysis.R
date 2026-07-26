# ------------------------------------------------------------------------------
# Expanded Analysis: GenGC Dual-Engine & Screening Benchmark
# ------------------------------------------------------------------------------
library(CausalStress)
library(pins)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# 1. Setup Board and Connect
# Ensure this path matches your GenGC benchmark board
board_path <- "C:/Simulations/CausalStress_GenGC_Benchmark_v4_Board"
board <- board_folder(board_path)

# 2. Consolidate and Tidy Data
# This step replaces manual pin_read() loops with the native tidy workflow
message("📦 Loading benchmark batches...")
all_pins <- pin_list(board)
batch_pins <- all_pins[grepl("^batch_", all_pins)]

df_tidy <- map_dfr(batch_pins, function(pin_name) {
  tryCatch({
    raw_batch <- cs_read_batch(board, pin_name)
    cs_tidy_batch(raw_batch)
  }, error = function(e) NULL)
}, .progress = TRUE)

# 3. Collect specialized results
df_att <- cs_collect_att(df_tidy)
df_qst <- cs_collect_qst(df_tidy)

# 4. Widen Analysis: Contender Performance Matrix
# Define your specific variants for comparison
contenders <- c("gengc", "gengc_qr_vanilla", "gengc_qr_screen", "gengc_rf_screen", "gengc_auto")

contenders <- c(
  "oracle_att", 
  "lm_att", 
  "gengc",
  "gengc_dr",
  "gengc_qr_screen",
  "gengc_rf_screen",
  "gengc_auto",
  "gengc_dr_qr",
  "gengc_dr_rf_screen",
  "grf_dr_att",
  "bart_att",     # Requires bartCause
  "tmle_att"      # Requires tmle, SuperLearner
)

# 4.1. The "Desert" Challenge (Sparsity)
# High-dimensional sparse PLM is where the QR engine should excel
p_desert <- df_att %>%
  filter(dgp_id == "synth_hd_sparse_plm", estimator_id %in% contenders) %>%
  mutate(error = est_att - true_att) %>%
  ggplot(aes(x = estimator_id, y = error, fill = estimator_id)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Engine Shootout: High-Dimensional Sparsity",
       subtitle = "Linear QR vs. Forests in 'The Desert'",
       y = "Error (Estimate - Truth)")

# 4.2. Global RMSE Heatmap
# Compare across all DGPs to see where engines win/fail
summary_matrix <- df_att %>%
  filter(estimator_id %in% contenders) %>%
  group_by(dgp_id, estimator_id) %>%
  summarise(rmse = sqrt(mean((est_att - true_att)^2, na.rm = TRUE)), .groups = "drop")

summary_matrix

p_heatmap <- ggplot(summary_matrix, aes(x = dgp_id, y = estimator_id, fill = rmse)) +
  geom_tile() +
  scale_fill_gradient(low = "#06A77D", high = "#E63946") +
  geom_text(aes(label = round(rmse, 3)), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "RMSE Matrix: All Variants vs. All DGPs",
       x = "DGP Architecture", y = "GenGC Variant")


p_heatmap

# 5. Export Results
#write.csv(summary_matrix, "C:/Simulations/GenGC_Benchmark_Summary.csv", row.names = FALSE)