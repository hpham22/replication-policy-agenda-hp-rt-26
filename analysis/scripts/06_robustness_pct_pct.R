###############################################################################
# 06_robustness_pct_pct.R
# Robustness: Percentage-percentage method (Lundgren et al. 2018)
# First differences in proportional shares, not percentage changes in counts
###############################################################################

cat("\n=== 06_robustness_pct_pct.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

# ============================================================================
# Step 1: Construct share matrix
# ============================================================================
cat("\n--- Step 1: Share matrix ---\n")

# Extract the count matrix (years x areas)
count_mat <- as.matrix(matrix_wide[, area_cols])
rownames(count_mat) <- matrix_wide$year
row_totals <- matrix_wide$total

# Compute shares: s_it = x_it / X_t (NA for zero-output years)
share_mat <- count_mat
for (i in seq_len(nrow(count_mat))) {
  if (row_totals[i] == 0) {
    share_mat[i, ] <- NA
  } else {
    share_mat[i, ] <- count_mat[i, ] / row_totals[i]
  }
}

# Verify: rows sum to 1 (or NA)
row_sums <- rowSums(share_mat, na.rm = FALSE)
non_na_sums <- row_sums[!is.na(row_sums)]
cat(sprintf("Share matrix: %d years, %d areas\n", nrow(share_mat), ncol(share_mat)))
cat(sprintf("Non-zero-output years: %d (all row sums = 1: %s)\n",
            length(non_na_sums), all(abs(non_na_sums - 1) < 1e-10)))
cat(sprintf("Zero-output years (NA rows): %d\n", sum(is.na(row_sums))))

# ============================================================================
# Step 2: First differences in consecutive calendar years
# ============================================================================
cat("\n--- Step 2: First differences ---\n")

n_years <- nrow(share_mat)
diff_mat <- share_mat[-1, ] - share_mat[-n_years, ]
diff_years_from <- matrix_wide$year[-n_years]
diff_years_to <- matrix_wide$year[-1]

# A pair is valid only if both years have positive output (non-NA shares)
valid_pairs <- complete.cases(share_mat[-n_years, ]) & complete.cases(share_mat[-1, ])
cat(sprintf("Total consecutive year-pairs: %d\n", length(valid_pairs)))
cat(sprintf("Valid pairs (both years have output): %d\n", sum(valid_pairs)))
cat(sprintf("Invalid pairs (at least one zero-output year): %d\n", sum(!valid_pairs)))

# Extract valid differences
diff_valid <- diff_mat[valid_pairs, ]

# Verify zero-sum property: each row should sum to 0
row_sums_diff <- rowSums(diff_valid)
cat(sprintf("Zero-sum check: max absolute row sum = %.2e (should be ~0)\n",
            max(abs(row_sums_diff))))

# Pool into single vector
pooled_pctpct <- as.vector(diff_valid)
cat(sprintf("Pooled percentage-percentage changes: N = %d\n", length(pooled_pctpct)))
cat(sprintf("  (= %d valid pairs x %d areas)\n", sum(valid_pairs), ncol(diff_valid)))

# ============================================================================
# Step 3: H1a — Aggregate distributional statistics
# ============================================================================
cat("\n--- Step 3: H1a aggregate comparison ---\n")

pctpct_stats <- list(
  N = length(pooled_pctpct),
  Mean = mean(pooled_pctpct),
  Median = median(pooled_pctpct),
  SD = sd(pooled_pctpct),
  Min = min(pooled_pctpct),
  Max = max(pooled_pctpct),
  Kurtosis = calc_kurtosis_raw(pooled_pctpct),
  L_Kurtosis = as.numeric(calc_lkurtosis(pooled_pctpct)),
  Prop_Zeros = mean(pooled_pctpct == 0),
  SW_W = unname(shapiro.test(pooled_pctpct)$statistic),
  SW_p = shapiro.test(pooled_pctpct)$p.value
)

cat(sprintf("  N: %d\n", pctpct_stats$N))
cat(sprintf("  Mean: %.6f, Median: %.6f, SD: %.6f\n",
            pctpct_stats$Mean, pctpct_stats$Median, pctpct_stats$SD))
cat(sprintf("  Kurtosis: %.4f (benchmark = 3)\n", pctpct_stats$Kurtosis))
cat(sprintf("  L-kurtosis: %.4f (benchmark = 0.1226)\n", pctpct_stats$L_Kurtosis))
cat(sprintf("  Shapiro-Wilk: W = %.6f, p = %.2e\n", pctpct_stats$SW_W, pctpct_stats$SW_p))
cat(sprintf("  Prop zeros: %.3f\n", pctpct_stats$Prop_Zeros))

# Restricted pct-count stats (from previous analysis)
restr_stats <- list(
  N = length(pooled_restricted),
  Mean = mean(pooled_restricted),
  Median = median(pooled_restricted),
  SD = sd(pooled_restricted),
  Min = min(pooled_restricted),
  Max = max(pooled_restricted),
  Kurtosis = calc_kurtosis_raw(pooled_restricted),
  L_Kurtosis = as.numeric(calc_lkurtosis(pooled_restricted)),
  Prop_Zeros = mean(pooled_restricted == 0),
  SW_W = unname(shapiro.test(pooled_restricted)$statistic),
  SW_p = shapiro.test(pooled_restricted)$p.value
)

fmt4 <- function(x) formatC(x, format = "f", digits = 4)
fmt6 <- function(x) formatC(x, format = "f", digits = 6)

comparison_h1a <- tibble(
  Statistic = c("N", "Mean", "Median", "SD", "Min", "Max",
                "Raw Kurtosis (benchmark=3)",
                "L-kurtosis (benchmark=0.1226)",
                "Shapiro-Wilk W", "Shapiro-Wilk p",
                "Proportion zeros"),
  `Pct-Count (restricted)` = c(
    as.character(restr_stats$N),
    fmt6(restr_stats$Mean), fmt6(restr_stats$Median), fmt4(restr_stats$SD),
    fmt4(restr_stats$Min), fmt4(restr_stats$Max),
    fmt4(restr_stats$Kurtosis), fmt4(restr_stats$L_Kurtosis),
    fmt6(restr_stats$SW_W), formatC(restr_stats$SW_p, format = "e", digits = 4),
    fmt4(restr_stats$Prop_Zeros)
  ),
  `Pct-Pct` = c(
    as.character(pctpct_stats$N),
    fmt6(pctpct_stats$Mean), fmt6(pctpct_stats$Median), fmt4(pctpct_stats$SD),
    fmt4(pctpct_stats$Min), fmt4(pctpct_stats$Max),
    fmt4(pctpct_stats$Kurtosis), fmt4(pctpct_stats$L_Kurtosis),
    fmt6(pctpct_stats$SW_W), formatC(pctpct_stats$SW_p, format = "e", digits = 4),
    fmt4(pctpct_stats$Prop_Zeros)
  ),
  `Lundgren et al. range` = c(
    "", "", "", "", "", "",
    "",
    "EU=0.26, OAS=0.26, UN=0.28, AU=0.30, OIC=0.31",
    "", "", ""
  )
)

cat("\nH1a method comparison:\n")
print(comparison_h1a, width = 120)

comp_h1a_path <- file.path(tables_dir, "appendix_pctpct_h1a_comparison.csv")
write_csv(comparison_h1a, comp_h1a_path)
verify_output(comp_h1a_path)

# ============================================================================
# Step 4: H1b — Core vs. peripheral comparison
# ============================================================================
cat("\n--- Step 4: H1b core vs peripheral ---\n")

core_idx <- which(area_cols %in% paste0("area_", CORE_AREAS))
periph_idx <- which(!area_cols %in% paste0("area_", CORE_AREAS))

core_pctpct <- as.vector(diff_valid[, core_idx])
periph_pctpct <- as.vector(diff_valid[, periph_idx])

compute_pctpct_group <- function(x, label) {
  n <- length(x)
  cat(sprintf("\n  %s (n = %d)\n", label, n))
  kurt <- calc_kurtosis_raw(x)
  lkurt <- as.numeric(calc_lkurtosis(x))
  sw <- shapiro.test(x)
  cat(sprintf("  Kurtosis: %.4f | L-kurtosis: %.4f\n", kurt, lkurt))
  cat(sprintf("  Shapiro-Wilk: W=%.6f, p=%.2e\n", sw$statistic, sw$p.value))
  list(N = n, Mean = mean(x), SD = sd(x),
       Kurtosis = kurt, L_Kurtosis = lkurt,
       SW_W = unname(sw$statistic), SW_p = sw$p.value,
       Prop_Zeros = mean(x == 0))
}

stats_agg_pp <- compute_pctpct_group(pooled_pctpct, "Aggregate")
stats_core_pp <- compute_pctpct_group(core_pctpct, "Core")
stats_periph_pp <- compute_pctpct_group(periph_pctpct, "Peripheral")

comparison_h1b <- tibble(
  Metric = c("N", "Mean", "SD",
             "Raw Kurtosis (benchmark=3)",
             "L-kurtosis (benchmark=0.1226)",
             "Shapiro-Wilk W", "Shapiro-Wilk p",
             "Proportion zeros"),
  Aggregate = c(
    as.character(stats_agg_pp$N),
    fmt6(stats_agg_pp$Mean), fmt4(stats_agg_pp$SD),
    fmt4(stats_agg_pp$Kurtosis), fmt4(stats_agg_pp$L_Kurtosis),
    fmt6(stats_agg_pp$SW_W), formatC(stats_agg_pp$SW_p, format = "e", digits = 4),
    fmt4(stats_agg_pp$Prop_Zeros)
  ),
  Core = c(
    as.character(stats_core_pp$N),
    fmt6(stats_core_pp$Mean), fmt4(stats_core_pp$SD),
    fmt4(stats_core_pp$Kurtosis), fmt4(stats_core_pp$L_Kurtosis),
    fmt6(stats_core_pp$SW_W), formatC(stats_core_pp$SW_p, format = "e", digits = 4),
    fmt4(stats_core_pp$Prop_Zeros)
  ),
  Peripheral = c(
    as.character(stats_periph_pp$N),
    fmt6(stats_periph_pp$Mean), fmt4(stats_periph_pp$SD),
    fmt4(stats_periph_pp$Kurtosis), fmt4(stats_periph_pp$L_Kurtosis),
    fmt6(stats_periph_pp$SW_W), formatC(stats_periph_pp$SW_p, format = "e", digits = 4),
    fmt4(stats_periph_pp$Prop_Zeros)
  )
)

cat("\nH1b core-peripheral comparison (pct-pct method):\n")
print(comparison_h1b, width = 120)

comp_h1b_path <- file.path(tables_dir, "appendix_pctpct_h1b_comparison.csv")
write_csv(comparison_h1b, comp_h1b_path)
verify_output(comp_h1b_path)

# ============================================================================
# Step 5: Histogram figure
# ============================================================================
cat("\n--- Step 5: Histogram ---\n")

hist_data_pp <- tibble(pct = pooled_pctpct)
x_range_pp <- seq(min(pooled_pctpct) - 0.05, max(pooled_pctpct) + 0.05, length.out = 500)

normal_overlay_pp <- tibble(
  x = x_range_pp,
  density = dnorm(x_range_pp, mean = mean(pooled_pctpct), sd = sd(pooled_pctpct)),
  Distribution = "Normal"
)

annot_pp <- sprintf(
  "Kurtosis = %.1f\nL-kurtosis = %.4f\nN = %d\nProp zeros = %.3f",
  pctpct_stats$Kurtosis, pctpct_stats$L_Kurtosis,
  pctpct_stats$N, pctpct_stats$Prop_Zeros
)

fig_pctpct <- ggplot() +
  geom_histogram(data = hist_data_pp,
                 aes(x = pct, y = after_stat(density)),
                 bins = 50, fill = "grey75", colour = "grey40",
                 linewidth = 0.2) +
  geom_line(data = normal_overlay_pp,
            aes(x = x, y = density),
            colour = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = max(pooled_pctpct) * 0.5, y = Inf,
           label = annot_pp, hjust = 0, vjust = 1.5,
           size = 3, family = "mono") +
  labs(
    x = "First Difference in Proportional Share",
    y = "Density",
    title = "Distribution of Percentage-Percentage Changes (Lundgren et al. method)",
    subtitle = sprintf("N = %d | Dashed line = Normal(%.4f, %.4f)",
                       pctpct_stats$N, pctpct_stats$Mean, pctpct_stats$SD)
  ) +
  theme_pub()

save_figure(fig_pctpct, "fig_appendix_pctpct_histogram", width = 8, height = 5)

# ============================================================================
# Note on variance decomposition
# ============================================================================
cat("\n--- Note on variance decomposition ---\n")
cat("The percentage-percentage method is zero-sum by construction: all changes\n")
cat("within a year sum to zero. This means between-group variance has different\n")
cat("mathematical properties than under the percentage-count method. A formal\n")
cat("decomposition is not reported here to avoid methodological conflation.\n")

cat("\n06_robustness_pct_pct.R completed.\n")
