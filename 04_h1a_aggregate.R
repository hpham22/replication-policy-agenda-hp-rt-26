###############################################################################
# 04_h1a_aggregate.R
# Table 2 — Aggregate distributional statistics
# Figure 3 — Distribution of annual percentage changes
###############################################################################

cat("\n=== 04_h1a_aggregate.R ===\n")

if (!exists("ALL_YEARS")) source("00_setup.R")

load(file.path(tables_dir, "pct_changes.RData"))

# ============================================================================
# Descriptive statistics for both distributions
# ============================================================================
cat("\n--- Descriptive statistics ---\n")

compute_desc_stats <- function(x, label) {
  cat(sprintf("\n  %s (N = %d)\n", label, length(x)))
  n <- length(x)
  raw_kurt <- calc_kurtosis_raw(x)
  lm_result <- calc_lmoments(x)
  lkurt <- as.numeric(lm_result$ratios["tau4"])
  sw <- shapiro.test(x)
  cat(sprintf("  Kurtosis: %.4f | L-kurtosis: %.4f | SW p: %.2e\n",
              raw_kurt, lkurt, sw$p.value))
  list(
    N = n, Min = min(x), Max = max(x),
    Mean = mean(x), Median = median(x), SD = sd(x),
    Skewness = calc_skewness(x),
    Prop_Zeros = mean(x == 0), Prop_Neg1 = mean(x == -1),
    Raw_Kurtosis = raw_kurt, Excess_Kurtosis = calc_kurtosis_excess(x),
    L_Kurtosis = lkurt,
    SW_W = unname(sw$statistic), SW_p = sw$p.value
  )
}

stats_restr <- compute_desc_stats(pooled_restricted, "Restricted (excl 0-to-0)")
stats_full <- compute_desc_stats(pooled_full, "Full Distribution")

# ============================================================================
# T-distribution fitting
# ============================================================================
cat("\n--- T-distribution fitting ---\n")

tfit_restr <- fit_location_scale_t(pooled_restricted, label = "restricted")
tfit_full <- fit_location_scale_t(pooled_full, label = "full")

# Save t-fit results for appendix B6
save(tfit_full, tfit_restr,
     file = file.path(tables_dir, "tfit_aggregate.RData"))

# ============================================================================
# Table 2 — Two-column distributional statistics
# ============================================================================
cat("\n--- Building Table 2 ---\n")

fmt <- function(x, d = 4) {
  if (is.na(x)) return("NA")
  if (is.infinite(x)) return("Inf")
  formatC(x, format = "f", digits = d)
}

fmt_se <- function(val, se, d = 4) {
  if (is.na(val)) return("NA")
  sprintf("%s (%s)", fmt(val, d), fmt(se, d))
}

n_excluded <- sum(all_area_pct$status == "excluded_undefined")
n_zero_to_zero <- sum(all_area_pct$status == "zero_to_zero")

stat_names <- c(
  "N",
  "N excluded (0 -> positive)",
  "N zero-to-zero",
  "Minimum", "Maximum",
  "Mean", "Median", "Standard Deviation",
  "Skewness",
  "Proportion zeros", "Proportion -1",
  "Raw Kurtosis (benchmark=3)",
  "Excess Kurtosis (benchmark=0)",
  "L-kurtosis (tau_4, benchmark=0.123)",
  "Shapiro-Wilk W", "Shapiro-Wilk p-value",
  "t-fit: mu (SE)", "t-fit: sigma (SE)", "t-fit: sigma^2",
  "t-fit: nu (SE)", "t-fit: Theo. variance", "t-fit: AIC", "t-fit: Note"
)

restr_vals <- c(
  as.character(stats_restr$N), as.character(n_excluded), "(excluded)",
  fmt(stats_restr$Min), fmt(stats_restr$Max),
  fmt(stats_restr$Mean), fmt(stats_restr$Median), fmt(stats_restr$SD),
  fmt(stats_restr$Skewness),
  fmt(stats_restr$Prop_Zeros, 3), fmt(stats_restr$Prop_Neg1, 3),
  fmt(stats_restr$Raw_Kurtosis), fmt(stats_restr$Excess_Kurtosis),
  fmt(stats_restr$L_Kurtosis),
  fmt(stats_restr$SW_W, 6), formatC(stats_restr$SW_p, format = "e", digits = 4),
  fmt_se(tfit_restr$mu, tfit_restr$se_mu),
  fmt_se(tfit_restr$sigma, tfit_restr$se_sigma),
  fmt(tfit_restr$sigma_sq, 6),
  fmt_se(tfit_restr$nu, tfit_restr$se_nu),
  if (is.infinite(tfit_restr$theo_var)) "Inf" else fmt(tfit_restr$theo_var),
  fmt(tfit_restr$aic, 2), tfit_restr$note
)

full_vals <- c(
  as.character(stats_full$N), as.character(n_excluded), as.character(n_zero_to_zero),
  fmt(stats_full$Min), fmt(stats_full$Max),
  fmt(stats_full$Mean), fmt(stats_full$Median), fmt(stats_full$SD),
  fmt(stats_full$Skewness),
  fmt(stats_full$Prop_Zeros, 3), fmt(stats_full$Prop_Neg1, 3),
  fmt(stats_full$Raw_Kurtosis), fmt(stats_full$Excess_Kurtosis),
  fmt(stats_full$L_Kurtosis),
  fmt(stats_full$SW_W, 6), formatC(stats_full$SW_p, format = "e", digits = 4),
  fmt_se(tfit_full$mu, tfit_full$se_mu),
  fmt_se(tfit_full$sigma, tfit_full$se_sigma),
  fmt(tfit_full$sigma_sq, 6),
  fmt_se(tfit_full$nu, tfit_full$se_nu),
  if (is.infinite(tfit_full$theo_var)) "Inf" else fmt(tfit_full$theo_var),
  fmt(tfit_full$aic, 2), tfit_full$note
)

benchmarks <- c(
  "", "", "", "", "",
  "", "", "", "0 for symmetric", "", "",
  "3 (normal)", "0 (normal)", "0.1226 (normal)",
  "", "< 0.05 rejects normality",
  "", "", "", "", "", "", ""
)

table2 <- tibble(
  Statistic = stat_names,
  `Restricted (primary)` = restr_vals,
  `Full (robustness)` = full_vals,
  Benchmark = benchmarks
)

table2_path <- file.path(tables_dir, "table2_aggregate_stats.csv")
write_csv(table2, table2_path)
verify_output(table2_path)

# --- kableExtra output ---
save_table(
  table2, "table2_aggregate_stats",
  caption = "Table 2. Aggregate Distributional Statistics",
  footnote = "Restricted distribution excludes 0-to-0 transitions. Benchmarks: kurtosis = 3 (normal), L-kurtosis = 0.1226 (normal).",
  col_names = c("Statistic", "Restricted (primary)", "Full (robustness)", "Benchmark")
)

# ============================================================================
# Figure 3 — Histogram with normal overlay
# ============================================================================
cat("\n--- Figure 3 ---\n")

hist_data <- tibble(pct = pooled_restricted)
x_range <- seq(min(pooled_restricted) - 0.5, max(pooled_restricted) + 0.5, length.out = 500)

normal_overlay <- tibble(
  x = x_range,
  density = dnorm(x_range, mean = mean(pooled_restricted), sd = sd(pooled_restricted))
)

annot_text <- sprintf(
  "N = %d\nKurtosis = %.1f\nL-kurtosis = %.4f",
  stats_restr$N, stats_restr$Raw_Kurtosis, stats_restr$L_Kurtosis
)

fig3 <- ggplot() +
  geom_histogram(data = hist_data,
                 aes(x = pct, y = after_stat(density)),
                 bins = 50, fill = "grey40", colour = "white",
                 linewidth = 0.2) +
  geom_line(data = normal_overlay,
            aes(x = x, y = density),
            linetype = "dashed", colour = "black", linewidth = 0.8) +
  annotate("label", x = max(pooled_restricted) * 0.5, y = Inf,
           label = annot_text, hjust = 0, vjust = 1.5,
           size = 4, family = "serif",
           fill = "white", colour = "black",
           label.size = 0.4, label.padding = unit(0.4, "lines")) +
  scale_x_continuous(breaks = c(-1, seq(0, ceiling(max(pooled_restricted)), by = 2))) +
  coord_cartesian(xlim = c(-1.5, max(pooled_restricted) + 0.5)) +
  labs(x = "Annual percentage change", y = "Density") +
  theme_pub()

save_figure(fig3, "fig3_pct_change_distribution", width = 6.5, height = 4.5)

cat("\n04_h1a_aggregate.R completed.\n")
