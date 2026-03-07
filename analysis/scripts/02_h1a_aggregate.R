###############################################################################
# 02_h1a_aggregate.R
# Step 3: H1a — Aggregate Punctuated Equilibrium
###############################################################################

cat("\n=== 02_h1a_aggregate.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "pct_changes.RData"))

# ============================================================================
# 3.1-3.2: Descriptive statistics for both distributions
# ============================================================================
cat("\n--- 3.1-3.2: Descriptive statistics ---\n")

compute_desc_stats <- function(x, label) {
  cat(sprintf("\n  %s (N = %d)\n", label, length(x)))
  n <- length(x)
  cat(sprintf("  Mean: %.4f, SD: %.4f, Median: %.4f\n",
              mean(x), sd(x), median(x)))
  cat(sprintf("  Min: %.4f, Max: %.4f\n", min(x), max(x)))
  cat(sprintf("  Prop zeros: %.3f, Prop -1: %.3f\n",
              mean(x == 0), mean(x == -1)))

  # Kurtosis
  raw_kurt <- calc_kurtosis_raw(x)
  excess_kurt <- calc_kurtosis_excess(x)
  cat(sprintf("  Raw kurtosis: %.4f (benchmark=3)\n", raw_kurt))

  # L-kurtosis
  lm_result <- calc_lmoments(x)
  lkurt <- as.numeric(lm_result$ratios["tau4"])
  cat(sprintf("  L-kurtosis: %.4f (benchmark=0.1226)\n", lkurt))

  # Shapiro-Wilk
  sw <- shapiro.test(x)
  cat(sprintf("  Shapiro-Wilk W: %.6f, p: %.6e\n", sw$statistic, sw$p.value))

  list(
    N = n, Min = min(x), Max = max(x),
    Mean = mean(x), Median = median(x), SD = sd(x),
    Skewness = calc_skewness(x),
    Prop_Zeros = mean(x == 0), Prop_Neg1 = mean(x == -1),
    Raw_Kurtosis = raw_kurt, Excess_Kurtosis = excess_kurt,
    L_Kurtosis = lkurt,
    SW_W = unname(sw$statistic), SW_p = sw$p.value
  )
}

stats_full <- compute_desc_stats(pooled_full, "Full Distribution")
stats_restr <- compute_desc_stats(pooled_restricted, "Restricted (excl 0-to-0)")

# ============================================================================
# 3.5: T-distribution fitting
# ============================================================================
cat("\n--- 3.5: T-distribution fitting ---\n")

cat("\n  Fitting full distribution...\n")
tfit_full <- fit_location_scale_t(pooled_full, label = "full")
cat(sprintf("  Full: mu=%.6f, sigma=%.6f, nu=%.4f (%s)\n",
            tfit_full$mu, tfit_full$sigma, tfit_full$nu, tfit_full$note))

cat("\n  Fitting restricted distribution...\n")
tfit_restr <- fit_location_scale_t(pooled_restricted, label = "restricted")
cat(sprintf("  Restricted: mu=%.6f, sigma=%.6f, nu=%.4f (%s)\n",
            tfit_restr$mu, tfit_restr$sigma, tfit_restr$nu, tfit_restr$note))

# ============================================================================
# 3.7: Table 1 — Two-column descriptive statistics
# ============================================================================
cat("\n--- 3.7: Building Table 1 ---\n")

fmt <- function(x, d = 4) {
  if (is.na(x)) return("NA")
  if (is.infinite(x)) return("Inf")
  formatC(x, format = "f", digits = d)
}

fmt_se <- function(val, se, d = 4) {
  if (is.na(val)) return("NA")
  sprintf("%s (%s)", fmt(val, d), fmt(se, d))
}

stat_names <- c(
  "N",
  "N excluded (0 -> positive)",
  "N zero-to-zero",
  "Minimum",
  "Maximum",
  "Mean",
  "Median",
  "Standard Deviation",
  "Skewness",
  "Proportion zeros",
  "Proportion -1",
  "Raw Kurtosis (benchmark=3)",
  "Excess Kurtosis (benchmark=0)",
  "L-kurtosis (tau_4, benchmark=0.123)",
  "Shapiro-Wilk W",
  "Shapiro-Wilk p-value",
  "t-fit: mu (SE)",
  "t-fit: sigma (SE)",
  "t-fit: sigma^2",
  "t-fit: nu (SE)",
  "t-fit: Theo. variance",
  "t-fit: AIC",
  "t-fit: Note"
)

n_excluded <- sum(all_area_pct$status == "excluded_undefined")
n_zero_to_zero <- sum(all_area_pct$status == "zero_to_zero")

full_vals <- c(
  as.character(stats_full$N),
  as.character(n_excluded),
  as.character(n_zero_to_zero),
  fmt(stats_full$Min), fmt(stats_full$Max),
  fmt(stats_full$Mean), fmt(stats_full$Median),
  fmt(stats_full$SD),
  fmt(stats_full$Skewness),
  fmt(stats_full$Prop_Zeros, 3), fmt(stats_full$Prop_Neg1, 3),
  fmt(stats_full$Raw_Kurtosis),
  fmt(stats_full$Excess_Kurtosis),
  fmt(stats_full$L_Kurtosis),
  fmt(stats_full$SW_W, 6),
  formatC(stats_full$SW_p, format = "e", digits = 4),
  fmt_se(tfit_full$mu, tfit_full$se_mu),
  fmt_se(tfit_full$sigma, tfit_full$se_sigma),
  fmt(tfit_full$sigma_sq, 6),
  fmt_se(tfit_full$nu, tfit_full$se_nu),
  if (is.infinite(tfit_full$theo_var)) "Inf" else fmt(tfit_full$theo_var),
  fmt(tfit_full$aic, 2),
  tfit_full$note
)

restr_vals <- c(
  as.character(stats_restr$N),
  as.character(n_excluded),
  "(excluded)",
  fmt(stats_restr$Min), fmt(stats_restr$Max),
  fmt(stats_restr$Mean), fmt(stats_restr$Median),
  fmt(stats_restr$SD),
  fmt(stats_restr$Skewness),
  fmt(stats_restr$Prop_Zeros, 3), fmt(stats_restr$Prop_Neg1, 3),
  fmt(stats_restr$Raw_Kurtosis),
  fmt(stats_restr$Excess_Kurtosis),
  fmt(stats_restr$L_Kurtosis),
  fmt(stats_restr$SW_W, 6),
  formatC(stats_restr$SW_p, format = "e", digits = 4),
  fmt_se(tfit_restr$mu, tfit_restr$se_mu),
  fmt_se(tfit_restr$sigma, tfit_restr$se_sigma),
  fmt(tfit_restr$sigma_sq, 6),
  fmt_se(tfit_restr$nu, tfit_restr$se_nu),
  if (is.infinite(tfit_restr$theo_var)) "Inf" else fmt(tfit_restr$theo_var),
  fmt(tfit_restr$aic, 2),
  tfit_restr$note
)

benchmarks <- c(
  "", "", "",
  "", "",
  "", "", "",
  "0 for symmetric",
  "", "",
  "3 (normal)",
  "0 (normal)",
  "0.1226 (normal)",
  "", "< 0.05 rejects normality",
  "", "", "", "", "", "", ""
)

table1 <- tibble(
  Statistic = stat_names,
  `Primary (excl 0-to-0)` = restr_vals,
  `Full (robustness)` = full_vals,
  Benchmark = benchmarks
)

table1_path <- file.path(tables_dir, "table1_h1a_aggregate.csv")
write_csv(table1, table1_path)
verify_output(table1_path)

# Save t-fit results for other scripts
save(tfit_full, tfit_restr,
     file = file.path(tables_dir, "tfit_aggregate.RData"))

# ============================================================================
# 3.8: Figure 3 — Histogram with distributional overlays
# ============================================================================
cat("\n--- 3.8: Figure 3 ---\n")

# Use restricted distribution (primary) for histogram
hist_data <- tibble(pct = pooled_restricted)

# Build overlay curves
x_range <- seq(min(pooled_restricted) - 0.5, max(pooled_restricted) + 0.5, length.out = 500)

# Normal overlay (fit to restricted)
normal_overlay <- tibble(
  x = x_range,
  density = dnorm(x_range, mean = mean(pooled_restricted), sd = sd(pooled_restricted)),
  Distribution = "Normal"
)

# T-distribution overlay (use restricted fit)
if (!is.na(tfit_restr$sigma) && tfit_restr$sigma > 1e-4) {
  tfit_for_plot <- tfit_restr
  t_label <- "Fitted t"
} else {
  tfit_for_plot <- NULL
  t_label <- NULL
}

overlay_df <- normal_overlay
if (!is.null(tfit_for_plot)) {
  t_overlay <- tibble(
    x = x_range,
    density = dlst(x_range, mu = tfit_for_plot$mu,
                   sigma = tfit_for_plot$sigma, nu = tfit_for_plot$nu),
    Distribution = t_label
  )
  overlay_df <- bind_rows(overlay_df, t_overlay)
}

# Determine annotation text (restricted as primary)
annot_text <- sprintf(
  "Kurtosis = %.1f\nL-kurtosis = %.4f\nN = %d",
  stats_restr$Raw_Kurtosis, stats_restr$L_Kurtosis, stats_restr$N
)
if (!is.null(tfit_for_plot)) {
  annot_text <- paste0(annot_text,
    sprintf("\n\u03BD = %.2f", tfit_for_plot$nu))
}

fig3 <- ggplot() +
  geom_histogram(data = hist_data,
                 aes(x = pct, y = after_stat(density)),
                 bins = 50, fill = "grey75", colour = "grey40",
                 linewidth = 0.2) +
  geom_line(data = overlay_df,
            aes(x = x, y = density, linetype = Distribution),
            colour = "black", linewidth = 0.8) +
  scale_linetype_manual(
    values = c("Normal" = "dashed",
               setNames("solid", t_label))
  ) +
  annotate("text", x = max(pooled_restricted) * 0.6, y = Inf,
           label = annot_text, hjust = 0, vjust = 1.5,
           size = 3, family = "mono") +
  coord_cartesian(xlim = c(-1.5, max(pooled_restricted) + 0.5)) +
  labs(
    x = "Annual Percentage Change",
    y = "Density",
    title = "Distribution of Pooled Annual Percentage Changes (Restricted)",
    subtitle = sprintf("N = %d (excl. 0-to-0) | Full distribution (N = %d) in robustness appendix",
                       length(pooled_restricted), length(pooled_full)),
    linetype = "Overlay"
  ) +
  theme_pub()

save_figure(fig3, "fig3_histogram_pct_changes", width = 8, height = 5)

cat("\n02_h1a_aggregate.R completed.\n")
