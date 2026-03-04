###############################################################################
# 02_h1a_aggregate.R
# Step 1: H1a — Aggregate Punctuated Equilibrium
###############################################################################

cat("\n=== 02_h1a_aggregate.R ===\n")

# Source setup if not already loaded
if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

# Load matrix data
load(file.path(tables_dir, "matrix_data.RData"))

# ============================================================================
# Step 1.1: Area-level percentage changes, pooled
# ============================================================================
cat("\n--- Step 1.1: Computing area-level percentage changes ---\n")

# For each area, compute year-over-year percentage changes
area_pct_list <- list()

for (area_code in ALL_AREAS) {
  col_name <- paste0("area_", area_code)
  counts <- matrix_wide[[col_name]]
  pct_df <- compute_pct_changes(counts, ALL_YEARS)
  pct_df$main_area <- area_code
  area_pct_list[[as.character(area_code)]] <- pct_df
}

# Combine all area-level pct changes
all_area_pct <- bind_rows(area_pct_list)

# Summary of transitions
cat("Total transitions:", nrow(all_area_pct), "\n")
cat("By status:\n")
print(table(all_area_pct$status))

# Pool valid (non-excluded) percentage changes
pooled_pct <- all_area_pct %>%
  filter(!is.na(pct_change)) %>%
  pull(pct_change)

n_valid <- length(pooled_pct)
n_excluded <- sum(all_area_pct$status == "excluded_undefined")
cat("\nValid percentage changes:", n_valid, "\n")
cat("Excluded (0 -> >0):", n_excluded, "\n")

# Save area-level pct changes for use in H1b
save(all_area_pct, pooled_pct, file = file.path(tables_dir, "pct_changes.RData"))

# ============================================================================
# Step 1.2: Descriptive statistics
# ============================================================================
cat("\n--- Step 1.2: Descriptive statistics ---\n")

desc <- tibble(
  Statistic = c("N (valid changes)", "N (excluded, 0->positive)",
                "Minimum", "Maximum", "Mean", "Standard Deviation", "Skewness"),
  Value = c(
    n_valid,
    n_excluded,
    min(pooled_pct),
    max(pooled_pct),
    mean(pooled_pct),
    sd(pooled_pct),
    calc_skewness(pooled_pct)
  )
)

cat("\nDescriptive statistics:\n")
for (i in seq_len(nrow(desc))) {
  cat(sprintf("  %-30s %12.4f\n", desc$Statistic[i], desc$Value[i]))
}

# ============================================================================
# Step 1.3: Kurtosis and L-kurtosis
# ============================================================================
cat("\n--- Step 1.3: Kurtosis and L-kurtosis ---\n")

raw_kurtosis <- calc_kurtosis_raw(pooled_pct)       # raw (benchmark = 3)
excess_kurtosis <- calc_kurtosis_excess(pooled_pct)  # excess (benchmark = 0)

lmom_result <- calc_lmoments(pooled_pct)
l_kurtosis <- lmom_result$ratios["tau4"]
l_skewness <- lmom_result$ratios["tau3"]

cat(sprintf("  Raw kurtosis:    %.4f  (normal benchmark = 3)\n", raw_kurtosis))
cat(sprintf("  Excess kurtosis: %.4f  (normal benchmark = 0)\n", excess_kurtosis))
cat(sprintf("  L-kurtosis:      %.4f  (normal benchmark = 0.1226)\n", l_kurtosis))
cat(sprintf("  L-skewness:      %.4f\n", l_skewness))

# ============================================================================
# Step 1.4: Shapiro-Wilk test
# ============================================================================
cat("\n--- Step 1.4: Shapiro-Wilk test ---\n")

sw_test <- shapiro.test(pooled_pct)
cat(sprintf("  W statistic: %.6f\n", sw_test$statistic))
cat(sprintf("  p-value:     %.6e\n", sw_test$p.value))
if (sw_test$p.value < 0.05) {
  cat("  -> Reject normality at 5% level\n")
} else {
  cat("  -> Cannot reject normality at 5% level\n")
}

# ============================================================================
# Step 1.5: T-distribution fitting via MLE
# ============================================================================
cat("\n--- Step 1.5: T-distribution MLE fitting (Fernandez-i-Marin et al.) ---\n")
cat("  sigma^2 = incrementalism (higher = less incremental)\n")
cat("  nu = punctuation (lower = more extreme punctuations)\n\n")

t_fit <- fit_location_scale_t(pooled_pct, label = "aggregate", do_bootstrap = TRUE)

cat(sprintf("  Method:          %s\n", t_fit$method))
cat(sprintf("  Note:            %s\n", t_fit$note))
if (!is.na(t_fit$mu)) {
  cat(sprintf("  mu (location):   %.6f\n", t_fit$mu))
  cat(sprintf("  sigma (scale):   %.6f  [95%% CI: %.6f, %.6f]\n",
              t_fit$sigma, t_fit$sigma_ci_lo, t_fit$sigma_ci_hi))
  cat(sprintf("  sigma^2:         %.6f\n", t_fit$sigma_sq))
  cat(sprintf("  nu (df):         %.4f  [95%% CI: %.4f, %.4f]\n",
              t_fit$nu, t_fit$nu_ci_lo, t_fit$nu_ci_hi))
  cat(sprintf("  Theo. variance:  %s\n",
              if (is.infinite(t_fit$theo_var)) "Inf (nu <= 2)" else sprintf("%.6f", t_fit$theo_var)))
  cat(sprintf("  Interpretation:  %s\n", t_fit$interpretation))
  cat(sprintf("  Log-lik: %.4f  AIC: %.4f\n", t_fit$loglik, t_fit$aic))
}

# ============================================================================
# Step 1.6: Outputs
# ============================================================================
cat("\n--- Step 1.6: Generating outputs ---\n")

# Table 1: Descriptive statistics
table1 <- tibble(
  Statistic = c(
    "N (valid changes)", "N (excluded, 0->positive)",
    "Minimum", "Maximum", "Mean", "Standard Deviation",
    "Skewness",
    "Kurtosis (raw, benchmark=3)", "Excess Kurtosis (benchmark=0)",
    "L-kurtosis (tau_4, benchmark=0.123)",
    "Shapiro-Wilk W", "Shapiro-Wilk p-value",
    "t-fit: mu (location)",
    "t-fit: sigma (scale)", "t-fit: sigma^2",
    "t-fit: sigma 95% CI lo", "t-fit: sigma 95% CI hi",
    "t-fit: nu (df)",
    "t-fit: nu 95% CI lo", "t-fit: nu 95% CI hi",
    "t-fit: Theo. variance (sigma^2 * nu/(nu-2))",
    "t-fit: Interpretation",
    "t-fit: Log-likelihood", "t-fit: AIC"
  ),
  Value = as.character(c(
    n_valid, n_excluded,
    min(pooled_pct), max(pooled_pct), mean(pooled_pct), sd(pooled_pct),
    calc_skewness(pooled_pct),
    raw_kurtosis, excess_kurtosis,
    as.numeric(l_kurtosis),
    sw_test$statistic, sw_test$p.value,
    t_fit$mu,
    t_fit$sigma, t_fit$sigma_sq,
    t_fit$sigma_ci_lo, t_fit$sigma_ci_hi,
    t_fit$nu,
    t_fit$nu_ci_lo, t_fit$nu_ci_hi,
    ifelse(is.infinite(t_fit$theo_var), "Inf", t_fit$theo_var),
    t_fit$interpretation,
    t_fit$loglik, t_fit$aic
  )),
  Benchmark = c(
    NA, NA,
    NA, NA, NA, NA,
    "0 (symmetric)",
    "3 (normal)", "0 (normal)",
    "0.1226 (normal)",
    NA, "<0.05 rejects normality",
    NA,
    "Lower = more incremental", "Lower = more incremental",
    NA, NA,
    "<5: extreme punctuation; >30: normal", NA, NA,
    "Inf when nu<=2",
    NA,
    NA, NA
  )
)

table1_path <- file.path(tables_dir, "table1_h1a_descriptive.csv")
write_csv(table1, table1_path)
verify_output(table1_path)

# Figure 1: Histogram with normal and t overlays
x_range <- range(pooled_pct)
x_seq <- seq(x_range[1] - 0.5, x_range[2] + 0.5, length.out = 500)

# Normal density
norm_dens <- dnorm(x_seq, mean = mean(pooled_pct), sd = sd(pooled_pct))

# Fitted t density
if (!is.na(t_fit$mu)) {
  t_dens <- dlst(x_seq, mu = t_fit$mu, sigma = t_fit$sigma, nu = t_fit$nu)
} else {
  t_dens <- rep(NA, length(x_seq))
}

overlay_df <- tibble(x = x_seq, Normal = norm_dens, `Fitted t` = t_dens) %>%
  pivot_longer(-x, names_to = "Distribution", values_to = "density")

fig1 <- ggplot() +
  geom_histogram(
    data = tibble(pct = pooled_pct),
    aes(x = pct, y = after_stat(density)),
    bins = 25, fill = "grey70", color = "black", alpha = 0.7
  ) +
  geom_line(
    data = overlay_df %>% filter(Distribution == "Normal"),
    aes(x = x, y = density, color = Distribution, linetype = Distribution),
    linewidth = 1
  ) +
  geom_line(
    data = overlay_df %>% filter(Distribution == "Fitted t", !is.na(density)),
    aes(x = x, y = density, color = Distribution, linetype = Distribution),
    linewidth = 1
  ) +
  scale_color_manual(values = c("Normal" = "blue", "Fitted t" = "red")) +
  scale_linetype_manual(values = c("Normal" = "dashed", "Fitted t" = "solid")) +
  labs(
    x = "Annual Percentage Change",
    y = "Density",
    title = "Distribution of Annual Percentage Changes in ASEAN Legal Instruments",
    subtitle = sprintf(
      "N = %d valid changes | Blue dashed = Normal | Red solid = Fitted t (nu = %.2f)",
      n_valid, ifelse(is.na(t_fit$nu), NA, t_fit$nu)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

fig1_png <- file.path(figures_dir, "fig1_histogram_pct_changes.png")
fig1_pdf <- file.path(figures_dir, "fig1_histogram_pct_changes.pdf")

ggsave(fig1_png, fig1, width = 8, height = 5, dpi = 300, device = agg_png)
ggsave(fig1_pdf, fig1, width = 8, height = 5)

verify_output(fig1_png)
verify_output(fig1_pdf)

cat("\n02_h1a_aggregate.R completed.\n")
