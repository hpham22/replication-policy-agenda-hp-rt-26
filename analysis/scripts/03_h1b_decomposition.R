###############################################################################
# 03_h1b_decomposition.R
# Step 2: H1b — Core-Periphery Decomposition
###############################################################################

cat("\n=== 03_h1b_decomposition.R ===\n")

# Source setup if not already loaded
if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

# Load matrix data and pct changes
load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

# ============================================================================
# Step 2.1: Area-level descriptive statistics
# ============================================================================
cat("\n--- Step 2.1: Area-level descriptive statistics ---\n")

area_stats <- year_area_long %>%
  group_by(main_area) %>%
  arrange(year) %>%
  summarise(
    total_count  = sum(count),
    active_years = sum(count > 0),
    total_years  = n(),
    prop_active  = active_years / total_years,
    longest_gap  = compute_longest_gap(count),
    mean_annual  = mean(count),
    variance     = var(count),
    .groups = "drop"
  ) %>%
  mutate(
    category  = if_else(main_area %in% CORE_AREAS, "Core", "Peripheral"),
    area_name = AREA_LABELS[as.character(main_area)]
  )

# Within-area kurtosis of percentage changes (min 20 valid obs)
area_kurtosis <- all_area_pct %>%
  filter(!is.na(pct_change)) %>%
  group_by(main_area) %>%
  summarise(
    n_pct_valid = n(),
    within_kurtosis = if (n() >= 20) calc_kurtosis_raw(pct_change) else NA_real_,
    .groups = "drop"
  )

area_stats <- area_stats %>%
  left_join(area_kurtosis, by = "main_area") %>%
  arrange(category, desc(total_count))

cat("\nArea-level descriptive statistics:\n")
area_stats %>%
  select(category, main_area, area_name, total_count, active_years,
         prop_active, longest_gap, mean_annual, variance, within_kurtosis) %>%
  print(n = 16, width = 120)

# Save Table 2
table2 <- area_stats %>%
  select(
    Category     = category,
    Area_Code    = main_area,
    Area_Name    = area_name,
    Total_Count  = total_count,
    Active_Years = active_years,
    Prop_Active  = prop_active,
    Longest_Gap  = longest_gap,
    Mean_Annual  = mean_annual,
    Variance     = variance,
    N_Pct_Valid  = n_pct_valid,
    Within_Kurtosis = within_kurtosis
  )

table2_path <- file.path(tables_dir, "table2_area_descriptives.csv")
write_csv(table2, table2_path)
verify_output(table2_path)

# ============================================================================
# Step 2.2: Core vs. Peripheral distributional analysis
# ============================================================================
cat("\n--- Step 2.2: Core vs. Peripheral distributional analysis ---\n")

# Split pooled area-level pct changes by core/peripheral
core_pct <- all_area_pct %>%
  filter(main_area %in% CORE_AREAS, !is.na(pct_change)) %>%
  pull(pct_change)

periph_pct <- all_area_pct %>%
  filter(main_area %in% PERIPHERAL_AREAS, !is.na(pct_change)) %>%
  pull(pct_change)

cat("Core observations:      ", length(core_pct), "\n")
cat("Peripheral observations:", length(periph_pct), "\n")
cat("Total pooled:           ", length(pooled_pct), "\n")

# Function to compute all distributional stats for a group
compute_group_stats <- function(x, label) {
  n <- length(x)
  cat(sprintf("\n--- %s (n = %d) ---\n", label, n))

  # Descriptive
  cat(sprintf("  Mean: %.4f, SD: %.4f\n", mean(x), sd(x)))
  cat(sprintf("  Min: %.4f, Max: %.4f\n", min(x), max(x)))

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

  # T-distribution fit
  tfit <- fit_location_scale_t(x, label = label)
  if (!is.na(tfit$mu)) {
    cat(sprintf("  t-fit: mu=%.4f, sigma=%.4f, nu=%.4f (%s)\n",
                tfit$mu, tfit$sigma, tfit$nu, tfit$note))
  } else {
    cat(sprintf("  t-fit: FAILED (%s)\n", tfit$note))
  }

  tibble(
    Group = label,
    N = n,
    Mean = mean(x),
    SD = sd(x),
    Raw_Kurtosis = raw_kurt,
    Excess_Kurtosis = excess_kurt,
    L_Kurtosis = lkurt,
    Shapiro_Wilk_W = sw$statistic,
    Shapiro_Wilk_p = sw$p.value,
    t_mu = tfit$mu,
    t_sigma = tfit$sigma,
    t_nu = tfit$nu,
    t_se_mu = tfit$se_mu,
    t_se_sigma = tfit$se_sigma,
    t_se_nu = tfit$se_nu,
    t_loglik = tfit$loglik,
    t_method = tfit$method,
    t_note = tfit$note
  )
}

stats_aggregate  <- compute_group_stats(pooled_pct, "Aggregate")
stats_core       <- compute_group_stats(core_pct, "Core")
stats_peripheral <- compute_group_stats(periph_pct, "Peripheral")

table3 <- bind_rows(stats_aggregate, stats_core, stats_peripheral)

table3_path <- file.path(tables_dir, "table3_core_peripheral_comparison.csv")
write_csv(table3, table3_path)
verify_output(table3_path)

# ============================================================================
# Step 2.3: Variance decomposition
# ============================================================================
cat("\n--- Step 2.3: Variance decomposition ---\n")

# Total variance of all pooled pct changes
total_var <- var(pooled_pct)

# Within-group variances (pooled from area-level pct changes)
var_core <- var(core_pct)
var_periph <- var(periph_pct)

n_core <- length(core_pct)
n_periph <- length(periph_pct)
n_total <- n_core + n_periph

# Weighted within-group variance
weighted_within <- (n_core / n_total) * var_core + (n_periph / n_total) * var_periph

# Between-group variance
between_var <- total_var - weighted_within

cat(sprintf("  Total variance:           %.4f\n", total_var))
cat(sprintf("  Var(core):                %.4f  (weight = %.3f)\n", var_core, n_core / n_total))
cat(sprintf("  Var(peripheral):          %.4f  (weight = %.3f)\n", var_periph, n_periph / n_total))
cat(sprintf("  Weighted within-group:    %.4f  (%.1f%% of total)\n",
            weighted_within, 100 * weighted_within / total_var))
cat(sprintf("  Between-group:            %.4f  (%.1f%% of total)\n",
            between_var, 100 * between_var / total_var))

# Within-core and within-peripheral proportions
prop_within_core   <- (n_core / n_total) * var_core / total_var
prop_within_periph <- (n_periph / n_total) * var_periph / total_var
prop_between       <- between_var / total_var

cat(sprintf("\n  Proportion within-core:       %.4f (%.1f%%)\n",
            prop_within_core, 100 * prop_within_core))
cat(sprintf("  Proportion within-peripheral: %.4f (%.1f%%)\n",
            prop_within_periph, 100 * prop_within_periph))
cat(sprintf("  Proportion between-group:     %.4f (%.1f%%)\n",
            prop_between, 100 * prop_between))

# Save variance decomposition
var_decomp <- tibble(
  Component = c("Total", "Within-Core", "Within-Peripheral",
                "Weighted Within-Group", "Between-Group"),
  Variance = c(total_var, var_core, var_periph, weighted_within, between_var),
  Weight = c(NA, n_core / n_total, n_periph / n_total, NA, NA),
  Proportion_of_Total = c(1, prop_within_core, prop_within_periph,
                          weighted_within / total_var, prop_between),
  N = c(n_total, n_core, n_periph, n_total, n_total)
)

var_decomp_path <- file.path(tables_dir, "table3b_variance_decomposition.csv")
write_csv(var_decomp, var_decomp_path)
verify_output(var_decomp_path)

# ============================================================================
# Step 2.4: Persistence measures (lag-1 autocorrelation)
# ============================================================================
cat("\n--- Step 2.4: Persistence measures ---\n")

area_autocorr <- year_area_long %>%
  group_by(main_area) %>%
  arrange(year) %>%
  summarise(
    lag1_acf = {
      ts_vec <- count
      if (sd(ts_vec) == 0) NA_real_
      else acf(ts_vec, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
    },
    .groups = "drop"
  ) %>%
  mutate(
    category  = if_else(main_area %in% CORE_AREAS, "Core", "Peripheral"),
    area_name = AREA_LABELS[as.character(main_area)]
  ) %>%
  arrange(category, desc(lag1_acf))

cat("\nLag-1 autocorrelation by area:\n")
print(area_autocorr, n = 16)

# Group summaries
cat("\nMean lag-1 ACF by group:\n")
area_autocorr %>%
  group_by(category) %>%
  summarise(
    mean_acf = mean(lag1_acf, na.rm = TRUE),
    sd_acf   = sd(lag1_acf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ============================================================================
# Step 2.5: Shannon entropy and core-area share
# ============================================================================
cat("\n--- Step 2.5: Entropy and core-area share ---\n")

# Get area column names
area_cols <- paste0("area_", ALL_AREAS)
core_cols <- paste0("area_", CORE_AREAS)

entropy_series <- matrix_wide %>%
  rowwise() %>%
  mutate(
    shannon = compute_shannon(c_across(all_of(area_cols))),
    core_total = sum(c_across(all_of(core_cols))),
    core_share = if_else(total > 0, core_total / total, NA_real_)
  ) %>%
  ungroup()

cat("Entropy range:", round(range(entropy_series$shannon, na.rm = TRUE), 3), "\n")
cat("Max possible entropy: log(16) =", round(log(16), 3), "\n")

# Save entropy series for other scripts
save(entropy_series, area_autocorr, file = file.path(tables_dir, "entropy_data.RData"))

# Figure 2: Entropy and core-area share over time (dual panel)
p_entropy <- ggplot(
  entropy_series %>% filter(!is.na(shannon)),
  aes(x = year, y = shannon)
) +
  geom_line(color = "darkblue", linewidth = 0.7) +
  geom_point(color = "darkblue", size = 1.2) +
  geom_smooth(method = "loess", se = FALSE,
              color = "darkblue", linetype = "dashed",
              alpha = 0.5, linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  labs(x = NULL, y = "Shannon Entropy (H)",
       title = "Policy Area Diversity and Core Dominance Over Time",
       subtitle = "Lower panel: Shannon entropy | Upper panel: Core area share") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_core <- ggplot(
  entropy_series %>% filter(!is.na(core_share)),
  aes(x = year, y = core_share)
) +
  geom_line(color = "darkred", linewidth = 0.7) +
  geom_point(color = "darkred", size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey50") +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Year", y = "Core Area Share") +
  theme_minimal(base_size = 11)

fig2 <- p_core / p_entropy

fig2_png <- file.path(figures_dir, "fig2_entropy_core_share.png")
fig2_pdf <- file.path(figures_dir, "fig2_entropy_core_share.pdf")

ggsave(fig2_png, fig2, width = 9, height = 7, dpi = 300, device = agg_png)
ggsave(fig2_pdf, fig2, width = 9, height = 7)

verify_output(fig2_png)
verify_output(fig2_pdf)

cat("\n03_h1b_decomposition.R completed.\n")
