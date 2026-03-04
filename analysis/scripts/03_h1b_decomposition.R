###############################################################################
# 03_h1b_decomposition.R
# Step 4: H1b — Core-Periphery Decomposition
###############################################################################

cat("\n=== 03_h1b_decomposition.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

core_cols <- paste0("area_", CORE_AREAS)

# ============================================================================
# 4.1: Table 2 — Area-level descriptive statistics
# ============================================================================
cat("\n--- 4.1: Area-level descriptives ---\n")

# Basic area statistics from annual counts
area_stats <- year_area_long %>%
  group_by(main_area) %>%
  arrange(year) %>%
  summarise(
    total_count  = sum(count),
    active_years = sum(count > 0),
    prop_active  = active_years / n(),
    longest_gap  = compute_longest_gap(count),
    mean_annual  = mean(count),
    within_var   = var(count),
    lag1_acf     = {
      ts_vec <- count
      if (sd(ts_vec) == 0) NA_real_
      else acf(ts_vec, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
    },
    .groups = "drop"
  ) %>%
  mutate(
    share_pct = total_count / sum(total_count) * 100,
    category  = if_else(main_area %in% CORE_AREAS, "Core", "Peripheral"),
    area_name = AREA_LABELS[as.character(main_area)]
  )

# Within-area kurtosis of percentage changes (min 15 valid obs)
area_kurtosis <- all_area_pct %>%
  filter(!is.na(pct_change)) %>%
  group_by(main_area) %>%
  summarise(
    n_pct_valid     = n(),
    within_kurtosis = if (n() >= 15) calc_kurtosis_raw(pct_change) else NA_real_,
    .groups = "drop"
  )

area_stats <- area_stats %>%
  left_join(area_kurtosis, by = "main_area") %>%
  arrange(category, desc(total_count))

# Build table with subtotals
core_rows <- area_stats %>% filter(category == "Core")
periph_rows <- area_stats %>% filter(category == "Peripheral")

core_subtotal <- tibble(
  main_area = NA_integer_, category = "Core", area_name = "SUBTOTAL",
  total_count = sum(core_rows$total_count),
  share_pct = sum(core_rows$share_pct),
  active_years = NA_integer_, prop_active = NA_real_,
  longest_gap = NA_integer_, mean_annual = mean(core_rows$mean_annual),
  within_var = NA_real_, lag1_acf = mean(core_rows$lag1_acf, na.rm = TRUE),
  n_pct_valid = sum(core_rows$n_pct_valid, na.rm = TRUE),
  within_kurtosis = NA_real_
)

periph_subtotal <- tibble(
  main_area = NA_integer_, category = "Peripheral", area_name = "SUBTOTAL",
  total_count = sum(periph_rows$total_count),
  share_pct = sum(periph_rows$share_pct),
  active_years = NA_integer_, prop_active = NA_real_,
  longest_gap = NA_integer_, mean_annual = mean(periph_rows$mean_annual),
  within_var = NA_real_, lag1_acf = mean(periph_rows$lag1_acf, na.rm = TRUE),
  n_pct_valid = sum(periph_rows$n_pct_valid, na.rm = TRUE),
  within_kurtosis = NA_real_
)

table2 <- bind_rows(core_rows, core_subtotal, periph_rows, periph_subtotal) %>%
  select(Category = category, Area_Code = main_area, Area_Name = area_name,
         Total_Count = total_count, Share_Pct = share_pct,
         Active_Years = active_years, Prop_Active = prop_active,
         Longest_Gap = longest_gap, Mean_Annual = mean_annual,
         Within_Variance = within_var,
         Within_Kurtosis = within_kurtosis,
         Lag1_ACF = lag1_acf)

table2_path <- file.path(tables_dir, "table2_area_descriptives.csv")
write_csv(table2, table2_path)
verify_output(table2_path)

cat("\nArea descriptives:\n")
print(table2 %>% select(Category, Area_Name, Total_Count, Share_Pct,
                          Active_Years, Within_Kurtosis, Lag1_ACF),
      n = 20, width = 120)

# ============================================================================
# 4.2: Table 3 — Distributional comparison (Aggregate/Core/Peripheral)
# ============================================================================
cat("\n--- 4.2: Distributional comparison ---\n")

core_pct <- all_area_pct %>%
  filter(main_area %in% CORE_AREAS, !is.na(pct_change)) %>%
  pull(pct_change)

periph_pct <- all_area_pct %>%
  filter(main_area %in% PERIPHERAL_AREAS, !is.na(pct_change)) %>%
  pull(pct_change)

cat("Core observations:      ", length(core_pct), "\n")
cat("Peripheral observations:", length(periph_pct), "\n")
cat("Total pooled:           ", length(pooled_full), "\n")

compute_group_stats <- function(x, label) {
  n <- length(x)
  cat(sprintf("\n  --- %s (n = %d) ---\n", label, n))

  raw_kurt <- calc_kurtosis_raw(x)
  excess_kurt <- calc_kurtosis_excess(x)
  lm_result <- calc_lmoments(x)
  lkurt <- as.numeric(lm_result$ratios["tau4"])
  sw <- shapiro.test(x)
  tfit <- fit_location_scale_t(x, label = label)

  cat(sprintf("  Kurtosis: %.4f | L-kurtosis: %.4f\n", raw_kurt, lkurt))
  cat(sprintf("  Shapiro-Wilk: W=%.6f, p=%.2e\n", sw$statistic, sw$p.value))
  if (!is.na(tfit$mu)) {
    cat(sprintf("  t-fit: mu=%.6f, sigma=%.6f, nu=%.4f (%s)\n",
                tfit$mu, tfit$sigma, tfit$nu, tfit$note))
  }

  list(
    N = n, Mean = mean(x), SD = sd(x),
    Skewness = calc_skewness(x),
    Raw_Kurtosis = raw_kurt, Excess_Kurtosis = excess_kurt,
    L_Kurtosis = lkurt,
    SW_W = unname(sw$statistic), SW_p = sw$p.value,
    t_mu = tfit$mu, t_mu_se = tfit$se_mu,
    t_sigma = tfit$sigma, t_sigma_se = tfit$se_sigma,
    t_sigma_sq = tfit$sigma_sq,
    t_nu = tfit$nu, t_nu_se = tfit$se_nu,
    t_theo_var = tfit$theo_var,
    t_aic = tfit$aic, t_note = tfit$note
  )
}

stats_agg    <- compute_group_stats(pooled_full, "Aggregate")
stats_core   <- compute_group_stats(core_pct, "Core")
stats_periph <- compute_group_stats(periph_pct, "Peripheral")

# Build Table 3 in column format (Metric | Aggregate | Core | Peripheral)
fmt <- function(x, d = 4) {
  if (is.na(x)) return("NA")
  if (is.infinite(x)) return("Inf")
  formatC(x, format = "f", digits = d)
}

fmt_se <- function(val, se, d = 4) {
  if (is.na(val)) return("NA")
  sprintf("%s (%s)", fmt(val, d), fmt(se, d))
}

metric_names <- c(
  "N", "Mean", "SD", "Skewness",
  "Raw Kurtosis (benchmark=3)",
  "L-kurtosis (tau_4, benchmark=0.123)",
  "Shapiro-Wilk W", "Shapiro-Wilk p",
  "t-fit: mu (SE)", "t-fit: sigma (SE)", "t-fit: sigma^2",
  "t-fit: nu (SE)", "t-fit: Theo. variance", "t-fit: AIC", "t-fit: Note"
)

extract_col <- function(s) {
  c(
    as.character(s$N), fmt(s$Mean), fmt(s$SD), fmt(s$Skewness),
    fmt(s$Raw_Kurtosis), fmt(s$L_Kurtosis),
    fmt(s$SW_W, 6), formatC(s$SW_p, format = "e", digits = 4),
    fmt_se(s$t_mu, s$t_mu_se, 6), fmt_se(s$t_sigma, s$t_sigma_se, 6),
    fmt(s$t_sigma_sq, 6),
    fmt_se(s$t_nu, s$t_nu_se),
    if (is.infinite(s$t_theo_var)) "Inf" else fmt(s$t_theo_var),
    fmt(s$t_aic, 2),
    s$t_note
  )
}

table3 <- tibble(
  Metric     = metric_names,
  Aggregate  = extract_col(stats_agg),
  Core       = extract_col(stats_core),
  Peripheral = extract_col(stats_periph)
)

table3_path <- file.path(tables_dir, "table3_distributional_comparison.csv")
write_csv(table3, table3_path)
verify_output(table3_path)

# ============================================================================
# 4.3: Variance decomposition
# ============================================================================
cat("\n--- 4.3: Variance decomposition ---\n")

total_var <- var(pooled_full)
var_core <- var(core_pct)
var_periph <- var(periph_pct)
n_core <- length(core_pct)
n_periph <- length(periph_pct)
n_total <- n_core + n_periph

weighted_within <- (n_core / n_total) * var_core + (n_periph / n_total) * var_periph
between_var <- total_var - weighted_within

prop_within_core   <- (n_core / n_total) * var_core / total_var
prop_within_periph <- (n_periph / n_total) * var_periph / total_var
prop_between       <- between_var / total_var

cat(sprintf("  Total variance:       %.4f\n", total_var))
cat(sprintf("  Within-core:          %.4f (%.1f%%)\n",
            var_core, 100 * prop_within_core))
cat(sprintf("  Within-peripheral:    %.4f (%.1f%%)\n",
            var_periph, 100 * prop_within_periph))
cat(sprintf("  Between-group:        %.4f (%.1f%%)\n",
            between_var, 100 * prop_between))

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
# 4.4: Persistence measures
# ============================================================================
cat("\n--- 4.4: Persistence measures ---\n")

cat("Mean lag-1 ACF by group:\n")
area_stats %>%
  filter(!is.na(lag1_acf), area_name != "SUBTOTAL") %>%
  group_by(category) %>%
  summarise(mean_acf = mean(lag1_acf, na.rm = TRUE),
            sd_acf = sd(lag1_acf, na.rm = TRUE),
            .groups = "drop") %>%
  print()

# ============================================================================
# 4.5: Entropy and core-area share
# ============================================================================
cat("\n--- 4.5: Entropy and core-area share ---\n")

entropy_series <- matrix_wide %>%
  rowwise() %>%
  mutate(
    shannon    = compute_shannon(c_across(all_of(area_cols))),
    core_total = sum(c_across(all_of(core_cols))),
    core_share = if_else(total > 0, core_total / total, NA_real_)
  ) %>%
  ungroup()

cat("Entropy range:", round(range(entropy_series$shannon, na.rm = TRUE), 3), "\n")
cat("Max possible entropy: log(16) =", round(log(16), 3), "\n")

save(entropy_series, file = file.path(tables_dir, "entropy_data.RData"))

# ============================================================================
# 4.6: Figure 4 — Core-area share and entropy (two-panel, grayscale)
# ============================================================================
cat("\n--- 4.6: Figure 4 ---\n")

# Event annotations (same as Figure 2)
crisis_bands <- tibble(
  xmin = c(1996.5, 2003.5, 2008.5, 2019.5),
  xmax = c(1998.5, 2005.5, 2009.5, 2020.5)
)
milestone_xs <- c(1976, 1992, 2007, 2015)

p_core <- ggplot() +
  geom_rect(data = crisis_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.7) +
  geom_vline(xintercept = milestone_xs, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_line(data = entropy_series %>% filter(!is.na(core_share)),
            aes(x = year, y = core_share),
            colour = "black", linewidth = 0.6) +
  geom_point(data = entropy_series %>% filter(!is.na(core_share)),
             aes(x = year, y = core_share),
             colour = "black", size = 1.2) +
  geom_hline(yintercept = 0.75, linetype = "dotted", colour = "grey50") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(x = NULL, y = "Core Area Share",
       title = "Policy Concentration and Agenda Breadth Over Time",
       subtitle = "Panel A: Core-area share | Panel B: Shannon entropy") +
  theme_pub()

p_entropy <- ggplot() +
  geom_rect(data = crisis_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.7) +
  geom_vline(xintercept = milestone_xs, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_line(data = entropy_series %>% filter(!is.na(shannon)),
            aes(x = year, y = shannon),
            colour = "black", linewidth = 0.6) +
  geom_point(data = entropy_series %>% filter(!is.na(shannon)),
             aes(x = year, y = shannon),
             colour = "black", size = 1.2) +
  geom_smooth(data = entropy_series %>% filter(!is.na(shannon)),
              aes(x = year, y = shannon),
              method = "loess", se = FALSE,
              colour = "grey50", linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  labs(x = "Year", y = "Shannon Entropy (H)") +
  theme_pub()

fig4 <- p_core / p_entropy

save_figure(fig4, "fig4_core_entropy_timeseries", width = 9, height = 7)

cat("\n03_h1b_decomposition.R completed.\n")
