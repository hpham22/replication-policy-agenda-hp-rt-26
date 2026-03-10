###############################################################################
# 07_appendix_b_robustness.R
# Appendix B — Robustness checks
# Table B1 — Full vs restricted distribution comparison
# Table B2 — Percentage-percentage method comparison (Lundgren et al. 2018)
# Table B3 — Core threshold sensitivity
# Table B4 — One-year window sensitivity
# Table B5 — Period effects (pre/post Charter, cutoff ≤2008/≥2009)
# Table B6 — T-distribution fitting details
# Figure B1 — Pct-pct histogram
###############################################################################

cat("\n=== 07_appendix_b_robustness.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))
load(file.path(tables_dir, "tfit_aggregate.RData"))

core_cols <- paste0("area_", CORE_AREAS)

# ============================================================================
# Table B1 — Full vs restricted distribution comparison
# ============================================================================
cat("\n--- Table B1: Full vs restricted ---\n")

full_core_pct <- all_area_pct %>%
  filter(main_area %in% CORE_AREAS, !is.na(pct_change)) %>%
  pull(pct_change)
full_periph_pct <- all_area_pct %>%
  filter(!main_area %in% CORE_AREAS, !is.na(pct_change)) %>%
  pull(pct_change)

tableB1 <- tibble(
  Group = c("Aggregate", "Core", "Peripheral"),
  Distribution = rep("Full (incl 0-to-0)", 3),
  N = c(length(pooled_full), length(full_core_pct), length(full_periph_pct)),
  Mean = c(mean(pooled_full), mean(full_core_pct), mean(full_periph_pct)),
  SD = c(sd(pooled_full), sd(full_core_pct), sd(full_periph_pct)),
  Kurtosis = c(calc_kurtosis_raw(pooled_full),
               calc_kurtosis_raw(full_core_pct),
               calc_kurtosis_raw(full_periph_pct)),
  L_Kurtosis = c(as.numeric(calc_lkurtosis(pooled_full)),
                 as.numeric(calc_lkurtosis(full_core_pct)),
                 as.numeric(calc_lkurtosis(full_periph_pct))),
  Prop_Zeros = c(mean(pooled_full == 0),
                 mean(full_core_pct == 0),
                 mean(full_periph_pct == 0)),
  Variance = c(var(pooled_full), var(full_core_pct), var(full_periph_pct))
)

cat("\nTable B1:\n")
print(tableB1, width = 120)

tableB1_path <- file.path(tables_dir, "tableB1_full_vs_restricted.csv")
write_csv(tableB1, tableB1_path)
verify_output(tableB1_path)

tableB1_display <- tableB1 %>%
  mutate(across(where(is.numeric) & !matches("^N$"), ~ round(., 4)))
save_table(
  tableB1_display, "tableB1_full_vs_restricted",
  caption = "Table B1. Full Distribution Statistics (Including 0-to-0 Transitions)",
  footnote = "Full distribution includes 642 zero-to-zero transitions. Compare with restricted distribution in Table 2.",
  col_names = c("Group", "Distribution", "N", "Mean", "SD",
                "Kurtosis", "L-kurtosis", "Prop. Zeros", "Variance")
)

# ============================================================================
# Table B2 — Percentage-percentage method (Lundgren et al. 2018)
# ============================================================================
cat("\n--- Table B2: Pct-pct method ---\n")

# Construct share matrix
count_mat <- as.matrix(matrix_wide[, area_cols])
rownames(count_mat) <- matrix_wide$year
row_totals <- matrix_wide$total

share_mat <- count_mat
for (i in seq_len(nrow(count_mat))) {
  if (row_totals[i] == 0) {
    share_mat[i, ] <- NA
  } else {
    share_mat[i, ] <- count_mat[i, ] / row_totals[i]
  }
}

# First differences
n_years <- nrow(share_mat)
diff_mat <- share_mat[-1, ] - share_mat[-n_years, ]
valid_pairs <- complete.cases(share_mat[-n_years, ]) & complete.cases(share_mat[-1, ])

diff_valid <- diff_mat[valid_pairs, ]
pooled_pctpct <- as.vector(diff_valid)

cat(sprintf("Valid pairs: %d | Pooled N: %d\n", sum(valid_pairs), length(pooled_pctpct)))

# Core/peripheral splits
core_idx <- which(area_cols %in% paste0("area_", CORE_AREAS))
periph_idx <- which(!area_cols %in% paste0("area_", CORE_AREAS))
core_pctpct <- as.vector(diff_valid[, core_idx])
periph_pctpct <- as.vector(diff_valid[, periph_idx])

# Helper
fmt4 <- function(x) formatC(x, format = "f", digits = 4)
fmt6 <- function(x) formatC(x, format = "f", digits = 6)

compute_pp_stats <- function(x) {
  sw <- shapiro.test(x)
  list(N = length(x), Mean = mean(x), SD = sd(x),
       Kurtosis = calc_kurtosis_raw(x),
       L_Kurtosis = as.numeric(calc_lkurtosis(x)),
       SW_W = unname(sw$statistic), SW_p = sw$p.value,
       Prop_Zeros = mean(x == 0))
}

pp_agg <- compute_pp_stats(pooled_pctpct)
pp_core <- compute_pp_stats(core_pctpct)
pp_periph <- compute_pp_stats(periph_pctpct)

# Also get restricted pct-count stats for comparison
restr_sw <- shapiro.test(pooled_restricted)
restr_stats <- list(
  N = length(pooled_restricted), Mean = mean(pooled_restricted),
  SD = sd(pooled_restricted), Kurtosis = calc_kurtosis_raw(pooled_restricted),
  L_Kurtosis = as.numeric(calc_lkurtosis(pooled_restricted)),
  SW_W = unname(restr_sw$statistic), SW_p = restr_sw$p.value,
  Prop_Zeros = mean(pooled_restricted == 0)
)

tableB2 <- tibble(
  Metric = c("N", "Mean", "SD",
             "Raw Kurtosis (benchmark=3)",
             "L-kurtosis (benchmark=0.1226)",
             "Shapiro-Wilk W", "Shapiro-Wilk p",
             "Proportion zeros"),
  `Pct-Count (restricted)` = c(
    as.character(restr_stats$N), fmt6(restr_stats$Mean), fmt4(restr_stats$SD),
    fmt4(restr_stats$Kurtosis), fmt4(restr_stats$L_Kurtosis),
    fmt6(restr_stats$SW_W), formatC(restr_stats$SW_p, format = "e", digits = 4),
    fmt4(restr_stats$Prop_Zeros)),
  `Pct-Pct Aggregate` = c(
    as.character(pp_agg$N), fmt6(pp_agg$Mean), fmt4(pp_agg$SD),
    fmt4(pp_agg$Kurtosis), fmt4(pp_agg$L_Kurtosis),
    fmt6(pp_agg$SW_W), formatC(pp_agg$SW_p, format = "e", digits = 4),
    fmt4(pp_agg$Prop_Zeros)),
  `Pct-Pct Core` = c(
    as.character(pp_core$N), fmt6(pp_core$Mean), fmt4(pp_core$SD),
    fmt4(pp_core$Kurtosis), fmt4(pp_core$L_Kurtosis),
    fmt6(pp_core$SW_W), formatC(pp_core$SW_p, format = "e", digits = 4),
    fmt4(pp_core$Prop_Zeros)),
  `Pct-Pct Peripheral` = c(
    as.character(pp_periph$N), fmt6(pp_periph$Mean), fmt4(pp_periph$SD),
    fmt4(pp_periph$Kurtosis), fmt4(pp_periph$L_Kurtosis),
    fmt6(pp_periph$SW_W), formatC(pp_periph$SW_p, format = "e", digits = 4),
    fmt4(pp_periph$Prop_Zeros)),
  `Lundgren et al. range` = c(
    "", "", "", "",
    "EU=0.26, OAS=0.26, UN=0.28, AU=0.30, OIC=0.31",
    "", "", "")
)

cat("\nTable B2:\n")
print(tableB2, width = 140)

tableB2_path <- file.path(tables_dir, "tableB2_pctpct_comparison.csv")
write_csv(tableB2, tableB2_path)
verify_output(tableB2_path)

save_table(
  tableB2, "tableB2_pctpct_comparison",
  caption = "Table B2. Percentage-Percentage Method Comparison (Lundgren et al. 2018)",
  footnote = "Pct-pct method uses first differences of proportional shares. Lundgren et al. (2018) L-kurtosis values for comparison IOs."
)

# ============================================================================
# Table B3 — Core threshold sensitivity
# ============================================================================
cat("\n--- Table B3: Core threshold sensitivity ---\n")

area_totals_df <- df %>%
  count(main_area, name = "total") %>%
  arrange(desc(total))

thresholds <- list(
  "Top-2 (15, 20)"               = c(15, 20),
  "Baseline Top-3 (10, 15, 20)"  = c(10, 15, 20),
  "Top-4 (4, 10, 15, 20)"        = c(4, 10, 15, 20)
)

tableB3 <- map_dfr(names(thresholds), function(tname) {
  core_set <- thresholds[[tname]]
  core_ch <- all_area_pct %>%
    filter(main_area %in% core_set, !is.na(pct_change), status != "zero_to_zero") %>%
    pull(pct_change)
  periph_ch <- all_area_pct %>%
    filter(!main_area %in% core_set, !is.na(pct_change), status != "zero_to_zero") %>%
    pull(pct_change)
  core_inst_share <- sum(area_totals_df$total[area_totals_df$main_area %in% core_set]) /
    sum(area_totals_df$total) * 100
  tibble(
    Threshold = tname, N_Core_Areas = length(core_set),
    Core_Instrument_Share = round(core_inst_share, 1),
    Core_N = length(core_ch), Periph_N = length(periph_ch),
    Core_Kurtosis = calc_kurtosis_raw(core_ch),
    Periph_Kurtosis = calc_kurtosis_raw(periph_ch),
    Core_LKurtosis = as.numeric(calc_lkurtosis(core_ch)),
    Periph_LKurtosis = as.numeric(calc_lkurtosis(periph_ch)),
    Core_Variance = var(core_ch), Periph_Variance = var(periph_ch),
    Variance_Ratio = var(core_ch) / var(periph_ch)
  )
})

cat("\nTable B3:\n")
print(tableB3, width = 120)

tableB3_path <- file.path(tables_dir, "tableB3_core_threshold.csv")
write_csv(tableB3, tableB3_path)
verify_output(tableB3_path)

tableB3_display <- tableB3 %>%
  mutate(across(where(is.numeric) & !matches("^(N_|Core_N|Periph_N|Core_Instrument)"),
                ~ round(., 4)))
save_table(
  tableB3_display, "tableB3_core_threshold",
  caption = "Table B3. Core Threshold Sensitivity Analysis",
  footnote = "Restricted distribution. Baseline uses top-3 areas by instrument count (10, 15, 20).",
  col_names = c("Threshold", "N Areas", "Inst. Share (%)",
                "Core N", "Periph N", "Core Kurt.", "Periph Kurt.",
                "Core L-kurt.", "Periph L-kurt.",
                "Core Var.", "Periph Var.", "Var. Ratio")
)

# ============================================================================
# Table B4 — One-year window sensitivity
# ============================================================================
cat("\n--- Table B4: One-year windows ---\n")

classify_1yr <- function(y) {
  if (y %in% ZERO_YEARS) return("zero_output")
  if (y %in% CRISIS_1YR) return("crisis")
  if (y %in% MILESTONE_1YR) return("milestone")
  return("normal")
}

year_profiles_1yr <- matrix_wide %>%
  filter(total > 0) %>%
  rowwise() %>%
  mutate(
    active_areas  = sum(c_across(all_of(area_cols)) > 0),
    hhi           = compute_hhi(c_across(all_of(area_cols))),
    shannon       = compute_shannon(c_across(all_of(area_cols))),
    core_total    = sum(c_across(all_of(core_cols))),
    core_prop     = if_else(total > 0, core_total / total, NA_real_),
    periph_prop   = if_else(total > 0, 1 - core_prop, NA_real_),
    year_type_1yr = classify_1yr(year)
  ) %>%
  ungroup() %>%
  filter(year_type_1yr != "zero_output")

tableB4 <- year_profiles_1yr %>%
  group_by(year_type_1yr) %>%
  summarise(
    n_years        = n(),
    mean_total     = mean(total),     sd_total     = sd(total),
    mean_active    = mean(active_areas), sd_active  = sd(active_areas),
    mean_hhi       = mean(hhi, na.rm = TRUE),  sd_hhi = sd(hhi, na.rm = TRUE),
    mean_core_prop = mean(core_prop, na.rm = TRUE),
    sd_core_prop   = sd(core_prop, na.rm = TRUE),
    mean_periph_prop = mean(periph_prop, na.rm = TRUE),
    sd_periph_prop   = sd(periph_prop, na.rm = TRUE),
    mean_entropy   = mean(shannon, na.rm = TRUE),
    sd_entropy     = sd(shannon, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nTable B4:\n")
print(tableB4, width = 120)

tableB4_path <- file.path(tables_dir, "tableB4_1yr_windows.csv")
write_csv(tableB4, tableB4_path)
verify_output(tableB4_path)

tableB4_display <- tableB4 %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  mutate(
    `Output (mean ± SD)` = sprintf("%.1f ± %.1f", mean_total, sd_total),
    `Active (mean ± SD)` = sprintf("%.1f ± %.1f", mean_active, sd_active),
    `HHI (mean ± SD)` = sprintf("%.3f ± %.3f", mean_hhi, sd_hhi),
    `Core Share (mean ± SD)` = sprintf("%.3f ± %.3f", mean_core_prop, sd_core_prop),
    `Entropy (mean ± SD)` = sprintf("%.3f ± %.3f", mean_entropy, sd_entropy)
  ) %>%
  select(Year_Type = year_type_1yr, N = n_years,
         `Output (mean ± SD)`, `Active (mean ± SD)`,
         `HHI (mean ± SD)`, `Core Share (mean ± SD)`,
         `Entropy (mean ± SD)`)

save_table(
  tableB4_display, "tableB4_1yr_windows",
  caption = "Table B4. One-Year Window Sensitivity Analysis",
  footnote = "One-year windows: crisis = {1997, 2004, 2009, 2020}, milestone = {1976, 1992, 2007, 2015}. Compare with two-year windows in Table 5."
)

# ============================================================================
# Table B5 — Period effects (pre/post Charter, cutoff ≤2008/≥2009)
# ============================================================================
cat("\n--- Table B5: Period effects ---\n")

pre_charter <- all_area_pct %>%
  filter(year_to <= 2008, !is.na(pct_change)) %>%
  pull(pct_change)
post_charter <- all_area_pct %>%
  filter(year_to >= 2009, !is.na(pct_change)) %>%
  pull(pct_change)

pre_restr <- all_area_pct %>%
  filter(year_to <= 2008, !is.na(pct_change), status != "zero_to_zero") %>%
  pull(pct_change)
post_restr <- all_area_pct %>%
  filter(year_to >= 2009, !is.na(pct_change), status != "zero_to_zero") %>%
  pull(pct_change)

cat(sprintf("  Pre-Charter (<=2008): n_full=%d, n_restr=%d\n",
            length(pre_charter), length(pre_restr)))
cat(sprintf("  Post-Charter (>=2009): n_full=%d, n_restr=%d\n",
            length(post_charter), length(post_restr)))

tfit_pre <- fit_location_scale_t(pre_restr, "pre-Charter restricted")
tfit_post <- fit_location_scale_t(post_restr, "post-Charter restricted")

tableB5 <- tibble(
  Period = c("Pre-Charter (<=2008)", "Post-Charter (>=2009)",
             "Pre-Charter restricted", "Post-Charter restricted"),
  N = c(length(pre_charter), length(post_charter),
        length(pre_restr), length(post_restr)),
  Mean = c(mean(pre_charter), mean(post_charter),
           mean(pre_restr), mean(post_restr)),
  SD = c(sd(pre_charter), sd(post_charter),
         sd(pre_restr), sd(post_restr)),
  Kurtosis = c(calc_kurtosis_raw(pre_charter), calc_kurtosis_raw(post_charter),
               calc_kurtosis_raw(pre_restr), calc_kurtosis_raw(post_restr)),
  L_Kurtosis = c(
    as.numeric(calc_lkurtosis(pre_charter)),
    as.numeric(calc_lkurtosis(post_charter)),
    as.numeric(calc_lkurtosis(pre_restr)),
    as.numeric(calc_lkurtosis(post_restr))
  ),
  Prop_Zeros = c(mean(pre_charter == 0), mean(post_charter == 0),
                 mean(pre_restr == 0), mean(post_restr == 0)),
  Prop_Neg1 = c(mean(pre_charter == -1), mean(post_charter == -1),
                mean(pre_restr == -1), mean(post_restr == -1)),
  t_mu    = c(NA, NA, tfit_pre$mu, tfit_post$mu),
  t_sigma = c(NA, NA, tfit_pre$sigma, tfit_post$sigma),
  t_nu    = c(NA, NA, tfit_pre$nu, tfit_post$nu),
  t_note  = c("", "", tfit_pre$note, tfit_post$note)
)

cat("\nTable B5:\n")
print(tableB5, width = 120)

tableB5_path <- file.path(tables_dir, "tableB5_period_effects.csv")
write_csv(tableB5, tableB5_path)
verify_output(tableB5_path)

tableB5_display <- tableB5 %>%
  mutate(across(where(is.numeric), ~ round(., 4)))
save_table(
  tableB5_display, "tableB5_period_effects",
  caption = "Table B5. Period Effects: Pre- vs. Post-Charter",
  footnote = "Cutoff: pre-Charter ≤2008 / post-Charter ≥2009. ASEAN Charter signed Nov 2007, entered force Dec 2008.",
  col_names = c("Period", "N", "Mean", "SD", "Kurtosis", "L-kurtosis",
                "Prop. Zeros", "Prop. -1", "t: mu", "t: sigma", "t: nu", "Note"),
  pack_rows = list(
    list(label = "Full Distribution", start = 1, end = 2),
    list(label = "Restricted Distribution", start = 3, end = 4)
  )
)

# ============================================================================
# Table B6 — T-distribution fitting details
# ============================================================================
cat("\n--- Table B6: T-distribution fitting details ---\n")

fmt <- function(x, d = 4) {
  if (is.na(x)) return("NA")
  if (is.infinite(x)) return("Inf")
  formatC(x, format = "f", digits = d)
}

fmt_se <- function(val, se, d = 4) {
  if (is.na(val)) return("NA")
  sprintf("%s (%s)", fmt(val, d), fmt(se, d))
}

# Collect all t-fits: aggregate restricted + full, plus period splits
fits <- list(
  list(label = "Aggregate (restricted)", fit = tfit_restr),
  list(label = "Aggregate (full)",       fit = tfit_full),
  list(label = "Pre-Charter restricted", fit = tfit_pre),
  list(label = "Post-Charter restricted", fit = tfit_post)
)

tableB6 <- map_dfr(fits, function(f) {
  fit <- f$fit
  tibble(
    Distribution = f$label,
    `mu (SE)` = fmt_se(fit$mu, fit$se_mu, 6),
    `sigma (SE)` = fmt_se(fit$sigma, fit$se_sigma, 6),
    `sigma^2` = fmt(fit$sigma_sq, 6),
    `nu (SE)` = fmt_se(fit$nu, fit$se_nu),
    `Theo. Variance` = if (is.na(fit$theo_var)) "NA"
                       else if (is.infinite(fit$theo_var)) "Inf"
                       else fmt(fit$theo_var),
    AIC = fmt(fit$aic, 2),
    BIC = fmt(fit$bic, 2),
    LogLik = fmt(fit$loglik, 2),
    Method = fit$method,
    Note = fit$note
  )
})

cat("\nTable B6:\n")
print(tableB6, width = 140)

tableB6_path <- file.path(tables_dir, "tableB6_tfit_details.csv")
write_csv(tableB6, tableB6_path)
verify_output(tableB6_path)

save_table(
  tableB6, "tableB6_tfit_details",
  caption = "Table B6. Location-Scale t-Distribution Fitting Details",
  footnote = "MLE via fitdistrplus. Degenerate fits (sigma near 0) reflect discrete point masses at 0 and -1. Jittered sensitivity checks reported in Note.",
  col_names = c("Distribution", "mu (SE)", "sigma (SE)", "sigma²",
                "nu (SE)", "Theo. Var.", "AIC", "BIC", "LogLik", "Method", "Note")
)

# ============================================================================
# Figure B1 — Pct-pct histogram
# ============================================================================
cat("\n--- Figure B1: Pct-pct histogram ---\n")

hist_data_pp <- tibble(pct = pooled_pctpct)
x_range_pp <- seq(min(pooled_pctpct) - 0.05, max(pooled_pctpct) + 0.05, length.out = 500)

normal_overlay_pp <- tibble(
  x = x_range_pp,
  density = dnorm(x_range_pp, mean = mean(pooled_pctpct), sd = sd(pooled_pctpct))
)

annot_pp <- sprintf(
  "N = %d\nKurtosis = %.1f\nL-kurtosis = %.4f",
  pp_agg$N, pp_agg$Kurtosis, pp_agg$L_Kurtosis
)

figB1 <- ggplot() +
  geom_histogram(data = hist_data_pp,
                 aes(x = pct, y = after_stat(density)),
                 bins = 50, fill = "grey40", colour = "white",
                 linewidth = 0.2) +
  geom_line(data = normal_overlay_pp,
            aes(x = x, y = density),
            linetype = "dashed", colour = "black", linewidth = 0.8) +
  annotate("text", x = max(pooled_pctpct) * 0.5, y = Inf,
           label = annot_pp, hjust = 0, vjust = 1.5,
           size = 4, family = "serif") +
  labs(x = "First difference in proportional share", y = "Density") +
  theme_pub()

save_figure(figB1, "figB1_pctpct_histogram", width = 6.5, height = 4.5)

cat("\n07_appendix_b_robustness.R completed.\n")
