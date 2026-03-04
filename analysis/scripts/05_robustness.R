###############################################################################
# 05_robustness.R
# Step 6: Robustness checks — alternative core thresholds, period effects,
#         and one-year window sensitivity
###############################################################################

cat("\n=== 05_robustness.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

# ============================================================================
# 6.1: Alternative core-periphery thresholds
# ============================================================================
cat("\n--- 6.1: Alternative core thresholds ---\n")

# Determine top areas by instrument count
area_totals <- df %>%
  count(main_area, name = "total") %>%
  arrange(desc(total))
cat("Area ranking by count:\n")
print(area_totals, n = 6)

# Define alternative thresholds
thresholds <- list(
  "Top-2 (15, 20)"          = c(15, 20),
  "Baseline Top-3 (10, 15, 20)" = c(10, 15, 20),
  "Top-4 (4, 10, 15, 20)"  = c(4, 10, 15, 20)
)

sensitivity_core <- map_dfr(names(thresholds), function(tname) {
  core_set <- thresholds[[tname]]

  core_changes <- all_area_pct %>%
    filter(main_area %in% core_set, !is.na(pct_change)) %>%
    pull(pct_change)
  periph_changes <- all_area_pct %>%
    filter(!main_area %in% core_set, !is.na(pct_change)) %>%
    pull(pct_change)

  # Core share of total instruments
  core_instrument_share <- sum(area_totals$total[area_totals$main_area %in% core_set]) /
    sum(area_totals$total) * 100

  tibble(
    Threshold = tname,
    N_Core_Areas = length(core_set),
    Core_Instrument_Share = round(core_instrument_share, 1),
    Core_N = length(core_changes),
    Periph_N = length(periph_changes),
    Core_Kurtosis = calc_kurtosis_raw(core_changes),
    Periph_Kurtosis = calc_kurtosis_raw(periph_changes),
    Core_LKurtosis = as.numeric(calc_lkurtosis(core_changes)),
    Periph_LKurtosis = as.numeric(calc_lkurtosis(periph_changes)),
    Core_Variance = var(core_changes),
    Periph_Variance = var(periph_changes),
    Variance_Ratio = var(core_changes) / var(periph_changes)
  )
})

cat("\nCore threshold sensitivity:\n")
print(sensitivity_core, width = 120)

core_sens_path <- file.path(tables_dir, "appendix_core_threshold_sensitivity.csv")
write_csv(sensitivity_core, core_sens_path)
verify_output(core_sens_path)

# ============================================================================
# 6.2: Period effects (pre-Charter vs post-Charter)
# ============================================================================
cat("\n--- 6.2: Period effects ---\n")

pre_charter <- all_area_pct %>%
  filter(year_to <= 2007, !is.na(pct_change)) %>%
  pull(pct_change)

post_charter <- all_area_pct %>%
  filter(year_to >= 2008, !is.na(pct_change)) %>%
  pull(pct_change)

cat(sprintf("  Pre-Charter (<=2007): n = %d\n", length(pre_charter)))
cat(sprintf("  Post-Charter (>=2008): n = %d\n", length(post_charter)))

# Also compute restricted versions
pre_restr <- all_area_pct %>%
  filter(year_to <= 2007, !is.na(pct_change), status != "zero_to_zero") %>%
  pull(pct_change)
post_restr <- all_area_pct %>%
  filter(year_to >= 2008, !is.na(pct_change), status != "zero_to_zero") %>%
  pull(pct_change)

period_comparison <- tibble(
  Period = c("Pre-Charter (<=2007)", "Post-Charter (>=2008)",
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
                mean(pre_restr == -1), mean(post_restr == -1))
)

# Try t-distribution fitting for restricted periods
cat("\n  Fitting t-distribution for restricted periods...\n")
tfit_pre <- fit_location_scale_t(pre_restr, "pre-Charter restricted")
tfit_post <- fit_location_scale_t(post_restr, "post-Charter restricted")

period_comparison$t_mu    <- c(NA, NA, tfit_pre$mu, tfit_post$mu)
period_comparison$t_sigma <- c(NA, NA, tfit_pre$sigma, tfit_post$sigma)
period_comparison$t_nu    <- c(NA, NA, tfit_pre$nu, tfit_post$nu)
period_comparison$t_note  <- c("", "", tfit_pre$note, tfit_post$note)

cat("\nPeriod comparison:\n")
print(period_comparison, width = 120)

period_path <- file.path(tables_dir, "appendix_period_effects.csv")
write_csv(period_comparison, period_path)
verify_output(period_path)

# ============================================================================
# Sensitivity: One-year windows
# ============================================================================
cat("\n--- Sensitivity: One-year windows ---\n")

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
    active_areas = sum(c_across(all_of(area_cols)) > 0),
    hhi          = compute_hhi(c_across(all_of(area_cols))),
    shannon      = compute_shannon(c_across(all_of(area_cols))),
    core_total   = sum(c_across(all_of(paste0("area_", CORE_AREAS)))),
    core_prop    = if_else(total > 0, core_total / total, NA_real_),
    periph_prop  = if_else(total > 0, 1 - core_prop, NA_real_),
    year_type_1yr = classify_1yr(year)
  ) %>%
  ungroup() %>%
  filter(year_type_1yr != "zero_output")

comparison_1yr <- year_profiles_1yr %>%
  group_by(year_type_1yr) %>%
  summarise(
    n_years        = n(),
    mean_total     = mean(total),
    sd_total       = sd(total),
    mean_active    = mean(active_areas),
    sd_active      = sd(active_areas),
    mean_hhi       = mean(hhi, na.rm = TRUE),
    sd_hhi         = sd(hhi, na.rm = TRUE),
    mean_core_prop = mean(core_prop, na.rm = TRUE),
    sd_core_prop   = sd(core_prop, na.rm = TRUE),
    mean_periph_prop = mean(periph_prop, na.rm = TRUE),
    sd_periph_prop   = sd(periph_prop, na.rm = TRUE),
    mean_entropy   = mean(shannon, na.rm = TRUE),
    sd_entropy     = sd(shannon, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nOne-year window comparison:\n")
print(comparison_1yr, width = 120)

sensitivity_1yr_path <- file.path(tables_dir, "appendix_sensitivity_1yr.csv")
write_csv(comparison_1yr, sensitivity_1yr_path)
verify_output(sensitivity_1yr_path)

cat("\n05_robustness.R completed.\n")
