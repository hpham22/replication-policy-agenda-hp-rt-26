###############################################################################
# 04_h2_crisis_milestone.R
# Step 3: H2 — Crisis vs. Milestone Change Patterns
###############################################################################

cat("\n=== 04_h2_crisis_milestone.R ===\n")

# Source setup if not already loaded
if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

# Load matrix data
load(file.path(tables_dir, "matrix_data.RData"))

# Area column names
area_cols <- paste0("area_", ALL_AREAS)
core_cols <- paste0("area_", CORE_AREAS)

# ============================================================================
# Year classification
# 2008 is included in BOTH crisis and milestone (per user instruction)
# ============================================================================

classify_year <- function(y) {
  tags <- character(0)
  if (y %in% ZERO_YEARS) return("zero_output")
  if (y %in% CRISIS_YEARS) tags <- c(tags, "crisis")
  if (y %in% MILESTONE_YEARS) tags <- c(tags, "milestone")
  if (length(tags) == 0) return("normal")
  paste(tags, collapse = "+")
}

# For aggregate comparison: 2008 appears in both groups
# We'll create rows for each classification
year_profiles_base <- matrix_wide %>%
  rowwise() %>%
  mutate(
    active_areas = sum(c_across(all_of(area_cols)) > 0),
    hhi = compute_hhi(c_across(all_of(area_cols))),
    shannon = compute_shannon(c_across(all_of(area_cols))),
    core_total = sum(c_across(all_of(core_cols))),
    core_prop = if_else(total > 0, core_total / total, NA_real_),
    periph_prop = if_else(total > 0, 1 - core_prop, NA_real_),
    year_class = classify_year(year)
  ) %>%
  ungroup()

# ============================================================================
# Step 3.1: Expand year types for duplicate counting of 2008
# ============================================================================
cat("\n--- Step 3.1: Year-level metrics ---\n")

# Create expanded dataset: duplicate 2008 (once as crisis, once as milestone)
year_profiles_expanded <- year_profiles_base %>%
  filter(year_class != "zero_output") %>%
  mutate(
    year_type = case_when(
      year_class == "crisis"            ~ "crisis",
      year_class == "milestone"         ~ "milestone",
      year_class == "crisis+milestone"  ~ "crisis",  # first copy: crisis
      year_class == "normal"            ~ "normal"
    )
  )

# Add second copy of 2008 as milestone
dual_years <- year_profiles_base %>%
  filter(year_class == "crisis+milestone") %>%
  mutate(year_type = "milestone")

year_profiles_full <- bind_rows(year_profiles_expanded, dual_years)

cat("Year type counts (with 2008 in both):\n")
print(table(year_profiles_full$year_type))

# ============================================================================
# Step 3.2: Comparison table across year types
# ============================================================================
cat("\n--- Step 3.2: Comparison by year type ---\n")

comparison <- year_profiles_full %>%
  group_by(year_type) %>%
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

cat("\nYear-type comparison:\n")
print(comparison, width = 120)

table4_path <- file.path(tables_dir, "table4_year_type_comparison.csv")
write_csv(comparison, table4_path)
verify_output(table4_path)

# ============================================================================
# Step 3.3: Event-level profiles with cosine similarity
# ============================================================================
cat("\n--- Step 3.3: Event-level profiles ---\n")

events <- list(
  list(name = "Asian Financial Crisis (1997-98)",
       type = "crisis", window = c(1997, 1998),
       pre = 1994:1996, post = 1999:2001),
  list(name = "Indian Ocean Tsunami (2004-05)",
       type = "crisis", window = c(2004, 2005),
       pre = 2001:2003, post = 2006:2008,
       note = "2004 trade output not tsunami-related; AADMER appears 2005"),
  list(name = "GFC / Cyclone Nargis (2008-09)",
       type = "crisis", window = c(2008, 2009),
       pre = 2005:2007, post = 2010:2012),
  list(name = "COVID-19 (2020)",
       type = "crisis", window = c(2020),
       pre = 2017:2019, post = NA,
       note = "Truncated: data ends 2020, no post-event comparison"),
  list(name = "Treaty of Amity / First Summit (1976-77)",
       type = "milestone", window = c(1976, 1977),
       pre = 1975:1975, post = 1978:1980,
       note = "Limited pre-event data (sparse output before 1976)"),
  list(name = "AFTA Agreement (1992-93)",
       type = "milestone", window = c(1992, 1993),
       pre = 1989:1991, post = 1994:1996,
       note = "1993 has zero output; milestone year 1992 produced output but follow-up year didn't"),
  list(name = "ASEAN Charter (2007-08)",
       type = "milestone", window = c(2007, 2008),
       pre = 2004:2006, post = 2009:2011),
  list(name = "ASEAN Community (2015-16)",
       type = "milestone", window = c(2015, 2016),
       pre = 2012:2014, post = 2017:2019)
)

# Helper: get area composition vector for a set of years
get_composition <- function(years) {
  if (any(is.na(years))) return(rep(NA_real_, length(area_cols)))
  matrix_wide %>%
    filter(year %in% years) %>%
    select(all_of(area_cols)) %>%
    colSums()
}

event_profiles <- map_dfr(events, function(evt) {
  window_comp <- get_composition(evt$window)
  pre_comp    <- get_composition(evt$pre)

  if (any(is.na(evt$post))) {
    post_comp <- rep(NA_real_, length(area_cols))
    cos_pre_event  <- cosine_similarity(pre_comp, window_comp)
    cos_event_post <- NA_real_
    cos_pre_post   <- NA_real_
  } else {
    post_comp      <- get_composition(evt$post)
    cos_pre_event  <- cosine_similarity(pre_comp, window_comp)
    cos_event_post <- cosine_similarity(window_comp, post_comp)
    cos_pre_post   <- cosine_similarity(pre_comp, post_comp)
  }

  tibble(
    Event         = evt$name,
    Type          = evt$type,
    Window        = paste(evt$window, collapse = "-"),
    Pre_Period    = paste(range(evt$pre), collapse = "-"),
    Post_Period   = if (any(is.na(evt$post))) "N/A" else paste(range(evt$post), collapse = "-"),
    Window_Total  = sum(window_comp),
    Pre_Total     = sum(pre_comp),
    Post_Total    = if (any(is.na(evt$post))) NA_real_ else sum(post_comp),
    Window_Active_Areas = sum(window_comp > 0),
    Pre_Active_Areas    = sum(pre_comp > 0),
    Post_Active_Areas   = if (any(is.na(evt$post))) NA_integer_ else sum(post_comp > 0),
    Window_HHI          = compute_hhi(window_comp),
    Pre_HHI             = compute_hhi(pre_comp),
    Post_HHI            = if (any(is.na(evt$post))) NA_real_ else compute_hhi(post_comp),
    Cos_Pre_Event       = cos_pre_event,
    Cos_Event_Post      = cos_event_post,
    Cos_Pre_Post        = cos_pre_post,
    Note                = evt$note %||% ""
  )
})

cat("\nEvent-level profiles:\n")
print(event_profiles %>% select(Event, Type, Window_Total, Window_Active_Areas,
                                 Cos_Pre_Event, Cos_Event_Post, Cos_Pre_Post),
      width = 120)

table5_path <- file.path(tables_dir, "table5_event_profiles.csv")
write_csv(event_profiles, table5_path)
verify_output(table5_path)

# Detailed area composition for each event
cat("\nDetailed area composition by event:\n")
for (evt in events) {
  cat(sprintf("\n  --- %s ---\n", evt$name))
  window_comp <- get_composition(evt$window)
  names(window_comp) <- ALL_AREAS
  active <- window_comp[window_comp > 0]
  if (length(active) > 0) {
    for (a in names(active)) {
      cat(sprintf("    Area %s (%s): %d\n", a, AREA_LABELS[a], active[a]))
    }
  } else {
    cat("    No instruments in window\n")
  }
}

# ============================================================================
# Step 3.4: Sensitivity check — one-year windows
# ============================================================================
cat("\n--- Step 3.4: Sensitivity check (one-year windows) ---\n")

classify_1yr <- function(y) {
  if (y %in% ZERO_YEARS) return("zero_output")
  tags <- character(0)
  if (y %in% CRISIS_1YR) tags <- c(tags, "crisis")
  if (y %in% MILESTONE_1YR) tags <- c(tags, "milestone")
  if (length(tags) == 0) return("normal")
  paste(tags, collapse = "+")
}

year_profiles_1yr <- year_profiles_base %>%
  filter(year_class != "zero_output") %>%
  mutate(year_class_1yr = sapply(year, classify_1yr))

# Expand for 2008 (in both crisis_1yr and milestone_1yr? No — 2008 is only crisis_1yr)
# Actually check: CRISIS_1YR has 2008, MILESTONE_1YR does not have 2008
# So no overlap in 1yr windows
year_profiles_1yr <- year_profiles_1yr %>%
  mutate(
    year_type_1yr = case_when(
      year_class_1yr == "crisis"    ~ "crisis",
      year_class_1yr == "milestone" ~ "milestone",
      year_class_1yr == "normal"    ~ "normal",
      TRUE                         ~ year_class_1yr
    )
  )

comparison_1yr <- year_profiles_1yr %>%
  filter(year_type_1yr %in% c("crisis", "milestone", "normal")) %>%
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

cat("\nSensitivity: One-year window comparison:\n")
print(comparison_1yr, width = 120)

sensitivity_path <- file.path(tables_dir, "appendix_sensitivity_1yr.csv")
write_csv(comparison_1yr, sensitivity_path)
verify_output(sensitivity_path)

# ============================================================================
# Step 3.5: Figures
# ============================================================================
cat("\n--- Step 3.5: Generating figures ---\n")

# Bar chart: annual output colored by year type
bar_data <- year_profiles_base %>%
  filter(total > 0) %>%
  mutate(
    year_type_display = case_when(
      year_class == "crisis"            ~ "Crisis",
      year_class == "milestone"         ~ "Milestone",
      year_class == "crisis+milestone"  ~ "Crisis + Milestone",
      year_class == "normal"            ~ "Normal"
    )
  )

fig3 <- ggplot(bar_data, aes(x = year, y = total, fill = year_type_display)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c(
    "Normal"              = "grey60",
    "Crisis"              = "firebrick",
    "Milestone"           = "steelblue",
    "Crisis + Milestone"  = "darkorchid"
  )) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  labs(
    x = "Year",
    y = "Number of Legal Instruments",
    title = "ASEAN Legal Instrument Output by Year Type",
    subtitle = "2008 classified as both Crisis and Milestone (shown in purple)",
    fill = "Year Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

fig3_png <- file.path(figures_dir, "fig3_output_by_year_type.png")
fig3_pdf <- file.path(figures_dir, "fig3_output_by_year_type.pdf")

ggsave(fig3_png, fig3, width = 10, height = 5, dpi = 300, device = agg_png)
ggsave(fig3_pdf, fig3, width = 10, height = 5)

verify_output(fig3_png)
verify_output(fig3_pdf)

# Figure 4: HHI and peripheral proportion comparison (dot plot)
dot_data <- comparison %>%
  select(year_type, mean_hhi, mean_periph_prop) %>%
  pivot_longer(-year_type, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = recode(Metric,
      "mean_hhi" = "Mean HHI (Concentration)",
      "mean_periph_prop" = "Mean Peripheral Share"
    ),
    year_type = factor(year_type, levels = c("crisis", "milestone", "normal"))
  )

fig4 <- ggplot(dot_data, aes(x = year_type, y = Value, color = year_type)) +
  geom_point(size = 4) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_color_manual(values = c(
    "crisis"    = "firebrick",
    "milestone" = "steelblue",
    "normal"    = "grey40"
  )) +
  labs(
    x = "Year Type",
    y = "Value",
    title = "Policy Concentration and Peripheral Attention by Year Type",
    color = "Year Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "none"
  )

fig4_png <- file.path(figures_dir, "fig4_hhi_peripheral_dotplot.png")
fig4_pdf <- file.path(figures_dir, "fig4_hhi_peripheral_dotplot.pdf")

ggsave(fig4_png, fig4, width = 8, height = 5, dpi = 300, device = agg_png)
ggsave(fig4_pdf, fig4, width = 8, height = 5)

verify_output(fig4_png)
verify_output(fig4_pdf)

# ============================================================================
# Step 3.6: Year-type t-distribution fits (Fernandez-i-Marin et al.)
# ============================================================================
cat("\n--- Step 3.6: Year-type t-distribution fits ---\n")
cat("  Classifying percentage changes by year_to (destination year)\n")
cat("  2008 included in both crisis and milestone subsets\n\n")

# Load pct changes data
load(file.path(tables_dir, "pct_changes.RData"))

# Classify year_to for each percentage change
# Normal years: years with output that aren't crisis, milestone, or zero-output
normal_years <- setdiff(ALL_YEARS[ALL_YEARS %in% matrix_wide$year[matrix_wide$total > 0]],
                        c(CRISIS_YEARS, MILESTONE_YEARS, ZERO_YEARS))

# Tag each percentage change with its year_to classification
pct_with_type <- all_area_pct %>%
  filter(!is.na(pct_change)) %>%
  mutate(
    is_crisis    = year_to %in% CRISIS_YEARS,
    is_milestone = year_to %in% MILESTONE_YEARS,
    is_normal    = year_to %in% normal_years
  )

# Extract subsets (2008 appears in both crisis and milestone)
crisis_pct    <- pct_with_type %>% filter(is_crisis) %>% pull(pct_change)
milestone_pct <- pct_with_type %>% filter(is_milestone) %>% pull(pct_change)
normal_pct    <- pct_with_type %>% filter(is_normal) %>% pull(pct_change)

cat(sprintf("  Crisis subset:    n = %d\n", length(crisis_pct)))
cat(sprintf("  Milestone subset: n = %d\n", length(milestone_pct)))
cat(sprintf("  Normal subset:    n = %d\n", length(normal_pct)))

# Fit t-distributions for each year-type subset
compute_yeartype_stats <- function(x, label) {
  n <- length(x)
  cat(sprintf("\n  --- %s (n = %d) ---\n", label, n))

  lm_result <- calc_lmoments(x)
  lkurt <- as.numeric(lm_result$ratios["tau4"])

  tfit <- fit_location_scale_t(x, label = label, do_bootstrap = TRUE)
  if (!is.na(tfit$mu)) {
    cat(sprintf("    mu=%.6f, sigma=%.6f, sigma^2=%.6f, nu=%.4f\n",
                tfit$mu, tfit$sigma, tfit$sigma_sq, tfit$nu))
    cat(sprintf("    %s\n", tfit$interpretation))
  } else {
    cat(sprintf("    FAILED: %s\n", tfit$note))
  }

  tibble(
    Group = label, N = n,
    t_mu = tfit$mu, t_sigma = tfit$sigma, t_sigma_sq = tfit$sigma_sq,
    t_nu = tfit$nu,
    t_theo_var = ifelse(is.null(tfit$theo_var), NA, tfit$theo_var),
    L_Kurtosis = lkurt,
    t_sigma_ci_lo = tfit$sigma_ci_lo, t_sigma_ci_hi = tfit$sigma_ci_hi,
    t_nu_ci_lo = tfit$nu_ci_lo, t_nu_ci_hi = tfit$nu_ci_hi,
    t_loglik = tfit$loglik, t_method = tfit$method,
    t_note = tfit$note, t_interpretation = tfit$interpretation
  )
}

stats_crisis    <- compute_yeartype_stats(crisis_pct, "Crisis years")
stats_milestone <- compute_yeartype_stats(milestone_pct, "Milestone years")
stats_normal    <- compute_yeartype_stats(normal_pct, "Normal years")

# ============================================================================
# Build unified 6-row t-distribution comparison table
# ============================================================================
cat("\n--- Building unified t-distribution comparison table ---\n")

# Load core/peripheral results from h1b
load(file.path(tables_dir, "tfit_results.RData"))

# Extract core/peripheral/aggregate rows in the same format
format_tfit_row <- function(stats_df) {
  stats_df %>%
    select(Group, N, t_mu, t_sigma, t_sigma_sq, t_nu, t_theo_var,
           L_Kurtosis, t_sigma_ci_lo, t_sigma_ci_hi,
           t_nu_ci_lo, t_nu_ci_hi, t_note, t_interpretation)
}

unified_table <- bind_rows(
  format_tfit_row(tfit_results$aggregate$stats),
  format_tfit_row(tfit_results$core$stats),
  format_tfit_row(tfit_results$peripheral$stats),
  format_tfit_row(stats_crisis),
  format_tfit_row(stats_milestone),
  format_tfit_row(stats_normal)
)

# Fix Inf values for CSV output
unified_table <- unified_table %>%
  mutate(
    t_theo_var_str = ifelse(is.infinite(t_theo_var), "Inf", as.character(round(t_theo_var, 6)))
  )

cat("\nUnified t-distribution comparison:\n")
unified_table %>%
  select(Group, N, t_sigma, t_sigma_sq, t_nu, L_Kurtosis, t_interpretation) %>%
  print(width = 120)

unified_path <- file.path(tables_dir, "table_tfit_comparison.csv")
write_csv(unified_table, unified_path)
verify_output(unified_path)

cat("\n04_h2_crisis_milestone.R completed.\n")
