###############################################################################
# 06_h2_crisis_milestone.R
# Table 5 — Year-type comparison
# Table 6 — Event-level profiles with cosine similarities
# Figure 4 — Faceted distribution comparison
# Appendix figure: Year-type grouped bars
###############################################################################

cat("\n=== 06_h2_crisis_milestone.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

core_cols <- paste0("area_", CORE_AREAS)

# ============================================================================
# Year classification
# ============================================================================
cat("\n--- Year classification ---\n")

# Derive normal years (non-zero, non-crisis, non-milestone)
normal_years <- setdiff(
  ALL_YEARS[ALL_YEARS %in% matrix_wide$year[matrix_wide$total > 0]],
  c(CRISIS_YEARS, MILESTONE_YEARS, ZERO_YEARS)
)

year_profiles <- matrix_wide %>%
  filter(total > 0) %>%
  rowwise() %>%
  mutate(
    active_areas = sum(c_across(all_of(area_cols)) > 0),
    hhi          = compute_hhi(c_across(all_of(area_cols))),
    shannon      = compute_shannon(c_across(all_of(area_cols))),
    core_total   = sum(c_across(all_of(core_cols))),
    core_prop    = core_total / total,
    periph_prop  = 1 - core_prop,
    year_type    = case_when(
      year %in% CRISIS_YEARS    ~ "crisis",
      year %in% MILESTONE_YEARS ~ "milestone",
      TRUE                      ~ "normal"
    )
  ) %>%
  ungroup()

cat("Year type counts:\n")
print(table(year_profiles$year_type))

# ============================================================================
# Table 5 — Year-type comparison
# ============================================================================
cat("\n--- Table 5: Year-type comparison ---\n")

comparison <- year_profiles %>%
  group_by(year_type) %>%
  summarise(
    n_years = n(),
    mean_total = mean(total), sd_total = sd(total),
    mean_active = mean(active_areas), sd_active = sd(active_areas),
    mean_hhi = mean(hhi, na.rm = TRUE), sd_hhi = sd(hhi, na.rm = TRUE),
    mean_core_prop = mean(core_prop, na.rm = TRUE),
    sd_core_prop = sd(core_prop, na.rm = TRUE),
    mean_periph_prop = mean(periph_prop, na.rm = TRUE),
    sd_periph_prop = sd(periph_prop, na.rm = TRUE),
    mean_entropy = mean(shannon, na.rm = TRUE),
    sd_entropy = sd(shannon, na.rm = TRUE),
    .groups = "drop"
  )

overall <- year_profiles %>%
  summarise(
    year_type = "overall", n_years = n(),
    mean_total = mean(total), sd_total = sd(total),
    mean_active = mean(active_areas), sd_active = sd(active_areas),
    mean_hhi = mean(hhi, na.rm = TRUE), sd_hhi = sd(hhi, na.rm = TRUE),
    mean_core_prop = mean(core_prop, na.rm = TRUE),
    sd_core_prop = sd(core_prop, na.rm = TRUE),
    mean_periph_prop = mean(periph_prop, na.rm = TRUE),
    sd_periph_prop = sd(periph_prop, na.rm = TRUE),
    mean_entropy = mean(shannon, na.rm = TRUE),
    sd_entropy = sd(shannon, na.rm = TRUE)
  )

table5 <- bind_rows(comparison, overall)
cat("\nTable 5:\n")
print(table5, width = 120)

table5_path <- file.path(tables_dir, "table5_year_type_comparison.csv")
write_csv(table5, table5_path)
verify_output(table5_path)

# --- kableExtra output for Table 5 ---
table5_display <- table5 %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  mutate(
    `Output (mean ± SD)` = sprintf("%.1f ± %.1f", mean_total, sd_total),
    `Active Areas (mean ± SD)` = sprintf("%.1f ± %.1f", mean_active, sd_active),
    `HHI (mean ± SD)` = sprintf("%.3f ± %.3f", mean_hhi, sd_hhi),
    `Core Share (mean ± SD)` = sprintf("%.3f ± %.3f", mean_core_prop, sd_core_prop),
    `Entropy (mean ± SD)` = sprintf("%.3f ± %.3f", mean_entropy, sd_entropy)
  ) %>%
  select(Year_Type = year_type, N = n_years,
         `Output (mean ± SD)`, `Active Areas (mean ± SD)`,
         `HHI (mean ± SD)`, `Core Share (mean ± SD)`,
         `Entropy (mean ± SD)`)

save_table(
  table5_display, "table5_year_type_comparison",
  caption = "Table 5. Year-Type Comparison of Agenda Characteristics",
  footnote = "Years with zero output excluded. Crisis: 1997-98, 2004-05, 2009, 2020. Milestone: 1976-77, 1992-93, 2007-08, 2015-16."
)

# ============================================================================
# T-distribution by year type (restricted)
# ============================================================================
cat("\n--- T-distribution by year type (restricted) ---\n")

pct_with_type <- all_area_pct %>%
  filter(!is.na(pct_change), status != "zero_to_zero") %>%
  mutate(
    is_crisis    = year_to %in% CRISIS_YEARS,
    is_milestone = year_to %in% MILESTONE_YEARS,
    is_normal    = year_to %in% normal_years
  )

crisis_pct    <- pct_with_type %>% filter(is_crisis) %>% pull(pct_change)
milestone_pct <- pct_with_type %>% filter(is_milestone) %>% pull(pct_change)
normal_pct    <- pct_with_type %>% filter(is_normal) %>% pull(pct_change)

cat(sprintf("  Crisis: n=%d | Milestone: n=%d | Normal: n=%d\n",
            length(crisis_pct), length(milestone_pct), length(normal_pct)))

fit_crisis    <- fit_location_scale_t(crisis_pct, "crisis")
fit_milestone <- fit_location_scale_t(milestone_pct, "milestone")
fit_normal    <- fit_location_scale_t(normal_pct, "normal")

for (f in list(list("Crisis", fit_crisis),
               list("Milestone", fit_milestone),
               list("Normal", fit_normal))) {
  cat(sprintf("  %s: mu=%.6f, sigma=%.6f, nu=%.4f (%s)\n",
              f[[1]], f[[2]]$mu, f[[2]]$sigma, f[[2]]$nu, f[[2]]$note))
}

# ============================================================================
# Table 6 — Event-level profiles with cosine similarity
# ============================================================================
cat("\n--- Table 6: Event profiles ---\n")

events <- list(
  list(name = "Asian Financial Crisis (1997-98)", type = "crisis",
       years = c(1997, 1998), pre_span = 1994:1996, post_span = 1999:2001),
  list(name = "Indian Ocean Tsunami (2004-05)", type = "crisis",
       years = c(2004, 2005), pre_span = 2001:2003, post_span = 2006:2008),
  list(name = "Global Financial Crisis (2009)", type = "crisis",
       years = c(2009), pre_span = 2006:2008, post_span = 2010:2012),
  list(name = "COVID-19 (2020)", type = "crisis",
       years = c(2020), pre_span = 2017:2019, post_span = integer(0)),
  list(name = "Treaty of Amity / First Summit (1976-77)", type = "milestone",
       years = c(1976, 1977), pre_span = 1972:1975, post_span = 1978:1980),
  list(name = "AFTA Agreement (1992-93)", type = "milestone",
       years = c(1992, 1993), pre_span = 1989:1991, post_span = 1994:1996),
  list(name = "ASEAN Charter (2007-08)", type = "milestone",
       years = c(2007, 2008), pre_span = 2004:2006, post_span = 2009:2011),
  list(name = "ASEAN Community (2015-16)", type = "milestone",
       years = c(2015, 2016), pre_span = 2012:2014, post_span = 2017:2019)
)

get_profile <- function(yrs) {
  subset_data <- matrix_wide %>% filter(year %in% yrs, total > 0)
  if (nrow(subset_data) == 0) return(list(vec = rep(0, length(area_cols)),
                                           total = 0, active = 0))
  pooled <- colSums(subset_data[, area_cols])
  list(vec = pooled / sum(pooled), total = sum(subset_data$total),
       active = sum(pooled > 0))
}

event_table <- map_dfr(events, function(ev) {
  pre  <- get_profile(ev$pre_span)
  event <- get_profile(ev$years)
  post <- get_profile(ev$post_span)

  cos_pre_event <- if (pre$total > 0 && event$total > 0)
    cosine_similarity(pre$vec, event$vec) else NA_real_
  cos_event_post <- if (event$total > 0 && post$total > 0)
    cosine_similarity(event$vec, post$vec) else NA_real_
  cos_pre_post <- if (pre$total > 0 && post$total > 0)
    cosine_similarity(pre$vec, post$vec) else NA_real_

  tibble(
    Event = ev$name, Type = ev$type,
    Window_Total = event$total, Window_Active = event$active,
    Cos_Pre_Event = round(cos_pre_event, 3),
    Cos_Event_Post = round(cos_event_post, 3),
    Cos_Pre_Post = round(cos_pre_post, 3)
  )
})

cat("\nEvent profiles:\n")
print(event_table, width = 120)

table6_path <- file.path(tables_dir, "table6_event_profiles.csv")
write_csv(event_table, table6_path)
verify_output(table6_path)

# --- kableExtra output for Table 6 ---
save_table(
  event_table, "table6_event_profiles",
  caption = "Table 6. Event Profiles with Cosine Similarity",
  footnote = "Cosine similarity between pooled area-share vectors for pre-event, event window, and post-event periods (3-year spans). NA indicates missing comparison window.",
  col_names = c("Event", "Type", "Window Total", "Active Areas",
                "Cos(Pre, Event)", "Cos(Event, Post)", "Cos(Pre, Post)"),
  pack_rows = list(
    list(label = "Crises", start = 1, end = 4),
    list(label = "Milestones", start = 5, end = 8)
  )
)

# ============================================================================
# Figure 4 — Faceted distribution comparison (2x2)
# ============================================================================
cat("\n--- Figure 4: Faceted distributions ---\n")

tagged <- all_area_pct %>%
  filter(!is.na(pct_change), status != "zero_to_zero") %>%
  mutate(
    year_type = case_when(
      year_from %in% CRISIS_YEARS    ~ "Crisis",
      year_from %in% MILESTONE_YEARS ~ "Milestone",
      year_from %in% normal_years    ~ "Normal",
      TRUE                           ~ NA_character_
    )
  ) %>%
  filter(!is.na(year_type))

agg_df <- tagged %>% select(pct_change) %>% mutate(panel = "Aggregate")
typed_df <- tagged %>% select(pct_change, panel = year_type)
plot_df <- bind_rows(agg_df, typed_df) %>%
  mutate(panel = factor(panel,
    levels = c("Aggregate", "Crisis", "Milestone", "Normal")))

panel_stats <- plot_df %>%
  group_by(panel) %>%
  summarise(
    n = n(),
    kurt = calc_kurtosis_raw(pct_change),
    lkurt = as.numeric(calc_lkurtosis(pct_change)),
    m = mean(pct_change),
    s = sd(pct_change),
    .groups = "drop"
  )

x_range <- seq(min(plot_df$pct_change) - 0.5, max(plot_df$pct_change) + 0.5,
               length.out = 300)

norm_data <- panel_stats %>%
  rowwise() %>%
  do({
    tibble(panel = factor(.$panel, levels = levels(plot_df$panel)),
           x = x_range,
           y = dnorm(x_range, mean = .$m, sd = .$s))
  }) %>%
  ungroup()

bw <- max(2 * IQR(pooled_restricted) / length(pooled_restricted)^(1/3), 0.25)

annot_labels <- panel_stats %>%
  mutate(label = sprintf("N = %d\nKurtosis = %.1f\nL-kurtosis = %.4f",
                         n, kurt, lkurt))

fig4 <- ggplot(plot_df, aes(x = pct_change)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = bw, fill = "grey40", colour = "white",
                 linewidth = 0.2) +
  geom_line(data = norm_data, aes(x = x, y = y),
            linetype = "dashed", colour = "black", linewidth = 0.5) +
  geom_text(data = annot_labels,
            aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.3, size = 3.5, family = "serif",
            inherit.aes = FALSE) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  coord_cartesian(xlim = c(-1.5, max(plot_df$pct_change) + 0.5)) +
  labs(x = "Annual percentage change", y = "Density") +
  theme_pub()

save_figure(fig4, "fig4_faceted_distributions", width = 9, height = 6)

# ============================================================================
# Appendix figure: Year-type grouped comparison
# ============================================================================
cat("\n--- Appendix figure: Year-type grouped bars ---\n")

comp_long <- comparison %>%
  select(year_type, mean_total, mean_active, mean_hhi) %>%
  pivot_longer(-year_type, names_to = "metric", values_to = "value") %>%
  mutate(
    metric = factor(metric,
      levels = c("mean_total", "mean_active", "mean_hhi"),
      labels = c("Mean output", "Mean active areas", "Mean HHI")),
    year_type = factor(year_type, levels = c("crisis", "milestone", "normal"))
  )

fig_app_bars <- ggplot(comp_long,
       aes(x = year_type, y = value, fill = year_type)) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = c("crisis" = "grey30", "milestone" = "grey55",
                                "normal" = "grey80")) +
  labs(x = NULL, y = NULL) +
  theme_pub() +
  theme(legend.position = "none")

save_figure(fig_app_bars, "fig_appendix_yeartype_bars", width = 9, height = 4.5)

cat("\n06_h2_crisis_milestone.R completed.\n")
