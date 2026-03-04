###############################################################################
# 04_h2_crisis_milestone.R
# Step 5: H2 — Crisis vs. Milestone Change Patterns
###############################################################################

cat("\n=== 04_h2_crisis_milestone.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

core_cols <- paste0("area_", CORE_AREAS)

# ============================================================================
# 5.1: Year classification and year-level measures
# ============================================================================
cat("\n--- 5.1: Year-level measures ---\n")

# 2008 is milestone only (Charter preceded GFC)
classify_year <- function(y) {
  if (y %in% ZERO_YEARS) return("zero_output")
  if (y %in% CRISIS_YEARS) return("crisis")
  if (y %in% MILESTONE_YEARS) return("milestone")
  return("normal")
}

year_profiles <- matrix_wide %>%
  rowwise() %>%
  mutate(
    active_areas = sum(c_across(all_of(area_cols)) > 0),
    hhi          = compute_hhi(c_across(all_of(area_cols))),
    shannon      = compute_shannon(c_across(all_of(area_cols))),
    core_total   = sum(c_across(all_of(core_cols))),
    core_prop    = if_else(total > 0, core_total / total, NA_real_),
    periph_prop  = if_else(total > 0, 1 - core_prop, NA_real_),
    year_type    = classify_year(year)
  ) %>%
  ungroup() %>%
  filter(year_type != "zero_output")

cat("Year type counts:\n")
print(table(year_profiles$year_type))
cat("\nNote: 2008 classified as milestone only.\n")

# ============================================================================
# 5.2: Table 4 — Year-type comparison
# ============================================================================
cat("\n--- 5.2: Year-type comparison ---\n")

comparison <- year_profiles %>%
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

# Add overall row
overall <- year_profiles %>%
  summarise(
    year_type = "overall",
    n_years = n(),
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

comparison <- bind_rows(comparison, overall)

cat("\nYear-type comparison:\n")
print(comparison, width = 120)

table4_path <- file.path(tables_dir, "table4_year_type_comparison.csv")
write_csv(comparison, table4_path)
verify_output(table4_path)

# ============================================================================
# 5.3: T-distribution by year type
# ============================================================================
cat("\n--- 5.3: T-distribution by year type ---\n")

normal_years <- setdiff(
  ALL_YEARS[ALL_YEARS %in% matrix_wide$year[matrix_wide$total > 0]],
  c(CRISIS_YEARS, MILESTONE_YEARS, ZERO_YEARS)
)

pct_with_type <- all_area_pct %>%
  filter(!is.na(pct_change)) %>%
  mutate(
    is_crisis    = year_to %in% CRISIS_YEARS,
    is_milestone = year_to %in% MILESTONE_YEARS,
    is_normal    = year_to %in% normal_years
  )

crisis_pct    <- pct_with_type %>% filter(is_crisis) %>% pull(pct_change)
milestone_pct <- pct_with_type %>% filter(is_milestone) %>% pull(pct_change)
normal_pct    <- pct_with_type %>% filter(is_normal) %>% pull(pct_change)

cat(sprintf("  Crisis subset:    n = %d\n", length(crisis_pct)))
cat(sprintf("  Milestone subset: n = %d\n", length(milestone_pct)))
cat(sprintf("  Normal subset:    n = %d\n", length(normal_pct)))

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
# 5.4: Table 5 — Event-level profiles with cosine similarity
# ============================================================================
cat("\n--- 5.4: Event-level profiles ---\n")

events <- list(
  list(name = "Asian Financial Crisis (1997-98)",
       type = "crisis", window = c(1997, 1998),
       pre = 1994:1996, post = 1999:2001),
  list(name = "Indian Ocean Tsunami (2004-05)",
       type = "crisis", window = c(2004, 2005),
       pre = 2001:2003, post = 2006:2008,
       note = "2004 trade output not tsunami-related; AADMER in 2005"),
  list(name = "Global Financial Crisis (2009)",
       type = "crisis", window = c(2009),
       pre = 2006:2008, post = 2010:2012,
       note = "2008 classified as milestone (Charter); GFC window = 2009 only"),
  list(name = "COVID-19 (2020)",
       type = "crisis", window = c(2020),
       pre = 2017:2019, post = NA,
       note = "Truncated: data ends 2020"),
  list(name = "Treaty of Amity / First Summit (1976-77)",
       type = "milestone", window = c(1976, 1977),
       pre = 1975:1975, post = 1978:1980,
       note = "Limited pre-event data"),
  list(name = "AFTA Agreement (1992-93)",
       type = "milestone", window = c(1992, 1993),
       pre = 1989:1991, post = 1994:1996,
       note = "1993 has zero output"),
  list(name = "ASEAN Charter (2007-08)",
       type = "milestone", window = c(2007, 2008),
       pre = 2004:2006, post = 2009:2011,
       note = "2008 overlaps with GFC onset; classified as milestone"),
  list(name = "ASEAN Community (2015-16)",
       type = "milestone", window = c(2015, 2016),
       pre = 2012:2014, post = 2017:2019)
)

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
    post_comp      <- rep(NA_real_, length(area_cols))
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
    Event = evt$name, Type = evt$type,
    Window = paste(evt$window, collapse = "-"),
    Pre_Period  = paste(range(evt$pre), collapse = "-"),
    Post_Period = if (any(is.na(evt$post))) "N/A"
                 else paste(range(evt$post), collapse = "-"),
    Window_Total  = sum(window_comp),
    Pre_Total     = sum(pre_comp),
    Post_Total    = if (any(is.na(evt$post))) NA_real_ else sum(post_comp),
    Window_Active = sum(window_comp > 0),
    Pre_Active    = sum(pre_comp > 0),
    Post_Active   = if (any(is.na(evt$post))) NA_integer_ else sum(post_comp > 0),
    Window_HHI    = compute_hhi(window_comp),
    Pre_HHI       = compute_hhi(pre_comp),
    Post_HHI      = if (any(is.na(evt$post))) NA_real_ else compute_hhi(post_comp),
    Cos_Pre_Event  = cos_pre_event,
    Cos_Event_Post = cos_event_post,
    Cos_Pre_Post   = cos_pre_post,
    Note = evt$note %||% ""
  )
})

cat("\nEvent-level profiles:\n")
print(event_profiles %>%
        select(Event, Type, Window_Total, Window_Active,
               Cos_Pre_Event, Cos_Event_Post, Cos_Pre_Post),
      width = 120)

table5_path <- file.path(tables_dir, "table5_event_profiles.csv")
write_csv(event_profiles, table5_path)
verify_output(table5_path)

# ============================================================================
# 5.6: Figure 5 — Grouped comparison (grayscale)
# ============================================================================
cat("\n--- 5.6: Figure 5 ---\n")

plot_data <- comparison %>%
  filter(year_type != "overall") %>%
  select(year_type, mean_total, mean_active, mean_hhi) %>%
  pivot_longer(-year_type, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = recode(Metric,
      "mean_total"  = "Mean Output",
      "mean_active" = "Mean Active Areas",
      "mean_hhi"    = "Mean HHI"
    ),
    year_type = factor(year_type, levels = c("crisis", "milestone", "normal"))
  )

fig5 <- ggplot(plot_data, aes(x = year_type, y = Value, fill = year_type)) +
  geom_col(width = 0.6) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c(
    "crisis" = "grey30", "milestone" = "grey55", "normal" = "grey80"
  )) +
  labs(
    x = "Year Type", y = NULL,
    title = "Policy Output Characteristics by Year Type",
    subtitle = "2008 classified as milestone (Charter preceded GFC)",
    fill = "Year Type"
  ) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(fig5, "fig5_yeartype_comparison", width = 10, height = 5)

cat("\n04_h2_crisis_milestone.R completed.\n")
