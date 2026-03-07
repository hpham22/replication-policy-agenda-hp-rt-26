###############################################################################
# 07_figure_faceted_distributions.R
# Faceted histogram: Crisis vs. Milestone vs. Normal year-type distributions
# (Restricted pct-count distribution, tagged on year_from)
###############################################################################

cat("\n=== 07_figure_faceted_distributions.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

# ============================================================================
# Tag restricted observations by year_from classification
# ============================================================================

# Derive normal years (non-zero, non-crisis, non-milestone)
normal_years <- setdiff(
  ALL_YEARS[ALL_YEARS %in% matrix_wide$year[matrix_wide$total > 0]],
  c(CRISIS_YEARS, MILESTONE_YEARS, ZERO_YEARS)
)

# Filter to restricted distribution and tag by year_from
tagged <- all_area_pct %>%
  filter(!is.na(pct_change), status != "zero_to_zero") %>%
  mutate(
    year_type = case_when(
      year_from %in% CRISIS_YEARS    ~ "Crisis years",
      year_from %in% MILESTONE_YEARS ~ "Milestone years",
      year_from %in% normal_years    ~ "Normal years",
      TRUE                           ~ NA_character_
    )
  ) %>%
  filter(!is.na(year_type))

cat("Year-type counts (tagged on year_from):\n")
print(table(tagged$year_type))
cat(sprintf("Total tagged: %d of %d restricted obs\n", nrow(tagged), length(pooled_restricted)))

# ============================================================================
# Build plot data: Aggregate + typed panels
# ============================================================================

agg_df <- tagged %>%
  select(pct_change) %>%
  mutate(panel = "Aggregate")

typed_df <- tagged %>%
  select(pct_change, panel = year_type)

plot_df <- bind_rows(agg_df, typed_df) %>%
  mutate(panel = factor(panel,
    levels = c("Aggregate", "Crisis years", "Milestone years", "Normal years")))

# ============================================================================
# Compute per-panel statistics
# ============================================================================

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

cat("\nPer-panel statistics:\n")
print(panel_stats, width = 100)

# ============================================================================
# Compute normal overlay curves per panel
# ============================================================================

x_range <- seq(min(plot_df$pct_change) - 0.5, max(plot_df$pct_change) + 0.5, length.out = 300)

norm_data <- panel_stats %>%
  rowwise() %>%
  do({
    tibble(
      panel = factor(.$panel, levels = levels(plot_df$panel)),
      x = x_range,
      y = dnorm(x_range, mean = .$m, sd = .$s)
    )
  }) %>%
  ungroup()

# ============================================================================
# Compute bin width from aggregate
# ============================================================================

bw <- 2 * IQR(pooled_restricted) / length(pooled_restricted)^(1/3)
bw <- max(bw, 0.25)  # floor at 0.25
cat(sprintf("\nBin width: %.3f\n", bw))

# ============================================================================
# Annotation labels
# ============================================================================

annot_labels <- panel_stats %>%
  mutate(
    label = sprintf("N = %d\nKurtosis = %.1f\nL-kurtosis = %.4f", n, kurt, lkurt)
  )

# ============================================================================
# Build figure
# ============================================================================

fig6 <- ggplot(plot_df, aes(x = pct_change)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = bw, fill = "grey40", colour = "white", linewidth = 0.2
  ) +
  geom_line(
    data = norm_data,
    aes(x = x, y = y),
    linetype = "dashed", colour = "black", linewidth = 0.5
  ) +
  geom_text(
    data = annot_labels,
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.1, vjust = 1.3, size = 2.8, family = "mono",
    inherit.aes = FALSE
  ) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  coord_cartesian(xlim = c(-1.5, max(plot_df$pct_change) + 0.5)) +
  labs(
    x = "Annual Percentage Change",
    y = "Density",
    title = "Distribution of Percentage Changes by Year Type",
    subtitle = "Restricted distribution (excl. 0-to-0) | Dashed line = Normal overlay"
  ) +
  theme_pub() +
  theme(
    strip.text = element_text(size = 11, face = "bold")
  )

save_figure(fig6, "fig6_faceted_distributions", width = 8, height = 6)

cat("\n07_figure_faceted_distributions.R completed.\n")
