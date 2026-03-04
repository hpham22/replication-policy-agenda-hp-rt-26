###############################################################################
# 01_figures_descriptive.R
# Step 1-2: Descriptive figures — attention shares and annual timeline
###############################################################################

cat("\n=== 01_figures_descriptive.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))

# ============================================================================
# Figure 1: Share of attention across policy areas (horizontal bar chart)
# ============================================================================
cat("\n--- Figure 1: Policy area attention shares ---\n")

area_shares <- df %>%
  count(main_area, name = "count") %>%
  mutate(
    area_name = AREA_LABELS[as.character(main_area)],
    share     = count / sum(count),
    category  = if_else(main_area %in% CORE_AREAS, "Core", "Peripheral")
  ) %>%
  arrange(desc(share))

# Calculate cumulative share for the 75% reference line
area_shares <- area_shares %>%
  mutate(
    cum_share = cumsum(share),
    area_name = fct_reorder(area_name, share)
  )

# Find the 75% boundary
boundary_idx <- min(which(area_shares$cum_share >= 0.75))
cat("Areas accounting for 75% of instruments:", boundary_idx, "\n")
cat("Cumulative share at boundary:", round(area_shares$cum_share[boundary_idx] * 100, 1), "%\n")

fig1 <- ggplot(area_shares, aes(x = area_name, y = share, fill = category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", share * 100)),
            hjust = -0.1, size = 3, colour = "black") +
  coord_flip() +
  scale_fill_manual(values = c("Core" = "grey30", "Peripheral" = "grey70")) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = NULL,
    y = "Share of Total Instruments",
    title = "Distribution of ASEAN Legal Instruments Across Policy Areas",
    subtitle = sprintf("N = %d instruments, 1967\u20132020. Core areas (dark) account for %.0f%% of total.",
                       nrow(df),
                       sum(area_shares$share[area_shares$category == "Core"]) * 100),
    fill = NULL
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

save_figure(fig1, "fig1_attention_shares", width = 8, height = 6)

# ============================================================================
# Figure 2: Annual output timeline with crisis/milestone annotations
# ============================================================================
cat("\n--- Figure 2: Annual timeline ---\n")

core_cols <- paste0("area_", CORE_AREAS)

# Prepare timeline data
timeline_data <- matrix_wide %>%
  rowwise() %>%
  mutate(
    active_areas = sum(c_across(all_of(area_cols)) > 0),
    core_total   = sum(c_across(all_of(core_cols))),
    periph_total = total - core_total
  ) %>%
  ungroup()

# Event annotation rectangles
crisis_bands <- tibble(
  xmin  = c(1996.5, 2003.5, 2008.5, 2019.5),
  xmax  = c(1998.5, 2005.5, 2009.5, 2020.5),
  label = c("AFC", "Tsunami", "GFC", "COVID")
)

milestone_lines <- tibble(
  x     = c(1976, 1992, 2007, 2015),
  label = c("TAC/Summit", "AFTA", "Charter", "Community")
)

# Panel A: Stacked bar chart (core vs peripheral)
bar_long <- timeline_data %>%
  filter(total > 0) %>%
  select(year, core_total, periph_total) %>%
  pivot_longer(-year, names_to = "group", values_to = "count") %>%
  mutate(group = recode(group,
    "core_total" = "Core (10, 15, 20)",
    "periph_total" = "Peripheral"
  ))

p_output <- ggplot() +
  geom_rect(data = crisis_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.7) +
  geom_vline(data = milestone_lines, aes(xintercept = x),
             linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_col(data = bar_long,
           aes(x = year, y = count, fill = group),
           width = 0.7) +
  scale_fill_manual(values = c("Core (10, 15, 20)" = "grey30",
                               "Peripheral" = "grey70")) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  # Add event labels at top
  annotate("text", x = c(1997.5, 2004.5, 2009, 2020),
           y = Inf, label = crisis_bands$label,
           vjust = 1.5, size = 2.5, fontface = "italic") +
  annotate("text", x = milestone_lines$x,
           y = Inf, label = milestone_lines$label,
           vjust = 3, size = 2.5, fontface = "italic", angle = 0) +
  labs(
    x = NULL,
    y = "Number of Instruments",
    title = "ASEAN Legal Instrument Output, 1967\u20132020",
    subtitle = "Shaded bands = crisis windows; dashed lines = milestone years",
    fill = NULL
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

# Panel B: Active policy areas per year
active_data <- timeline_data %>%
  filter(total > 0)

p_active <- ggplot() +
  geom_rect(data = crisis_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.7) +
  geom_vline(data = milestone_lines, aes(xintercept = x),
             linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_line(data = active_data,
            aes(x = year, y = active_areas),
            colour = "black", linewidth = 0.6) +
  geom_point(data = active_data,
             aes(x = year, y = active_areas),
             colour = "black", size = 1.2) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Active Policy Areas") +
  theme_pub()

fig2 <- p_output / p_active + plot_layout(heights = c(2, 1))

save_figure(fig2, "fig2_timeline_annotated", width = 10, height = 7)

cat("\n01_figures_descriptive.R completed.\n")
