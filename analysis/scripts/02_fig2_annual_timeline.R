###############################################################################
# 02_fig2_annual_timeline.R
# Figure 2 — Annual policy output timeline (1967–2020)
###############################################################################

cat("\n=== 02_fig2_annual_timeline.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))

# Prepare stacked bar data
bar_data <- year_area_long %>%
  mutate(category = if_else(main_area %in% CORE_AREAS, "Core", "Peripheral")) %>%
  group_by(year, category) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  mutate(category = factor(category, levels = c("Core", "Peripheral")))

# Crisis bands and milestone lines
crisis_bands <- tibble(
  xmin = c(1996.5, 2003.5, 2008.5, 2019.5),
  xmax = c(1998.5, 2005.5, 2009.5, 2020.5)
)
milestone_xs <- c(1976, 1992, 2007, 2015)

# Active areas per year
active_per_year <- year_area_long %>%
  group_by(year) %>%
  summarise(active = sum(count > 0), .groups = "drop")

# Panel A: Stacked bar chart
p_bars <- ggplot() +
  geom_rect(data = crisis_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.7) +
  geom_vline(xintercept = milestone_xs, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_col(data = bar_data, aes(x = year, y = count, fill = category),
           width = 0.7, colour = "black", linewidth = 0.15) +
  scale_fill_manual(values = c("Core" = "grey30", "Peripheral" = "grey70"),
                    name = NULL) +
  annotate("text", x = c(1997.5, 2004.5, 2009, 2020),
           y = Inf, label = c("AFC", "Tsunami", "GFC", "COVID"),
           vjust = 1.5, size = 3.5, fontface = "italic", family = "serif") +
  annotate("text", x = c(1976, 1992, 2007, 2015),
           y = Inf, label = c("TAC/Summit", "AFTA", "Charter", "Community"),
           vjust = 3, size = 3.5, fontface = "italic", family = "serif") +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Number of instruments") +
  theme_pub() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin = margin(t = -5))

# Panel B: Active policy areas
p_active <- ggplot() +
  geom_rect(data = crisis_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.7) +
  geom_vline(xintercept = milestone_xs, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_line(data = active_per_year, aes(x = year, y = active),
            colour = "black", linewidth = 0.6) +
  geom_point(data = active_per_year, aes(x = year, y = active),
             colour = "black", size = 1.5, shape = 16) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Active policy areas") +
  theme_pub() +
  theme(panel.grid.major.x = element_blank())

fig2 <- (p_bars + theme(plot.margin = margin(5, 5, 2, 5))) /
  (p_active + theme(plot.margin = margin(2, 5, 5, 5))) +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    caption = paste0(
      "Notes: Upper panel shows annual instrument counts by core (dark) and peripheral (light) policy areas.\n",
      "Lower panel shows the number of distinct policy areas receiving at least one instrument per year.\n",
      "Shaded bands = crisis windows; dashed lines = milestone years."
    ),
    theme = theme(
      plot.caption = element_text(size = 10, family = "serif",
                                  hjust = 0, lineheight = 1.2)
    )
  )

save_figure(fig2, "fig2_annual_timeline", width = 9, height = 7)

cat("\n02_fig2_annual_timeline.R completed.\n")
