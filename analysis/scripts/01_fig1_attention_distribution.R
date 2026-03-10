###############################################################################
# 01_fig1_attention_distribution.R
# Figure 1 — Horizontal bar chart of policy area attention shares
###############################################################################

cat("\n=== 01_fig1_attention_distribution.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

load(file.path(tables_dir, "matrix_data.RData"))

# Compute area shares
area_shares <- year_area_long %>%
  group_by(main_area) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  mutate(
    share = total / sum(total),
    area_name = AREA_LABELS[as.character(main_area)],
    category = if_else(main_area %in% CORE_AREAS, "Core", "Peripheral")
  ) %>%
  arrange(desc(total)) %>%
  mutate(area_name = fct_reorder(area_name, total))

core_share <- sum(area_shares$share[area_shares$category == "Core"]) * 100
cat(sprintf("Core share: %.1f%%\n", core_share))

fig1 <- ggplot(area_shares, aes(x = area_name, y = share, fill = category)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%d (%.1f%%)", total, share * 100)),
            hjust = -0.1, size = 4, family = "serif") +
  scale_fill_manual(values = c("Core" = "grey30", "Peripheral" = "grey70"),
                    name = NULL) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(x = NULL, y = "Share of total instruments") +
  theme_pub() +
  theme(legend.position = c(0.8, 0.3))

save_figure(fig1, "fig1_attention_distribution", width = 6.5, height = 5)

cat("\n01_fig1_attention_distribution.R completed.\n")
