###############################################################################
# run_all.R
# Master runner: executes all analysis scripts in order
###############################################################################

cat("============================================================\n")
cat("ASEAN Policy Agenda Analysis - Full Pipeline\n")
cat("============================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Set working directory to repo root
repo_root <- "/home/user/replication-policy-agenda-hp-rt-26"
setwd(repo_root)
project_root <- file.path(repo_root, "analysis")
scripts_dir <- file.path(project_root, "scripts")

# Source all scripts in order
cat(">>> Sourcing 00_setup.R\n")
source(file.path(scripts_dir, "00_setup.R"))

cat("\n>>> Sourcing 01_construct_matrix.R\n")
source(file.path(scripts_dir, "01_construct_matrix.R"))

cat("\n>>> Sourcing 02_h1a_aggregate.R\n")
source(file.path(scripts_dir, "02_h1a_aggregate.R"))

cat("\n>>> Sourcing 03_h1b_decomposition.R\n")
source(file.path(scripts_dir, "03_h1b_decomposition.R"))

cat("\n>>> Sourcing 04_h2_crisis_milestone.R\n")
source(file.path(scripts_dir, "04_h2_crisis_milestone.R"))

# ============================================================================
# Generate summary.md
# ============================================================================
cat("\n>>> Generating summary.md\n")

# Reload all results
load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

table1 <- read_csv(file.path(tables_dir, "table1_h1a_descriptive.csv"),
                   show_col_types = FALSE)
table3 <- read_csv(file.path(tables_dir, "table3_core_peripheral_comparison.csv"),
                   show_col_types = FALSE)
table4 <- read_csv(file.path(tables_dir, "table4_year_type_comparison.csv"),
                   show_col_types = FALSE)

# Build summary
get_val <- function(stat) {
  row <- table1 %>% filter(Statistic == stat)
  if (nrow(row) == 0) return(NA_real_)
  row$Value[1]
}

summary_lines <- c(
  "# ASEAN Policy Agenda Analysis: Summary of Results",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Data",
  "",
  sprintf("- **Valid instruments:** %d", nrow(df)),
  sprintf("- **Policy areas:** %d", length(ALL_AREAS)),
  sprintf("- **Years:** %d-%d (%d years, %d with output)",
          min(ALL_YEARS), max(ALL_YEARS), length(ALL_YEARS),
          sum(matrix_wide$total > 0)),
  sprintf("- **Zero-output years:** %s", paste(ZERO_YEARS, collapse = ", ")),
  "",
  "## Parameter Choices",
  "",
  "- **Percentage changes:** Area-level, pooled across all 16 areas and 53 year-pairs",
  "- **Zero-handling rules:**",
  "  - 0 -> 0: assign 0",
  "  - 0 -> >0: exclude (undefined)",
  "  - >0 -> 0: assign -1 (100% decrease)",
  "  - >0 -> >0: standard formula",
  "- **Kurtosis convention:** Raw kurtosis (benchmark = 3)",
  "- **Core areas:** 10 (Transportation), 15 (Intra-ASEAN Trade), 20 (ASEAN Governance)",
  "- **2008 classification:** Included in both crisis and milestone categories",
  "- **Crisis windows (2-year):** 1997-98, 2004-05, 2008-09, 2020 (1-year only)",
  "- **Milestone windows (2-year):** 1976-77, 1992-93, 2007-08, 2015-16",
  "",
  "## H1a: Aggregate Punctuated Equilibrium",
  "",
  sprintf("- **N valid percentage changes:** %d", get_val("N (valid changes)")),
  sprintf("- **Mean:** %.4f", get_val("Mean")),
  sprintf("- **SD:** %.4f", get_val("Standard Deviation")),
  sprintf("- **Skewness:** %.4f", get_val("Skewness")),
  sprintf("- **Raw kurtosis:** %.4f (benchmark = 3)", get_val("Kurtosis (raw, benchmark=3)")),
  sprintf("- **Excess kurtosis:** %.4f", get_val("Excess Kurtosis (benchmark=0)")),
  sprintf("- **L-kurtosis (tau_4):** %.4f (benchmark = 0.1226)",
          get_val("L-kurtosis (tau_4, benchmark=0.123)")),
  sprintf("- **Shapiro-Wilk W:** %.6f, p = %.2e",
          get_val("Shapiro-Wilk W"), get_val("Shapiro-Wilk p-value")),
  sprintf("- **t-fit mu:** %.4f (SE = %.4f)",
          get_val("t-fit: mu (location)"), get_val("t-fit: SE(mu)")),
  sprintf("- **t-fit sigma:** %.4f (SE = %.4f)",
          get_val("t-fit: sigma (scale)"), get_val("t-fit: SE(sigma)")),
  sprintf("- **t-fit nu:** %.4f (SE = %.4f)",
          get_val("t-fit: nu (df)"), get_val("t-fit: SE(nu)")),
  "",
  "## H1b: Core-Periphery Decomposition",
  ""
)

# Add core vs peripheral comparison
for (i in seq_len(nrow(table3))) {
  row <- table3[i, ]
  summary_lines <- c(summary_lines,
    sprintf("### %s (n = %d)", row$Group, row$N),
    sprintf("- Kurtosis: %.4f | L-kurtosis: %.4f", row$Raw_Kurtosis, row$L_Kurtosis),
    sprintf("- Shapiro-Wilk: W = %.6f, p = %.2e", row$Shapiro_Wilk_W, row$Shapiro_Wilk_p),
    sprintf("- t-fit: mu = %.4f, sigma = %.4f, nu = %.4f (%s)",
            ifelse(is.na(row$t_mu), NA, row$t_mu),
            ifelse(is.na(row$t_sigma), NA, row$t_sigma),
            ifelse(is.na(row$t_nu), NA, row$t_nu),
            row$t_note),
    ""
  )
}

# Add variance decomposition
var_decomp <- read_csv(file.path(tables_dir, "table3b_variance_decomposition.csv"),
                       show_col_types = FALSE)
summary_lines <- c(summary_lines,
  "### Variance Decomposition",
  sprintf("- Total variance: %.4f", var_decomp$Variance[1]),
  sprintf("- Within-core: %.1f%% of total",
          100 * var_decomp$Proportion_of_Total[2]),
  sprintf("- Within-peripheral: %.1f%% of total",
          100 * var_decomp$Proportion_of_Total[3]),
  sprintf("- Between-group: %.1f%% of total",
          100 * var_decomp$Proportion_of_Total[5]),
  ""
)

# Add H2 comparison
summary_lines <- c(summary_lines,
  "## H2: Crisis vs. Milestone Change Patterns",
  "",
  "### Year-Type Comparison (2-year windows)",
  ""
)

for (i in seq_len(nrow(table4))) {
  row <- table4[i, ]
  summary_lines <- c(summary_lines,
    sprintf("**%s** (n = %d years):", row$year_type, row$n_years),
    sprintf("- Mean output: %.1f (SD = %.1f)", row$mean_total, row$sd_total),
    sprintf("- Mean active areas: %.1f", row$mean_active),
    sprintf("- Mean HHI: %.4f", row$mean_hhi),
    sprintf("- Mean peripheral share: %.1f%%", 100 * row$mean_periph_prop),
    sprintf("- Mean entropy: %.4f", row$mean_entropy),
    ""
  )
}

# Add caveats
summary_lines <- c(summary_lines,
  "## Caveats and Notes",
  "",
  "- **2008 overlap:** Year 2008 appears in both crisis (GFC, 2008-09) and milestone",
  "  (Charter, 2007-08) windows. It is counted in both groups for the aggregate comparison.",
  "- **2020 truncation:** COVID-19 window is one year only; no post-event comparison possible.",
  "- **1993 zero-output:** Within the AFTA milestone window (1992-93), 1993 had zero instruments.",
  "  This may reflect the pre-1995 pattern of sparse output rather than a milestone dynamic.",
  "- **2004-05 tsunami:** Trade integration output in 2004 (Framework Agreement annexes) is",
  "  not tsunami-related; disaster response (AADMER) appears in 2005.",
  "- **Small-sample t-distribution:** If the peripheral t-distribution fit failed to converge,",
  "  the kurtosis and L-kurtosis comparisons remain valid as distributional evidence.",
  ""
)

# Write summary
summary_path <- file.path(project_root, "summary.md")
writeLines(summary_lines, summary_path)
cat("Summary written to:", summary_path, "\n")

# ============================================================================
# Final verification
# ============================================================================
cat("\n============================================================\n")
cat("Output verification:\n")
cat("============================================================\n")

expected_files <- c(
  file.path(tables_dir, "year_area_matrix.csv"),
  file.path(tables_dir, "matrix_data.RData"),
  file.path(tables_dir, "pct_changes.RData"),
  file.path(tables_dir, "table1_h1a_descriptive.csv"),
  file.path(tables_dir, "table2_area_descriptives.csv"),
  file.path(tables_dir, "table3_core_peripheral_comparison.csv"),
  file.path(tables_dir, "table3b_variance_decomposition.csv"),
  file.path(tables_dir, "table4_year_type_comparison.csv"),
  file.path(tables_dir, "table5_event_profiles.csv"),
  file.path(tables_dir, "appendix_sensitivity_1yr.csv"),
  file.path(figures_dir, "fig1_histogram_pct_changes.png"),
  file.path(figures_dir, "fig1_histogram_pct_changes.pdf"),
  file.path(figures_dir, "fig2_entropy_core_share.png"),
  file.path(figures_dir, "fig2_entropy_core_share.pdf"),
  file.path(figures_dir, "fig3_output_by_year_type.png"),
  file.path(figures_dir, "fig3_output_by_year_type.pdf"),
  file.path(figures_dir, "fig4_hhi_peripheral_dotplot.png"),
  file.path(figures_dir, "fig4_hhi_peripheral_dotplot.pdf"),
  summary_path
)

all_ok <- TRUE
for (f in expected_files) {
  if (file.exists(f)) {
    cat(sprintf("  OK  %-60s (%s bytes)\n", basename(f), file.size(f)))
  } else {
    cat(sprintf("  MISSING  %s\n", f))
    all_ok <- FALSE
  }
}

cat("\n============================================================\n")
if (all_ok) {
  cat("All outputs generated successfully.\n")
} else {
  cat("WARNING: Some outputs are missing.\n")
}
cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")
