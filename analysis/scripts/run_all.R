###############################################################################
# run_all.R
# Master runner: executes all analysis scripts and generates summary.md
###############################################################################

cat("============================================================\n")
cat("ASEAN Policy Agenda Analysis - Full Pipeline\n")
cat("============================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

repo_root <- "/home/user/replication-policy-agenda-hp-rt-26"
setwd(repo_root)
project_root <- file.path(repo_root, "analysis")
scripts_dir <- file.path(project_root, "scripts")

# Source all scripts in order
cat(">>> Sourcing 00_setup.R\n")
source(file.path(scripts_dir, "00_setup.R"))

cat("\n>>> Sourcing 00_data_preparation.R\n")
source(file.path(scripts_dir, "00_data_preparation.R"))

cat("\n>>> Sourcing 01_figures_descriptive.R\n")
source(file.path(scripts_dir, "01_figures_descriptive.R"))

cat("\n>>> Sourcing 02_h1a_aggregate.R\n")
source(file.path(scripts_dir, "02_h1a_aggregate.R"))

cat("\n>>> Sourcing 03_h1b_decomposition.R\n")
source(file.path(scripts_dir, "03_h1b_decomposition.R"))

cat("\n>>> Sourcing 04_h2_crisis_milestone.R\n")
source(file.path(scripts_dir, "04_h2_crisis_milestone.R"))

cat("\n>>> Sourcing 05_robustness.R\n")
source(file.path(scripts_dir, "05_robustness.R"))

# ============================================================================
# Generate summary.md
# ============================================================================
cat("\n>>> Generating summary.md\n")

load(file.path(tables_dir, "matrix_data.RData"))
load(file.path(tables_dir, "pct_changes.RData"))

table1 <- read_csv(file.path(tables_dir, "table1_h1a_aggregate.csv"),
                   show_col_types = FALSE)
table3 <- read_csv(file.path(tables_dir, "table3_distributional_comparison.csv"),
                   show_col_types = FALSE)
table4 <- read_csv(file.path(tables_dir, "table4_year_type_comparison.csv"),
                   show_col_types = FALSE)
var_decomp <- read_csv(file.path(tables_dir, "table3b_variance_decomposition.csv"),
                       show_col_types = FALSE)

get_val <- function(stat, col = "Full_Distribution") {
  row <- table1 %>% filter(Statistic == stat)
  if (nrow(row) == 0) return(NA)
  row[[col]][1]
}

summary_lines <- c(
  "# ASEAN Policy Agenda Analysis: Summary of Results",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Data",
  "",
  sprintf("- **Total instruments:** 245 (1 unclassifiable, 244 area-assignable)"),
  sprintf("- **Policy areas:** %d active (of 22 in codebook)", length(ALL_AREAS)),
  sprintf("- **Years:** %d\u2013%d (%d years, %d with output)",
          min(ALL_YEARS), max(ALL_YEARS), length(ALL_YEARS),
          sum(matrix_wide$total > 0)),
  sprintf("- **Zero-output years:** %s", paste(ZERO_YEARS, collapse = ", ")),
  sprintf("- **Codebook verification:** floor(Sub/100) == Main for all rows"),
  "",
  "## Parameter Choices",
  "",
  "- **Percentage changes:** Area-level, pooled across 16 areas and 53 year-pairs",
  "- **Zero-handling rules:**",
  "  - 0 -> 0: assign 0 (included in full distribution, excluded in restricted)",
  "  - 0 -> >0: exclude (undefined)",
  "  - >0 -> 0: assign -1 (100% decrease)",
  "  - >0 -> >0: standard formula",
  "- **Full distribution:** N = 772 (incl. 642 zero-to-zero transitions)",
  "- **Restricted distribution:** N = 130 (excl. zero-to-zero)",
  "- **Core areas:** 10 (Transportation), 15 (Intra-ASEAN Trade), 20 (ASEAN Governance)",
  "- **2008 classification:** Milestone only (Charter preceded GFC; overlap flagged)",
  "- **Crisis windows:** 1997-98, 2004-05, 2009, 2020",
  "- **Milestone windows:** 1976-77, 1992-93, 2007-08, 2015-16",
  "",
  "## H1a: Aggregate Punctuated Equilibrium",
  "",
  "### Full Distribution",
  sprintf("- **N:** %s", get_val("N")),
  sprintf("- **Mean:** %s, **Median:** %s, **SD:** %s",
          get_val("Mean"), get_val("Median"), get_val("Standard Deviation")),
  sprintf("- **Raw kurtosis:** %s (benchmark = 3)", get_val("Raw Kurtosis (benchmark=3)")),
  sprintf("- **L-kurtosis:** %s (benchmark = 0.1226)",
          get_val("L-kurtosis (tau_4, benchmark=0.123)")),
  sprintf("- **Shapiro-Wilk:** W = %s, p = %s",
          get_val("Shapiro-Wilk W"), get_val("Shapiro-Wilk p-value")),
  sprintf("- **t-fit:** mu = %s, sigma = %s, nu = %s",
          get_val("t-fit: mu (SE)"), get_val("t-fit: sigma (SE)"),
          get_val("t-fit: nu (SE)")),
  sprintf("- **Note:** %s", get_val("t-fit: Note")),
  "",
  "### Restricted Distribution (excl. 0-to-0)",
  sprintf("- **N:** %s", get_val("N", "Restricted_Distribution")),
  sprintf("- **Mean:** %s, **SD:** %s",
          get_val("Mean", "Restricted_Distribution"),
          get_val("Standard Deviation", "Restricted_Distribution")),
  sprintf("- **Raw kurtosis:** %s",
          get_val("Raw Kurtosis (benchmark=3)", "Restricted_Distribution")),
  sprintf("- **L-kurtosis:** %s",
          get_val("L-kurtosis (tau_4, benchmark=0.123)", "Restricted_Distribution")),
  sprintf("- **t-fit:** mu = %s, sigma = %s, nu = %s",
          get_val("t-fit: mu (SE)", "Restricted_Distribution"),
          get_val("t-fit: sigma (SE)", "Restricted_Distribution"),
          get_val("t-fit: nu (SE)", "Restricted_Distribution")),
  sprintf("- **Note:** %s", get_val("t-fit: Note", "Restricted_Distribution")),
  "",
  "## H1b: Core-Periphery Decomposition",
  "",
  "### Distributional Comparison (Table 3)",
  ""
)

# Add Table 3 rows
for (i in seq_len(nrow(table3))) {
  row <- table3[i, ]
  summary_lines <- c(summary_lines,
    sprintf("**%s:** Agg = %s | Core = %s | Periph = %s",
            row$Metric, row$Aggregate, row$Core, row$Peripheral)
  )
}

summary_lines <- c(summary_lines,
  "",
  "### Variance Decomposition",
  sprintf("- Total variance: %.4f", var_decomp$Variance[1]),
  sprintf("- Within-core: %.1f%% of total",
          100 * var_decomp$Proportion_of_Total[2]),
  sprintf("- Within-peripheral: %.1f%% of total",
          100 * var_decomp$Proportion_of_Total[3]),
  sprintf("- Between-group: %.1f%% of total",
          100 * var_decomp$Proportion_of_Total[5]),
  "",
  "## H2: Crisis vs. Milestone Change Patterns",
  "",
  "### Year-Type Comparison",
  ""
)

for (i in seq_len(nrow(table4))) {
  row <- table4[i, ]
  summary_lines <- c(summary_lines,
    sprintf("**%s** (n = %d): mean output = %.1f (SD = %.1f), active areas = %.1f, HHI = %.4f, entropy = %.4f",
            row$year_type, row$n_years, row$mean_total, row$sd_total,
            row$mean_active, row$mean_hhi, row$mean_entropy)
  )
}

summary_lines <- c(summary_lines,
  "",
  "## Caveats and Notes",
  "",
  "- **2008 overlap:** 2007-08 is both the Charter milestone and the onset of the GFC.",
  "  Classified as milestone since the Charter signing preceded the GFC.",
  "  The GFC crisis window is 2009 only.",
  "- **2020 truncation:** COVID-19 window is one year only; no post-event comparison.",
  "- **1993 zero-output:** Within the AFTA milestone window, 1993 had zero instruments.",
  "- **2004-05 tsunami:** Trade output in 2004 is not tsunami-related; AADMER in 2005.",
  "- **T-distribution fitting:** Degenerate fits (sigma near 0) reflect the data's",
  "  discrete structure (point masses at 0 and -1). The restricted distribution",
  "  (excluding 0-to-0) provides more meaningful continuous distribution fits.",
  "- **Observation count:** The full distribution has 772 observations (16 areas x 53",
  "  year-pairs, minus 76 excluded). The restricted distribution (130 obs) better",
  "  reflects the subset with actual policy changes.",
  ""
)

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
  file.path(tables_dir, "entropy_data.RData"),
  file.path(tables_dir, "tfit_aggregate.RData"),
  file.path(tables_dir, "table1_h1a_aggregate.csv"),
  file.path(tables_dir, "table2_area_descriptives.csv"),
  file.path(tables_dir, "table3_distributional_comparison.csv"),
  file.path(tables_dir, "table3b_variance_decomposition.csv"),
  file.path(tables_dir, "table4_year_type_comparison.csv"),
  file.path(tables_dir, "table5_event_profiles.csv"),
  file.path(tables_dir, "appendix_core_threshold_sensitivity.csv"),
  file.path(tables_dir, "appendix_period_effects.csv"),
  file.path(tables_dir, "appendix_sensitivity_1yr.csv"),
  file.path(figures_dir, "fig1_attention_shares.png"),
  file.path(figures_dir, "fig1_attention_shares.pdf"),
  file.path(figures_dir, "fig2_timeline_annotated.png"),
  file.path(figures_dir, "fig2_timeline_annotated.pdf"),
  file.path(figures_dir, "fig3_histogram_pct_changes.png"),
  file.path(figures_dir, "fig3_histogram_pct_changes.pdf"),
  file.path(figures_dir, "fig4_core_entropy_timeseries.png"),
  file.path(figures_dir, "fig4_core_entropy_timeseries.pdf"),
  file.path(figures_dir, "fig5_yeartype_comparison.png"),
  file.path(figures_dir, "fig5_yeartype_comparison.pdf"),
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
  cat("All", length(expected_files), "outputs generated successfully.\n")
} else {
  cat("WARNING: Some outputs are missing.\n")
}
cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")
