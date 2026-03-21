###############################################################################
# run_all.R
# Master runner: executes all analysis scripts and generates summary.md
###############################################################################

cat("============================================================\n")
cat("ASEAN Policy Agenda Analysis - Full Pipeline\n")
cat("============================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

repo_root <- getwd()

# Source all scripts in order
scripts <- c(
  "00_setup.R",
  "00_data_preparation.R",
  "01_fig1_attention_distribution.R",
  "02_fig2_annual_timeline.R",
  "03_table1_crisis_milestone.R",
  "04_h1a_aggregate.R",
  "05_h1b_decomposition.R",
  "06_h2_crisis_milestone.R",
  "07_appendix_b_robustness.R"
)

for (s in scripts) {
  cat(sprintf("\n>>> Sourcing %s\n", s))
  source(s)
}

# ============================================================================
# Generate summary.md
# ============================================================================
cat("\n>>> Generating summary.md\n")

load(file.path(results_dir, "matrix_data.RData"))
load(file.path(results_dir, "pct_changes.RData"))

table2 <- read_csv(file.path(results_dir, "table2_aggregate_stats.csv"),
                   show_col_types = FALSE)

get_val <- function(stat, col = "Restricted (primary)") {
  row <- table2 %>% filter(Statistic == stat)
  if (nrow(row) == 0) return("NA")
  row[[col]][1]
}

summary_lines <- c(
  "# ASEAN Policy Agenda Analysis: Summary of Results",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Data",
  "",
  "- **Total instruments:** 245 (1 unclassifiable, 244 area-assignable)",
  sprintf("- **Policy areas:** %d active (of 22 in codebook)", length(ALL_AREAS)),
  sprintf("- **Years:** %d-%d (%d years, %d with output)",
          min(ALL_YEARS), max(ALL_YEARS), length(ALL_YEARS),
          sum(matrix_wide$total > 0)),
  sprintf("- **Zero-output years:** %s", paste(ZERO_YEARS, collapse = ", ")),
  "",
  "## Parameter Choices",
  "",
  "- **Primary distribution:** Restricted (excl. 0-to-0), N = 130",
  "- **Full distribution (robustness):** N = 772",
  "- **Core areas:** 10 (Transportation), 15 (Intra-ASEAN Trade), 20 (ASEAN Governance)",
  "- **2008 classification:** Milestone only (Charter preceded GFC)",
  "- **Crisis windows:** 1997-98, 2004-05, 2009, 2020",
  "- **Milestone windows:** 1976-77, 1992-93, 2007-08, 2015-16",
  "- **Period effects cutoff:** Pre-Charter <=2008 / Post-Charter >=2009",
  "",
  "## H1a: Aggregate Punctuated Equilibrium",
  "",
  sprintf("- **N:** %s", get_val("N")),
  sprintf("- **Raw kurtosis:** %s (benchmark = 3)",
          get_val("Raw Kurtosis (benchmark=3)")),
  sprintf("- **L-kurtosis:** %s (benchmark = 0.1226)",
          get_val("L-kurtosis (tau_4, benchmark=0.123)")),
  sprintf("- **Shapiro-Wilk p:** %s", get_val("Shapiro-Wilk p-value")),
  "",
  "## Output Files",
  "",
  "### Main Tables",
  "- Table 1: table1_classifications.csv",
  "- Table 2: table2_aggregate_stats.csv",
  "- Table 3: table3_area_descriptives.csv",
  "- Table 4: table4_distributional_comparison.csv",
  "- Table 5: table5_year_type_comparison.csv",
  "- Table 6: table6_event_profiles.csv",
  "",
  "### Main Figures",
  "- Figure 1: fig1_attention_distribution.pdf",
  "- Figure 2: fig2_annual_timeline.pdf",
  "- Figure 3: fig3_pct_change_distribution.pdf",
  "- Figure 4: fig4_faceted_distributions.pdf",
  "",
  "### Appendix Tables",
  "- Table B1: tableB1_full_vs_restricted.csv",
  "- Table B2: tableB2_pctpct_comparison.csv",
  "- Table B3: tableB3_core_threshold.csv",
  "- Table B4: tableB4_1yr_windows.csv",
  "- Table B5: tableB5_period_effects.csv",
  "- Table B6: tableB6_tfit_details.csv",
  "",
  "### Appendix Figures",
  "- Figure B1: figB1_pctpct_histogram.pdf",
  "- fig_appendix_core_entropy.pdf",
  "- fig_appendix_yeartype_bars.pdf",
  ""
)

summary_path <- file.path(repo_root, "summary.md")
writeLines(summary_lines, summary_path)
cat("Summary written to:", summary_path, "\n")

# ============================================================================
# Final verification
# ============================================================================
cat("\n============================================================\n")
cat("Output verification:\n")
cat("============================================================\n")

expected_files <- c(
  # Data files
  file.path(results_dir, "year_area_matrix.csv"),
  file.path(results_dir, "matrix_data.RData"),
  file.path(results_dir, "pct_changes.RData"),
  file.path(results_dir, "entropy_data.RData"),
  file.path(results_dir, "tfit_aggregate.RData"),
  # Main tables
  file.path(results_dir, "table1_classifications.csv"),
  file.path(results_dir, "table2_aggregate_stats.csv"),
  file.path(results_dir, "table3_area_descriptives.csv"),
  file.path(results_dir, "table4_distributional_comparison.csv"),
  file.path(results_dir, "table5_year_type_comparison.csv"),
  file.path(results_dir, "table6_event_profiles.csv"),
  # Appendix tables
  file.path(results_dir, "tableB1_full_vs_restricted.csv"),
  file.path(results_dir, "tableB2_pctpct_comparison.csv"),
  file.path(results_dir, "tableB3_core_threshold.csv"),
  file.path(results_dir, "tableB4_1yr_windows.csv"),
  file.path(results_dir, "tableB5_period_effects.csv"),
  file.path(results_dir, "tableB6_tfit_details.csv"),
  # Main figures
  file.path(results_dir, "fig1_attention_distribution.pdf"),
  file.path(results_dir, "fig2_annual_timeline.pdf"),
  file.path(results_dir, "fig3_pct_change_distribution.pdf"),
  file.path(results_dir, "fig4_faceted_distributions.pdf"),
  # Appendix figures
  file.path(results_dir, "fig_appendix_core_entropy.pdf"),
  file.path(results_dir, "fig_appendix_yeartype_bars.pdf"),
  file.path(results_dir, "figB1_pctpct_histogram.pdf"),
  # Summary
  summary_path
)

all_ok <- TRUE
for (f in expected_files) {
  if (file.exists(f)) {
    cat(sprintf("  OK  %-50s (%s bytes)\n", basename(f), file.size(f)))
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
