###############################################################################
# 03_table1_crisis_milestone.R
# Table 1 — Crisis and milestone classifications
###############################################################################

cat("\n=== 03_table1_crisis_milestone.R ===\n")

if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

table1 <- tibble(
  Event = c(
    "Asian Financial Crisis",
    "Indian Ocean Tsunami",
    "Global Financial Crisis / Cyclone Nargis",
    "COVID-19",
    "Treaty of Amity / First Summit",
    "AFTA Agreement",
    "ASEAN Charter",
    "ASEAN Community"
  ),
  Type = c(rep("Crisis", 4), rep("Milestone", 4)),
  Window = c(
    "1997-1998",
    "2004-2005",
    "2008-2009",
    "2020",
    "1976-1977",
    "1992-1993",
    "2007-2008",
    "2015-2016"
  ),
  Rationale = c(
    "Regional financial contagion; IMF interventions across ASEAN",
    "Tsunami devastated ASEAN members; AADMER adopted 2005",
    "Global recession + Cyclone Nargis (2008); 2008 classified as milestone for year-type analysis",
    "Pandemic; data ends 2020, no post-event comparison",
    "TAC signed 1976; first ASEAN Summit",
    "AFTA framework adopted; 1993 has zero output",
    "Charter signed Nov 2007, entered force Dec 2008; 2007-08 overlap with GFC onset",
    "ASEAN Community established Dec 2015"
  )
)

cat("\nTable 1 — Crisis and milestone classifications:\n")
print(table1, width = 120)

table1_path <- file.path(tables_dir, "table1_classifications.csv")
write_csv(table1, table1_path)
verify_output(table1_path)

cat("\n03_table1_crisis_milestone.R completed.\n")
