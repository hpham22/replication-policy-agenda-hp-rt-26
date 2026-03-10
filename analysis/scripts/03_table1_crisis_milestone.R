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
    "ASEAN Free Trade Area Agreement",
    "ASEAN Charter",
    "ASEAN Community"
  ),
  Type = c(rep("Crisis", 4), rep("Milestone", 4)),
  Window = c(
    "1997-98",
    "2004-05",
    "2008-09",
    "2020",
    "1976-77",
    "1992-93",
    "2007-08",
    "2015-16"
  ),
  Rationale = c(
    "Regional economic collapse; triggered institutional reform",
    "Transboundary disaster; AADMER adopted 2005",
    "Global economic shock; humanitarian crisis in Myanmar",
    "Pandemic; truncated (data ends 2020)",
    "First Summit; TAC and Declaration of ASEAN Concord",
    "Free trade area established",
    "Constitutional document; entered force Dec 2008",
    "Formal community inauguration"
  )
)

cat("\nTable 1 — Crisis and milestone classifications:\n")
print(table1, width = 120)

table1_path <- file.path(tables_dir, "table1_classifications.csv")
write_csv(table1, table1_path)
verify_output(table1_path)

# --- kableExtra output ---
save_table(
  table1, "table1_classifications",
  caption = "Table 1. Crisis and milestone classifications"
)

cat("\n03_table1_crisis_milestone.R completed.\n")
