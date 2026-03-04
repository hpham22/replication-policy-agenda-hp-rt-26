###############################################################################
# 01_construct_matrix.R
# Step 0: Construct the Year-by-Area Matrix
# Creates a 54x16 matrix (years 1967-2020, 16 policy areas)
###############################################################################

cat("\n=== 01_construct_matrix.R ===\n")

# Source setup if not already loaded
if (!exists("ALL_YEARS")) source(file.path(dirname(sys.frame(1)$ofile %||% "scripts"), "00_setup.R"))

# --- Read data ---
cat("Reading data from:", data_file, "\n")
raw <- read_excel(data_file, sheet = 1)

# Trim column names (trailing spaces)
names(raw) <- trimws(names(raw))
cat("Columns:", paste(names(raw), collapse = ", "), "\n")
cat("Raw rows:", nrow(raw), "\n")

# --- Clean and filter ---
df <- raw %>%
  filter(!is.na(Instrument)) %>%
  filter(`Main-Policy Area` != "-") %>%
  mutate(
    main_area = as.integer(`Main-Policy Area`),
    year      = as.integer(`Year of Adoption`)
  ) %>%
  filter(!is.na(main_area), !is.na(year))

cat("Valid instruments after filtering:", nrow(df), "\n")

# Verify area codes
actual_areas <- sort(unique(df$main_area))
cat("Policy areas found:", paste(actual_areas, collapse = ", "), "\n")
stopifnot(setequal(actual_areas, ALL_AREAS))

# Verify year range
cat("Year range:", min(df$year), "-", max(df$year), "\n")
stopifnot(min(df$year) >= 1967, max(df$year) <= 2020)

# --- Build year-by-area matrix ---
# Count instruments per year and area
year_area_counts <- df %>%
  count(year, main_area, name = "count")

# Create complete grid and fill zeros
year_area_long <- expand_grid(
  year      = ALL_YEARS,
  main_area = ALL_AREAS
) %>%
  left_join(year_area_counts, by = c("year", "main_area")) %>%
  replace_na(list(count = 0L))

# Pivot to wide format
matrix_wide <- year_area_long %>%
  pivot_wider(
    names_from  = main_area,
    values_from = count,
    names_prefix = "area_"
  ) %>%
  mutate(total = rowSums(across(starts_with("area_"))))

cat("\nMatrix dimensions:", nrow(matrix_wide), "rows x", ncol(matrix_wide), "cols\n")

# --- Verification ---
# Check total matches
total_from_matrix <- sum(matrix_wide$total)
cat("Total instruments in matrix:", total_from_matrix, "\n")
cat("Total instruments in data:  ", nrow(df), "\n")
stopifnot(total_from_matrix == nrow(df))

# Check zero-output years
zero_check <- matrix_wide %>% filter(total == 0) %>% pull(year)
cat("Zero-output years:", paste(sort(zero_check), collapse = ", "), "\n")
stopifnot(setequal(zero_check, ZERO_YEARS))

# Annual totals summary
cat("\nAnnual output summary:\n")
cat("  Min:", min(matrix_wide$total), "\n")
cat("  Max:", max(matrix_wide$total), "\n")
cat("  Mean:", round(mean(matrix_wide$total), 2), "\n")
cat("  Years with output:", sum(matrix_wide$total > 0), "\n")

# --- Save outputs ---
# CSV
csv_path <- file.path(tables_dir, "year_area_matrix.csv")
write_csv(matrix_wide, csv_path)
verify_output(csv_path)

# RData for downstream scripts
rdata_path <- file.path(tables_dir, "matrix_data.RData")
save(df, matrix_wide, year_area_long, file = rdata_path)
verify_output(rdata_path)

cat("\n01_construct_matrix.R completed.\n")
