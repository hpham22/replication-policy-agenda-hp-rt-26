###############################################################################
# 00_data_preparation.R
# Step 0: Data loading, cleaning, codebook verification, matrix construction,
#         and percentage change computation
###############################################################################

cat("\n=== 00_data_preparation.R ===\n")

if (!exists("ALL_YEARS")) source("00_setup.R")

# ============================================================================
# 0.1 Read and clean the dataset
# ============================================================================
cat("\n--- 0.1: Read and clean dataset ---\n")

df_raw <- read_excel(data_file, sheet = 1)
names(df_raw) <- trimws(names(df_raw))
cat("Raw rows read:", nrow(df_raw), "\n")

# Identify the dash-coded instrument
dash_rows <- which(df_raw$`Main-Policy Area` == "-")
cat("Dash-coded instruments (unclassifiable):", length(dash_rows),
    "at row(s):", dash_rows, "\n")

# Working dataset: exclude dash but note total is 245
df <- df_raw %>%
  filter(!is.na(Instrument),
         `Main-Policy Area` != "-",
         !is.na(`Main-Policy Area`)) %>%
  mutate(
    main_area = as.integer(`Main-Policy Area`),
    year      = as.integer(`Year of Adoption`)
  ) %>%
  filter(!is.na(main_area), !is.na(year))

cat("Total instruments (incl dash):", nrow(df_raw), "\n")
cat("Area-assignable instruments:", nrow(df), "\n")

# ============================================================================
# 0.2 Codebook verification
# ============================================================================
cat("\n--- 0.2: Codebook verification ---\n")

df <- df %>%
  mutate(
    sub_area     = as.integer(`Sub-Policy Area`),
    derived_main = floor(sub_area / 100)
  )

n_mismatch <- sum(df$derived_main != df$main_area, na.rm = TRUE)
cat(sprintf("Codebook check: %d/%d rows pass floor(Sub/100) == Main\n",
            nrow(df) - n_mismatch, nrow(df)))
if (n_mismatch > 0) {
  warning(sprintf("%d rows FAIL codebook verification:", n_mismatch))
  print(df %>% filter(derived_main != main_area) %>%
          select(Instrument, sub_area, main_area, derived_main))
}

# Report active vs zero-instrument topics
active_topics <- sort(unique(df$main_area))
all_codebook <- as.integer(names(CODEBOOK))
zero_topics <- setdiff(all_codebook, active_topics)
cat("Active policy areas:", length(active_topics), "\n")
cat("Zero-instrument topics:", paste(zero_topics, collapse = ", "),
    "(", paste(CODEBOOK[as.character(zero_topics)], collapse = ", "), ")\n")

# ============================================================================
# 0.3 Construct year-by-main-topic matrix
# ============================================================================
cat("\n--- 0.3: Construct year-by-area matrix ---\n")

# Create complete grid
grid <- expand_grid(year = ALL_YEARS, main_area = ALL_AREAS)

# Count instruments per year x area
counts <- df %>%
  count(year, main_area, name = "count")

# Join and fill zeros
year_area_long <- grid %>%
  left_join(counts, by = c("year", "main_area")) %>%
  replace_na(list(count = 0))

# Pivot to wide format
area_cols <- paste0("area_", ALL_AREAS)
matrix_wide <- year_area_long %>%
  mutate(col_name = paste0("area_", main_area)) %>%
  pivot_wider(id_cols = year, names_from = col_name, values_from = count,
              values_fill = 0) %>%
  select(year, all_of(area_cols)) %>%
  mutate(total = rowSums(across(all_of(area_cols))))

# Verification
total_instruments <- sum(matrix_wide$total)
cat("Matrix dimensions:", nrow(matrix_wide), "years x", length(area_cols), "areas\n")
cat("Total instruments in matrix:", total_instruments, "\n")
stopifnot(total_instruments == nrow(df))

# Verify key area counts
area_totals <- colSums(matrix_wide[, area_cols])
cat("\nTop area counts:\n")
sorted_areas <- sort(area_totals, decreasing = TRUE)
for (i in seq_len(min(5, length(sorted_areas)))) {
  area_code <- gsub("area_", "", names(sorted_areas)[i])
  cat(sprintf("  Area %s (%s): %d\n", area_code,
              AREA_LABELS[area_code], sorted_areas[i]))
}

# Verify zero-output years
zero_output_years <- matrix_wide$year[matrix_wide$total == 0]
cat("\nZero-output years:", paste(zero_output_years, collapse = ", "), "\n")
if (!setequal(zero_output_years, ZERO_YEARS)) {
  warning("Zero-output years don't match expected ZERO_YEARS constant!")
}
cat("Years with output:", sum(matrix_wide$total > 0), "of", nrow(matrix_wide), "\n")

# Save matrix
matrix_path <- file.path(tables_dir, "year_area_matrix.csv")
write_csv(matrix_wide, matrix_path)
verify_output(matrix_path)

# ============================================================================
# 0.4-0.5: Area and year classifications (defined in 00_setup.R)
# ============================================================================
cat("\n--- 0.4-0.5: Classifications ---\n")
cat("Core areas:", paste(CORE_AREAS, collapse = ", "), "\n")
cat("Peripheral areas:", paste(PERIPHERAL_AREAS, collapse = ", "), "\n")
cat("Crisis years:", paste(CRISIS_YEARS, collapse = ", "), "\n")
cat("Milestone years:", paste(MILESTONE_YEARS, collapse = ", "), "\n")
cat("Note: 2008 classified as milestone only (Charter preceded GFC)\n")

# ============================================================================
# Compute percentage changes
# ============================================================================
cat("\n--- Computing percentage changes ---\n")

all_area_pct <- map_dfr(ALL_AREAS, function(area) {
  area_data <- year_area_long %>%
    filter(main_area == area) %>%
    arrange(year)
  pct <- compute_pct_changes(area_data$count, area_data$year)
  pct$main_area <- area
  pct
})

# Full distribution (including zero-to-zero, excluding undefined)
pooled_full <- all_area_pct %>%
  filter(!is.na(pct_change)) %>%
  pull(pct_change)

# Restricted distribution (excluding zero-to-zero)
pooled_restricted <- all_area_pct %>%
  filter(!is.na(pct_change), status != "zero_to_zero") %>%
  pull(pct_change)

cat("Total year-pairs:", nrow(all_area_pct), "\n")
cat("Status breakdown:\n")
print(table(all_area_pct$status))
cat("\nFull distribution (incl 0-to-0):", length(pooled_full), "\n")
cat("Restricted (excl 0-to-0):", length(pooled_restricted), "\n")

# Save
save(df, matrix_wide, year_area_long, area_cols,
     file = file.path(tables_dir, "matrix_data.RData"))
save(all_area_pct, pooled_full, pooled_restricted,
     file = file.path(tables_dir, "pct_changes.RData"))

cat("\n00_data_preparation.R completed.\n")
