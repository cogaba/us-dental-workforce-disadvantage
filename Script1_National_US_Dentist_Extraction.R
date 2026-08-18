# =============================================================================
# NPI Data Extraction: Active Dentists in the United States (50 States + DC)
# Study: National Dental Workforce & Social Deprivation
#
# Source:  NPPES Full Replacement Monthly File (July 2026)
#          npidata_pfile_20050523-20260712.csv
#
# Filters Applied:
#   1. Entity Type = 1 (individual providers only)
#   2. No NPI Deactivation Date (active providers only)
#   3. At least one of 14 ADA dental taxonomy codes
#   4. Practice state in 50 states + DC
#      (territories PR/GU/VI/AS/MP and military codes AA/AE/AP are counted
#       and reported for the STROBE flow diagram, then excluded)
#
# Taxonomy Reference: NUCC Health Care Provider Taxonomy v26.0 (Jan 1, 2026)
#   -> confirm version number against taxonomy.nucc.org for the July file
#
# Output Files (saved to Extraction directory):
#   US_Dentist_Count_Validation.csv   — summary count by taxonomy
#   US_Dentist_Count_by_State.csv     — count by state (for ADA HPI validation)
#   US_Dentists_NPI_Final.csv         — full filtered provider records with ZIP
#   US_Extraction_Exclusion_Log.csv   — excluded territory/military/missing-state counts
#
# =============================================================================

library(readr)
library(dplyr)

# -----------------------------------------------------------------------------
# 0. PATHS
# -----------------------------------------------------------------------------
# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"

input_file <- file.path(base_dir, "NPPES_Data_Dissemination_July_2026_V2",
                        "npidata_pfile_20050523-20260712.csv")
output_dir <- file.path(base_dir, "Extraction")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. TAXONOMY CODE DATA DICTIONARY
#    Source: NUCC Health Care Provider Taxonomy Code Set v26.0
#    14 codes: 2 General Dentistry + 12 ADA-recognized Specialties
#    (unchanged from California paper)
# -----------------------------------------------------------------------------
dental_taxonomy <- tibble::tribble(
  ~taxonomy_code,   ~taxonomy_label,                                  ~category_type,
  "122300000X",     "General Dentist",                                "General Dentistry",
  "1223G0001X",     "General Practice",                               "General Dentistry",
  "1223D0004X",     "Dental Anesthesiology",                          "Specialty",
  "1223D0001X",     "Dental Public Health",                           "Specialty",
  "1223E0200X",     "Endodontics",                                    "Specialty",
  "1223P0106X",     "Oral and Maxillofacial Pathology",               "Specialty",
  "1223X0008X",     "Oral and Maxillofacial Radiology",               "Specialty",
  "1223S0112X",     "Oral and Maxillofacial Surgery",                 "Specialty",
  "125Q00000X",     "Oral Medicine",                                  "Specialty",
  "1223X2210X",     "Orofacial Pain",                                 "Specialty",
  "1223X0400X",     "Orthodontics and Dentofacial Orthopedics",       "Specialty",
  "1223P0221X",     "Pediatric Dentistry",                            "Specialty",
  "1223P0300X",     "Periodontics",                                   "Specialty",
  "1223P0700X",     "Prosthodontics",                                 "Specialty"
)

dental_taxonomy_codes <- dental_taxonomy$taxonomy_code

# -----------------------------------------------------------------------------
# 1b. STATE INCLUSION LIST — 50 states + DC
#     Territories and military mail codes are tallied, then excluded
# -----------------------------------------------------------------------------
included_states <- c(state.abb, "DC")
territory_codes <- c("PR", "GU", "VI", "AS", "MP")
military_codes  <- c("AA", "AE", "AP")

# -----------------------------------------------------------------------------
# 2. READ FILE HEADER — identify exact column names
# -----------------------------------------------------------------------------
cat("Reading column names from file header...\n")
header_cols <- names(read_csv(input_file, n_max = 0, show_col_types = FALSE))
cat(sprintf("  Total columns in source file: %d\n", length(header_cols)))

# Taxonomy code columns (up to 15 in NPPES)
taxonomy_col_names <- grep("Healthcare Provider Taxonomy Code_", header_cols, value = TRUE)
cat(sprintf("  Taxonomy code columns found: %d\n", length(taxonomy_col_names)))

# Define key columns
col_npi          <- "NPI"
col_entity       <- "Entity Type Code"
col_deactivation <- "NPI Deactivation Date"
col_state        <- "Provider Business Practice Location Address State Name"
col_zip          <- "Provider Business Practice Location Address Postal Code"
col_country      <- "Provider Business Practice Location Address Country Code (If outside U.S.)"
col_enum_date    <- "Provider Enumeration Date"
col_update_date  <- "Last Update Date"
# enum/update dates retained now so the "recently updated NPI" sensitivity
# analysis will not require re-reading the 11 GB source file later

# Validate all required columns exist
required_cols <- c(col_npi, col_entity, col_deactivation, col_state, col_zip,
                   col_country, col_enum_date, col_update_date)
missing_cols  <- setdiff(required_cols, header_cols)
if (length(missing_cols) > 0) {
  stop(paste("MISSING EXPECTED COLUMNS:", paste(missing_cols, collapse = ", ")))
}
cat("  All required columns confirmed present.\n")

# Columns to retain in output
cols_to_read <- c(required_cols, taxonomy_col_names)

# -----------------------------------------------------------------------------
# 3. CHUNKED READ & FILTER
#    Processes file in 500,000-row chunks to manage RAM
#    Reduce chunk_size to 250000 if memory issues occur
#    NOTE: taxonomy filter now runs BEFORE the state filter so that
#    territory/military/missing-state dental providers can be counted
#    for the STROBE flow diagram before exclusion
# -----------------------------------------------------------------------------
chunk_size      <- 500000
us_dentists     <- list()
chunk_num       <- 0
total_rows_read <- 0

# exclusion tallies (dental providers only)
n_territory     <- 0
n_military      <- 0
n_foreign_other <- 0
n_missing_state <- 0

cat(sprintf("\nProcessing source file in chunks of %s rows...\n",
            format(chunk_size, big.mark = ",")))

read_csv_chunked(
  file     = input_file,
  callback = DataFrameCallback$new(function(chunk, pos) {

    chunk_num       <<- chunk_num + 1
    total_rows_read <<- total_rows_read + nrow(chunk)

    # Retain only needed columns
    chunk <- chunk[, intersect(cols_to_read, names(chunk)), drop = FALSE]

    # Filter 1: Individual providers only (Entity Type = 1)
    chunk <- chunk %>%
      filter(`Entity Type Code` == 1)

    # Filter 2: Active NPIs only (no deactivation date)
    chunk <- chunk %>%
      filter(is.na(`NPI Deactivation Date`) | trimws(`NPI Deactivation Date`) == "")

    # Filter 3: At least one matching dental taxonomy code
    tax_matrix     <- chunk %>%
      select(all_of(taxonomy_col_names)) %>%
      mutate(across(everything(), ~ trimws(.x) %in% dental_taxonomy_codes))
    has_dental_tax <- rowSums(tax_matrix, na.rm = TRUE) > 0
    chunk          <- chunk[has_dental_tax, ]

    # Filter 4: Practice state in 50 states + DC, with exclusion tallies
    if (nrow(chunk) > 0) {
      st <- trimws(toupper(
        chunk$`Provider Business Practice Location Address State Name`))

      n_territory     <<- n_territory     + sum(st %in% territory_codes, na.rm = TRUE)
      n_military      <<- n_military      + sum(st %in% military_codes,  na.rm = TRUE)
      n_missing_state <<- n_missing_state + sum(is.na(st) | st == "")
      n_foreign_other <<- n_foreign_other +
        sum(!(st %in% c(included_states, territory_codes, military_codes)) &
              !is.na(st) & st != "")

      chunk <- chunk[st %in% included_states & !is.na(st), ]
    }

    if (nrow(chunk) > 0) us_dentists[[chunk_num]] <<- chunk

    if (chunk_num %% 10 == 0) {
      cat(sprintf("  Chunk %3d | Rows read: %10s | US dentists found: %s\n",
                  chunk_num,
                  format(total_rows_read, big.mark = ","),
                  format(sum(sapply(us_dentists, nrow)), big.mark = ",")))
    }
  }),
  chunk_size     = chunk_size,
  col_types      = cols(.default = col_character()),
  show_col_types = FALSE
)

cat(sprintf("\nChunked read complete. Total rows read: %s\n",
            format(total_rows_read, big.mark = ",")))

# -----------------------------------------------------------------------------
# 4. COMBINE & DEDUPLICATE
# -----------------------------------------------------------------------------
cat("Combining chunks...\n")
us_df <- bind_rows(us_dentists)
rm(us_dentists); gc()

cat(sprintf("  Rows before deduplication: %s\n", format(nrow(us_df), big.mark = ",")))
us_df <- us_df %>% distinct(NPI, .keep_all = TRUE)
cat(sprintf("  Rows after deduplication (unique NPIs): %s\n", format(nrow(us_df), big.mark = ",")))

# -----------------------------------------------------------------------------
# 5. CLEAN ZIP CODE
#    NPPES stores ZIP as 9-digit (XXXXX-XXXX) or 5-digit
#    Extract both 5-digit and retain 9-digit for reference
# -----------------------------------------------------------------------------
us_df <- us_df %>%
  rename(
    zip9  = `Provider Business Practice Location Address Postal Code`,
    state = `Provider Business Practice Location Address State Name`
  ) %>%
  mutate(
    state = trimws(toupper(state)),
    zip9  = trimws(zip9),
    zip5  = substr(zip9, 1, 5)
  )

# Report missing ZIPs
n_missing_zip <- sum(is.na(us_df$zip5) | us_df$zip5 == "" | nchar(us_df$zip5) < 5)
cat(sprintf("  Records missing or incomplete ZIP: %s (%.1f%%)\n",
            n_missing_zip,
            100 * n_missing_zip / nrow(us_df)))

# -----------------------------------------------------------------------------
# 6. CLASSIFY BY PRIMARY DENTAL TAXONOMY CODE
#    Assigns the first matching dental taxonomy code per provider
# -----------------------------------------------------------------------------
cat("Classifying providers by primary dental taxonomy code...\n")

us_df$primary_dental_taxonomy_code <- apply(
  us_df[, taxonomy_col_names, drop = FALSE], 1, function(r) {
    for (v in r) {
      v <- trimws(v)
      if (!is.na(v) && v %in% dental_taxonomy_codes) return(v)
    }
    NA_character_
  }
)

# Join taxonomy labels and category from data dictionary
us_df <- us_df %>%
  left_join(dental_taxonomy, by = c("primary_dental_taxonomy_code" = "taxonomy_code"))

# -----------------------------------------------------------------------------
# 7. SUMMARY TABLES
# -----------------------------------------------------------------------------

# 7a. By taxonomy (national analogue of the CA validation table)
summary_by_taxonomy <- us_df %>%
  count(category_type, primary_dental_taxonomy_code, taxonomy_label) %>%
  rename(
    taxonomy_code = primary_dental_taxonomy_code,
    n_providers   = n
  ) %>%
  arrange(category_type, desc(n_providers)) %>%
  mutate(pct_of_total = round(100 * n_providers / sum(n_providers), 1))

total_row <- tibble(
  category_type  = "TOTAL",
  taxonomy_code  = "ALL",
  taxonomy_label = "All Dental Providers",
  n_providers    = sum(summary_by_taxonomy$n_providers),
  pct_of_total   = 100.0
)

summary_table <- bind_rows(summary_by_taxonomy, total_row)

# 7b. By state — feeds the ADA HPI validation appendix directly
summary_by_state <- us_df %>%
  count(state, name = "n_providers") %>%
  arrange(desc(n_providers))

cat("\n================================================================\n")
cat(" EXTRACTION RESULT: Active Dentists, US 50 States + DC (14 Taxonomies)\n")
cat(" Source: NPPES Full Replacement File, July 2026\n")
cat("================================================================\n")
print(summary_table, n = Inf)
cat("\nTop 10 states by provider count:\n")
print(head(summary_by_state, 10))
cat(sprintf("\nExcluded dental providers (for STROBE flow diagram):\n"))
cat(sprintf("  Territories (PR/GU/VI/AS/MP): %s\n", format(n_territory, big.mark = ",")))
cat(sprintf("  Military codes (AA/AE/AP):    %s\n", format(n_military, big.mark = ",")))
cat(sprintf("  Other/foreign state codes:    %s\n", format(n_foreign_other, big.mark = ",")))
cat(sprintf("  Missing state:                %s\n", format(n_missing_state, big.mark = ",")))
cat(sprintf("\nTotal rows read from source file: %s\n",
            format(total_rows_read, big.mark = ",")))

# -----------------------------------------------------------------------------
# 8. SAVE OUTPUTS
# -----------------------------------------------------------------------------

# Summary validation table (by taxonomy)
summary_path <- file.path(output_dir, "US_Dentist_Count_Validation.csv")
write_csv(summary_table, summary_path)
cat(sprintf("\nSummary table saved to:\n  %s\n", summary_path))

# State-level counts (for ADA HPI validation appendix)
state_path <- file.path(output_dir, "US_Dentist_Count_by_State.csv")
write_csv(summary_by_state, state_path)
cat(sprintf("State-level counts saved to:\n  %s\n", state_path))

# Exclusion log (for STROBE flow diagram)
exclusion_log <- tibble(
  exclusion_reason = c("Territory practice address (PR/GU/VI/AS/MP)",
                       "Military mail code (AA/AE/AP)",
                       "Other/foreign state code",
                       "Missing state"),
  n_providers      = c(n_territory, n_military, n_foreign_other, n_missing_state)
)
exclusion_path <- file.path(output_dir, "US_Extraction_Exclusion_Log.csv")
write_csv(exclusion_log, exclusion_path)
cat(sprintf("Exclusion log saved to:\n  %s\n", exclusion_path))

# Full provider records with ZIP
full_path <- file.path(output_dir, "US_Dentists_NPI_Final.csv")
write_csv(us_df, full_path)
cat(sprintf("Full provider records saved to:\n  %s\n", full_path))

cat("\nDone.\n")
