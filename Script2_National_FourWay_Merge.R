# =============================================================================
# SCRIPT 2 (NATIONAL): Four-Way Merge + Consolidated Single-Principle Exclusions
# Study: National Dental Workforce & Social Deprivation
#
# 
#
#   
# Exclusion steps:
#
#   STEP A (ZCTA-level, at load): Puerto Rico / U.S. Virgin Islands ZCTAs
#          (codes below 01000) removed from the Census population frame —
#          territories are outside the study scope (SDI does not cover them).
#
#   STEP B (provider-level, at merge): Providers whose practice ZIP does
#          NOT appear in the Census ZCTA population file are dropped — no
#          population denominator exists (PO Box/unique ZIPs, institutional
#          ZIPs, discontinued ZIPs, NPPES entry errors)
#
#   STEP C (ZCTA-level): ZCTAs with Census-tabulated population of zero.
#
#   STEP D (ZCTA-level): ZCTAs with population below 500 — small-denominator
#          rate instability.
#
#   STEP E (ZCTA-level): ZCTAs with no valid SDI score (ZCTA absent from the
#          RGC file, or SDI score suppressed/missing). 
#
#
#
# SDI quartiles are constructed as NATIONAL empirical quartiles of the
# analytic sample 
#
# State assignment (for downstream mixed models): taken from the RUCA ZIP
# file's State field (ZIP-to-state). A small number of ZCTAs cross state
# lines; each is assigned the state of its identically numbered ZIP.
#
# Inputs:
#   US_Dentists_NPI_Final.csv                    (Script 1 output)
#   asset_rgc_sdi_2015_through_2019_zcta.csv     (RGC 2019 SDI, ZCTA level)
#   ACSDT5Y2024_B01003-Data.csv                  (ACS 2020-2024 5-yr population)
#   RUCA-codes-2020-zipcode.xlsx                 (USDA ERS 2020 RUCA, ZIP file)
#
# Outputs (US_ prefix):
#   US_Dentist_SDI_Merged_Full.csv     — all merged ZCTAs (pre-exclusion)
#   US_Dentist_SDI_Analytic_Final.csv  — final analytic sample
#   US_Merge_Diagnostics.csv           — step-by-step record counts
#   Exclusion_Log_Providers.csv        — every excluded provider documented
#   Exclusion_Log_ZCTAs.csv            — every excluded ZCTA documented
#   US_Summary_by_SDI_Quartile.csv     — descriptive summary by SDI quartile
#   US_Summary_by_RUCA.csv             — descriptive summary by RUCA category
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

# -----------------------------------------------------------------------------
# 0. PATHS  (adjust file locations to match where each download was saved)
# -----------------------------------------------------------------------------
# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"

npi_file   <- file.path(base_dir, "Extraction", "US_Dentists_NPI_Final.csv")
sdi_file   <- file.path(base_dir, "asset_rgc_sdi_2015_through_2019_zcta.csv")
pop_file   <- file.path(base_dir, "ACSDT5Y2024_B01003-Data.csv")
ruca_file  <- file.path(base_dir, "RUCA-codes-2020-zipcode.xlsx")
output_dir <- file.path(base_dir, "Merge")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

diagnostics <- list()

# =============================================================================
# STEP 1: LOAD NPI DENTIST DATA
# =============================================================================
cat("================================================================\n")
cat("STEP 1: Loading NPI dentist data...\n")
cat("================================================================\n")

us_df <- read_csv(npi_file,
                  col_types = cols(.default = col_character()),
                  show_col_types = FALSE) %>%
  mutate(zip5 = str_pad(trimws(zip5), width = 5, side = "left", pad = "0"))

diagnostics$npi_total_providers <- nrow(us_df)
cat(sprintf("  Total NPI providers loaded: %s\n",
            format(diagnostics$npi_total_providers, big.mark = ",")))

# =============================================================================
# STEP 2: LOAD POPULATION DATA (master frame)
#   ACS 2020-2024 5-year, table B01003, all ZCTAs.
#   File quirks handled here:
#     - Row 2 of the CSV is a descriptive-label row  -> dropped after read
#     - GEO_ID format "860Z200US#####"               -> ZCTA = last 5 chars
#     - Trailing empty column from a dangling comma  -> not selected
#   STEP A: territories (PR/VI; ZCTA codes < 01000) removed here.
# =============================================================================
cat("\n================================================================\n")
cat("STEP 2: Loading Census ZCTA population data...\n")
cat("================================================================\n")

pop_raw <- read_csv(pop_file,
                    col_types = cols(.default = col_character()),
                    show_col_types = FALSE) %>%
  filter(GEO_ID != "Geography")            # drop the second header row

pop_df <- pop_raw %>%
  mutate(
    zip5       = str_sub(GEO_ID, -5, -1),
    population = as.numeric(B01003_001E)
  ) %>%
  select(zip5, population)

diagnostics$pop_zcta_total_incl_territories <- nrow(pop_df)

# STEP A: exclude PR/VI ZCTAs (codes 00600-00999)
territory_zctas <- pop_df %>% filter(zip5 < "01000")
diagnostics$pop_zcta_territories_excluded <- nrow(territory_zctas)
diagnostics$pop_territory_population      <- sum(territory_zctas$population, na.rm = TRUE)

pop_df <- pop_df %>% filter(zip5 >= "01000")
diagnostics$pop_zcta_total <- nrow(pop_df)

cat(sprintf("  ZCTAs in ACS file (incl. territories): %s\n",
            format(diagnostics$pop_zcta_total_incl_territories, big.mark = ",")))
cat(sprintf("  STEP A - Territory ZCTAs excluded (PR/VI): %s (population %s)\n",
            format(diagnostics$pop_zcta_territories_excluded, big.mark = ","),
            format(diagnostics$pop_territory_population, big.mark = ",")))
cat(sprintf("  U.S. (50 states + DC) ZCTAs retained: %s\n",
            format(diagnostics$pop_zcta_total, big.mark = ",")))

valid_census_zctas <- unique(pop_df$zip5)

# =============================================================================
# STEP 3: PROVIDER-LEVEL EXCLUSION (single rule)  [STEP B]
#   Exclude any provider whose ZIP does not appear in the Census ZCTA file.
# =============================================================================
cat("\n================================================================\n")
cat("STEP 3: Applying provider-level exclusion (no Census ZCTA match)...\n")
cat("================================================================\n")

us_df <- us_df %>%
  mutate(
    provider_exclusion_flag = if_else(
      zip5 %in% valid_census_zctas,
      "Included",
      "Excluded: ZIP not in Census ZCTA list (no population denominator)"
    ),
    exclusion_reason = if_else(
      provider_exclusion_flag == "Included",
      NA_character_,
      "Practice ZIP does not appear in the ACS 2020-2024 ZCTA population file; no population denominator exists for rate-based density estimation."
    )
  )

diagnostics$npi_excluded_no_zcta <- sum(us_df$provider_exclusion_flag != "Included")
diagnostics$npi_included         <- sum(us_df$provider_exclusion_flag == "Included")

cat(sprintf("  Providers excluded (no Census ZCTA match): %s (%.2f%%)\n",
            format(diagnostics$npi_excluded_no_zcta, big.mark = ","),
            100 * diagnostics$npi_excluded_no_zcta / diagnostics$npi_total_providers))
cat(sprintf("  Providers retained:                        %s\n",
            format(diagnostics$npi_included, big.mark = ",")))

# Count dentists per ZIP (included only) — total, category, specialty
zip_counts_total <- us_df %>%
  filter(provider_exclusion_flag == "Included") %>%
  group_by(zip5) %>%
  summarise(n_dentists_total = n(), .groups = "drop")

zip_counts_category <- us_df %>%
  filter(provider_exclusion_flag == "Included") %>%
  group_by(zip5, category_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = category_type, values_from = n, values_fill = 0) %>%
  rename_with(~ paste0("n_", tolower(gsub(" ", "_", .x))), -zip5)

zip_counts_specialty <- us_df %>%
  filter(provider_exclusion_flag == "Included") %>%
  group_by(zip5, taxonomy_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = taxonomy_label, values_from = n, values_fill = 0) %>%
  rename_with(~ paste0("n_", tolower(gsub("[^a-zA-Z0-9]", "_", .x))), -zip5)

zip_dentist_counts <- zip_counts_total %>%
  left_join(zip_counts_category,  by = "zip5") %>%
  left_join(zip_counts_specialty, by = "zip5")

# =============================================================================
# STEP 4: LOAD SDI DATA (direct ZCTA join — no aggregation stage)
#   RGC 2019 SDI (2015-2019 ACS), native ZCTA release.
#   SDI_score = official centile (1-100), higher = more deprived.
# =============================================================================
cat("\n================================================================\n")
cat("STEP 4: Loading RGC SDI (ZCTA level)...\n")
cat("================================================================\n")

sdi_df <- read_csv(sdi_file,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE) %>%
  mutate(
    zip5      = str_pad(trimws(ZCTA5_FIPS), width = 5, side = "left", pad = "0"),
    sdi_score = as.numeric(SDI_score)
  ) %>%
  select(zip5, sdi_score,
         pct_Poverty_LT100, pct_Single_Parent_Fam, pct_Education_LT12years,
         pct_NonEmployed, pctHH_No_Vehicle, pctHH_Renter_Occupied, pctHH_Crowding) %>%
  mutate(across(starts_with("pct"), as.numeric)) %>%
  distinct(zip5, .keep_all = TRUE)

diagnostics$sdi_zcta_total         <- nrow(sdi_df)
diagnostics$sdi_zcta_missing_score <- sum(is.na(sdi_df$sdi_score))

cat(sprintf("  SDI ZCTAs loaded: %s\n",
            format(diagnostics$sdi_zcta_total, big.mark = ",")))
cat(sprintf("  SDI ZCTAs with missing/suppressed score: %s\n",
            format(diagnostics$sdi_zcta_missing_score, big.mark = ",")))

# =============================================================================
# STEP 5: LOAD RUCA DATA (2020 ZIP-code file)
#   Real header is on the second row of the data tab (skip = 1).
#   4-category collapse: Metro (1-3), Micropolitan (4-6),
#   Small town (7-9), Rural (10).
#   State field retained for downstream state-level modeling.
# =============================================================================
cat("\n================================================================\n")
cat("STEP 5: Loading 2020 RUCA ZIP-code file...\n")
cat("================================================================\n")

ruca_df <- read_excel(ruca_file,
                      sheet = "RUCA 2020 ZIP Code Data",
                      skip = 1, col_types = "text") %>%
  mutate(
    zip5      = str_pad(trimws(ZIPCode), width = 5, side = "left", pad = "0"),
    ruca1     = as.numeric(PrimaryRUCA),
    ruca_cat  = case_when(
      ruca1 >= 1 & ruca1 <= 3  ~ "Metropolitan",
      ruca1 >= 4 & ruca1 <= 6  ~ "Micropolitan",
      ruca1 >= 7 & ruca1 <= 9  ~ "Small town",
      ruca1 == 10              ~ "Rural",
      TRUE                     ~ NA_character_
    ),
    state = trimws(toupper(State))
  ) %>%
  select(zip5, state, ruca1, ruca_cat) %>%
  distinct(zip5, .keep_all = TRUE)

diagnostics$ruca_zips_total <- nrow(ruca_df)
cat(sprintf("  RUCA ZIP records loaded: %s\n",
            format(diagnostics$ruca_zips_total, big.mark = ",")))

# =============================================================================
# STEP 6: FOUR-WAY MERGE (master frame = population file)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 6: Merging population + SDI + RUCA + dentist counts...\n")
cat("================================================================\n")

merged_full <- pop_df %>%
  left_join(sdi_df,            by = "zip5") %>%
  left_join(ruca_df,           by = "zip5") %>%
  left_join(zip_dentist_counts, by = "zip5") %>%
  mutate(
    across(starts_with("n_"), ~ replace_na(.x, 0)),
    n_dentists_total = replace_na(n_dentists_total, 0)
  )

diagnostics$merge_total_zctas    <- nrow(merged_full)
diagnostics$merge_total_dentists <- sum(merged_full$n_dentists_total)
diagnostics$merge_ruca_missing   <- sum(is.na(merged_full$ruca_cat))

cat(sprintf("  Total ZCTAs in merged dataset:    %s\n",
            format(diagnostics$merge_total_zctas, big.mark = ",")))
cat(sprintf("  Total dentists in merged dataset: %s\n",
            format(diagnostics$merge_total_dentists, big.mark = ",")))
cat(sprintf("  ZCTAs without a RUCA match:       %s\n",
            format(diagnostics$merge_ruca_missing, big.mark = ",")))

# =============================================================================
# STEP 7: ZCTA-LEVEL EXCLUSIONS (single principle, applied in order)
#   STEP C: population = 0
#   STEP D: 0 < population < 500
#   STEP E: no valid SDI score
# =============================================================================
cat("\n================================================================\n")
cat("STEP 7: Applying ZCTA-level exclusions...\n")
cat("================================================================\n")

merged_full <- merged_full %>%
  mutate(
    zcta_exclusion_flag = case_when(
      is.na(population) | population == 0 ~ "Excluded: Population = 0",
      population < 500                    ~ "Excluded: Population < 500",
      is.na(sdi_score)                    ~ "Excluded: No valid SDI score",
      TRUE                                ~ "Included"
    )
  )

excl_tab <- merged_full %>%
  group_by(zcta_exclusion_flag) %>%
  summarise(n_zctas = n(), n_dentists = sum(n_dentists_total), .groups = "drop")
print(excl_tab)

diagnostics$zcta_excl_pop_zero  <- sum(merged_full$zcta_exclusion_flag == "Excluded: Population = 0")
diagnostics$zcta_excl_pop_lt500 <- sum(merged_full$zcta_exclusion_flag == "Excluded: Population < 500")
diagnostics$zcta_excl_sdi_miss  <- sum(merged_full$zcta_exclusion_flag == "Excluded: No valid SDI score")
diagnostics$dent_excl_pop_zero  <- sum(merged_full$n_dentists_total[merged_full$zcta_exclusion_flag == "Excluded: Population = 0"])
diagnostics$dent_excl_pop_lt500 <- sum(merged_full$n_dentists_total[merged_full$zcta_exclusion_flag == "Excluded: Population < 500"])
diagnostics$dent_excl_sdi_miss  <- sum(merged_full$n_dentists_total[merged_full$zcta_exclusion_flag == "Excluded: No valid SDI score"])

# =============================================================================
# STEP 8: FINAL ANALYTIC SAMPLE + DERIVED VARIABLES
#   - dentists per 10,000 residents (total, general, specialist)
#   - national empirical SDI quartiles (achieved cut points printed)
# =============================================================================
analytic_df <- merged_full %>%
  filter(zcta_exclusion_flag == "Included") %>%
  mutate(
    dentists_per_10k    = 10000 * n_dentists_total / population,
    general_per_10k     = 10000 * replace_na(n_general_dentistry, 0) / population,
    specialist_per_10k  = 10000 * replace_na(n_specialty, 0) / population,
    no_dentists         = n_dentists_total == 0,
    no_general          = replace_na(n_general_dentistry, 0) == 0,
    no_specialist       = replace_na(n_specialty, 0) == 0
  )

sdi_cuts <- quantile(analytic_df$sdi_score, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
cat(sprintf("\n  National empirical SDI quartile cut points: Q1<=%.0f | Q2<=%.0f | Q3<=%.0f | Q4>%.0f\n",
            sdi_cuts[1], sdi_cuts[2], sdi_cuts[3], sdi_cuts[3]))

analytic_df <- analytic_df %>%
  mutate(
    sdi_quartile = cut(
      sdi_score,
      breaks = c(-Inf, sdi_cuts, Inf),
      labels = c(
        sprintf("Q1 - Least Deprived (SDI 1-%.0f)", sdi_cuts[1]),
        sprintf("Q2 (SDI %.0f-%.0f)", sdi_cuts[1] + 1, sdi_cuts[2]),
        sprintf("Q3 (SDI %.0f-%.0f)", sdi_cuts[2] + 1, sdi_cuts[3]),
        sprintf("Q4 - Most Deprived (SDI %.0f-100)", sdi_cuts[3] + 1)
      )
    ),
    ruca_cat = factor(ruca_cat,
                      levels = c("Metropolitan", "Micropolitan", "Small town", "Rural"))
  )

diagnostics$analytic_zctas             <- nrow(analytic_df)
diagnostics$analytic_population        <- sum(analytic_df$population)
diagnostics$analytic_dentists          <- sum(analytic_df$n_dentists_total)
diagnostics$analytic_zctas_no_dentists <- sum(analytic_df$no_dentists)
diagnostics$analytic_ruca_missing      <- sum(is.na(analytic_df$ruca_cat))
diagnostics$analytic_states            <- n_distinct(analytic_df$state, na.rm = TRUE)

cat(sprintf("\n  FINAL ANALYTIC SAMPLE\n"))
cat(sprintf("    ZCTAs included:        %s\n",
            format(diagnostics$analytic_zctas, big.mark = ",")))
cat(sprintf("    Population covered:    %s\n",
            format(diagnostics$analytic_population, big.mark = ",")))
cat(sprintf("    Dentists included:     %s\n",
            format(diagnostics$analytic_dentists, big.mark = ",")))
cat(sprintf("    ZCTAs with 0 dentists: %s (%.1f%%)\n",
            format(diagnostics$analytic_zctas_no_dentists, big.mark = ","),
            100 * diagnostics$analytic_zctas_no_dentists / diagnostics$analytic_zctas))
cat(sprintf("    Distinct states:       %s\n", diagnostics$analytic_states))
cat(sprintf("    ZCTAs missing RUCA:    %s\n", diagnostics$analytic_ruca_missing))

# =============================================================================
# STEP 9: DESCRIPTIVE SUMMARIES (by SDI quartile; by RUCA category)
# =============================================================================
quartile_summary <- analytic_df %>%
  group_by(sdi_quartile) %>%
  summarise(
    n_zctas                  = n(),
    total_population         = sum(population, na.rm = TRUE),
    total_dentists           = sum(n_dentists_total),
    zctas_with_no_dentist    = sum(no_dentists),
    pct_zctas_no_dentist     = round(100 * mean(no_dentists), 1),
    mean_dentists_per_10k    = round(mean(dentists_per_10k), 2),
    median_dentists_per_10k  = round(median(dentists_per_10k), 2),
    mean_general_per_10k     = round(mean(general_per_10k), 2),
    mean_specialist_per_10k  = round(mean(specialist_per_10k), 2),
    pct_no_specialist        = round(100 * mean(no_specialist), 1),
    mean_sdi                 = round(mean(sdi_score), 1),
    .groups = "drop"
  )

ruca_summary <- analytic_df %>%
  filter(!is.na(ruca_cat)) %>%
  group_by(ruca_cat) %>%
  summarise(
    n_zctas                  = n(),
    total_population         = sum(population, na.rm = TRUE),
    total_dentists           = sum(n_dentists_total),
    pct_zctas_no_dentist     = round(100 * mean(no_dentists), 1),
    mean_dentists_per_10k    = round(mean(dentists_per_10k), 2),
    median_dentists_per_10k  = round(median(dentists_per_10k), 2),
    mean_sdi                 = round(mean(sdi_score), 1),
    .groups = "drop"
  )

cat("\n  Summary by national SDI quartile:\n")
print(quartile_summary, n = Inf, width = Inf)
cat("\n  Summary by RUCA category:\n")
print(ruca_summary, n = Inf, width = Inf)

# =============================================================================
# STEP 10: BUILD DIAGNOSTICS TABLE
# =============================================================================
diagnostics_df <- tibble::tibble(
  step = c(
    "1.0  NPI: Total providers extracted (Script 1, 50 states + DC)",
    "2.0  Population: ZCTAs in ACS 2020-2024 file (incl. territories)",
    "2.1  Population: Territory ZCTAs excluded (PR/VI, codes < 01000)",
    "2.2  Population: U.S. ZCTAs retained (master frame)",
    "3.0  NPI: Excluded - ZIP not in Census ZCTA list (no denominator)",
    "3.1  NPI: Included after provider-level exclusion",
    "4.0  SDI: ZCTAs in RGC 2019 file",
    "4.1  SDI: ZCTAs with missing/suppressed score",
    "5.0  RUCA: ZIP records in 2020 file",
    "6.0  Merge: Total ZCTAs in merged dataset (= population frame)",
    "6.1  Merge: Total dentists in merged dataset",
    "6.2  Merge: ZCTAs without RUCA match",
    "7.0  ZCTA Excluded: Population = 0",
    "7.1  ZCTA Excluded: 0 < Population < 500",
    "7.2  ZCTA Excluded: No valid SDI score",
    "7.3  Dentists removed under population = 0 rule",
    "7.4  Dentists removed under population < 500 rule",
    "7.5  Dentists removed under missing-SDI rule",
    "8.0  Final analytic sample: ZCTAs",
    "8.1  Final analytic sample: Population",
    "8.2  Final analytic sample: Dentists",
    "8.3  Final analytic sample: ZCTAs with 0 dentists",
    "8.4  Final analytic sample: Distinct states",
    "8.5  Final analytic sample: ZCTAs missing RUCA"
  ),
  value = c(
    diagnostics$npi_total_providers,
    diagnostics$pop_zcta_total_incl_territories,
    diagnostics$pop_zcta_territories_excluded,
    diagnostics$pop_zcta_total,
    diagnostics$npi_excluded_no_zcta,
    diagnostics$npi_included,
    diagnostics$sdi_zcta_total,
    diagnostics$sdi_zcta_missing_score,
    diagnostics$ruca_zips_total,
    diagnostics$merge_total_zctas,
    diagnostics$merge_total_dentists,
    diagnostics$merge_ruca_missing,
    diagnostics$zcta_excl_pop_zero,
    diagnostics$zcta_excl_pop_lt500,
    diagnostics$zcta_excl_sdi_miss,
    diagnostics$dent_excl_pop_zero,
    diagnostics$dent_excl_pop_lt500,
    diagnostics$dent_excl_sdi_miss,
    diagnostics$analytic_zctas,
    diagnostics$analytic_population,
    diagnostics$analytic_dentists,
    diagnostics$analytic_zctas_no_dentists,
    diagnostics$analytic_states,
    diagnostics$analytic_ruca_missing
  ),
  note = c(
    "NPPES Full Replacement File July 2026; 14 ADA taxonomy codes; active Type 1 providers",
    "ACS 2020-2024 5-year estimates, table B01003; ZCTA geographic unit",
    "Territories outside study scope; SDI coverage unavailable",
    "50 states + DC master frame for the merge",
    "Single-principle exclusion: no residential population denominator for these ZIPs",
    "Providers retained for ZCTA-level density computation",
    "RGC 2019 SDI (2015-2019 ACS), native ZCTA release; direct join, no aggregation",
    "Suppressed/missing official centile scores",
    "USDA ERS 2020 RUCA ZIP-code approximation file; also supplies state assignment",
    "Master frame = population file; left-joins of SDI, RUCA, dentist counts",
    "Sum of n_dentists_total over all merged ZCTAs",
    "ZCTA code with no identically numbered ZIP in RUCA file",
    "Typically institutional or non-residential ZCTAs",
    "Small-denominator rate instability (consistent with California paper)",
    "ZCTA absent from RGC file or score suppressed; analogue of CA ADI-suppressed rule",
    "Dentists in ZCTAs excluded under the population = 0 rule",
    "Dentists in ZCTAs excluded under the population < 500 rule",
    "Dentists in ZCTAs excluded under the missing-SDI rule",
    "Final analytic ZCTA count after all consolidated exclusions",
    "Sum of ACS population across analytic ZCTAs",
    "Sum of NPPES dentist counts across analytic ZCTAs",
    "Retained as valid zero-dentist ZCTAs (dental deserts) for analysis",
    "Should be 51 (50 states + DC)",
    "Retained; excluded only from RUCA-stratified models"
  )
)

cat("\n================================================================\n")
cat(" FULL DIAGNOSTICS TABLE\n")
cat("================================================================\n")
print(diagnostics_df, n = Inf, width = Inf)

# =============================================================================
# STEP 11: BUILD EXCLUSION LOGS
# =============================================================================
provider_exclusion_log <- us_df %>%
  filter(provider_exclusion_flag != "Included") %>%
  select(NPI, state, zip5, zip9, provider_exclusion_flag, exclusion_reason,
         primary_dental_taxonomy_code, taxonomy_label, category_type)

zcta_exclusion_log <- merged_full %>%
  filter(zcta_exclusion_flag != "Included") %>%
  select(zip5, state, population, n_dentists_total, sdi_score,
         ruca_cat, zcta_exclusion_flag)

# =============================================================================
# STEP 12: SAVE ALL OUTPUTS
# =============================================================================
cat("\n================================================================\n")
cat(" SAVING OUTPUTS\n")
cat("================================================================\n")

write_csv(merged_full, file.path(output_dir, "US_Dentist_SDI_Merged_Full.csv"))
cat("  1. US_Dentist_SDI_Merged_Full.csv     (pre-exclusion merged universe)\n")

write_csv(analytic_df, file.path(output_dir, "US_Dentist_SDI_Analytic_Final.csv"))
cat(sprintf("  2. US_Dentist_SDI_Analytic_Final.csv  (%s ZCTAs, final analytic sample)\n",
            format(nrow(analytic_df), big.mark = ",")))

write_csv(diagnostics_df, file.path(output_dir, "US_Merge_Diagnostics.csv"))
cat("  3. US_Merge_Diagnostics.csv\n")

write_csv(provider_exclusion_log, file.path(output_dir, "Exclusion_Log_Providers.csv"))
cat(sprintf("  4. Exclusion_Log_Providers.csv        (%s providers)\n",
            format(nrow(provider_exclusion_log), big.mark = ",")))

write_csv(zcta_exclusion_log, file.path(output_dir, "Exclusion_Log_ZCTAs.csv"))
cat(sprintf("  5. Exclusion_Log_ZCTAs.csv            (%s ZCTAs)\n",
            format(nrow(zcta_exclusion_log), big.mark = ",")))

write_csv(quartile_summary, file.path(output_dir, "US_Summary_by_SDI_Quartile.csv"))
cat("  6. US_Summary_by_SDI_Quartile.csv\n")

write_csv(ruca_summary, file.path(output_dir, "US_Summary_by_RUCA.csv"))
cat("  7. US_Summary_by_RUCA.csv\n")

cat("\n================================================================\n")
cat(" SCRIPT 2 (NATIONAL) COMPLETE\n")
cat(" Next: Script 3 - Descriptives, Diagnostics, Mixed-Effects Regression\n")
cat("================================================================\n")
cat("\nDone.\n")
