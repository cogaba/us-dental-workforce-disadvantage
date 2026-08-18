# =============================================================================
# SCRIPT 2b (NATIONAL): Construct ZCTA-Level ADI from Block Groups
# Study: National Dental Workforce & Neighborhood Deprivation
#
# Method:
#   The ADI is constructed by the Neighborhood Atlas at the census block group
#   (BG), its native unit. BGs do not nest within ZCTAs, so BG scores are
#   allocated to ZCTAs using the GeoCorr 2022 population-weighted crosswalk
#   (source: 2020 block group; target: 2020 ZCTA; weight: 2020 Census
#   population). The ZCTA-level ADI is the POPULATION-WEIGHTED MEDIAN of
#   constituent BG national ranks, where each BG piece is weighted by the
#   population it contributes to that ZCTA (pop20 x afact).
#
#   Suppressed BGs (Neighborhood Atlas codes GQ, PH, GQ-PH, QDI) are excluded
#   before aggregation and counted for the STROBE flow diagram.
#
# Inputs:
#   US_2024_block_group_adi_v_4_0_1.csv   (Neighborhood Atlas, national BG file)
#   geocorr2022_2621602379.csv            (GeoCorr 2022 BG->ZCTA, all states,
#                                          2020 population weighting)
#   Merge/US_Dentist_SDI_Analytic_Final.csv  (Script 2 output)
#
# Outputs:
#   US_ADI_ZCTA_PopWeighted.csv           — ZCTA-level ADI (all constructable ZCTAs)
#   US_Analytic_With_ADI.csv              — analytic sample + adi_natrank column
#   US_ADI_Construction_Diagnostics.csv   — counts for flow diagram / Methods
# =============================================================================

library(readr)
library(dplyr)
library(stringr)

# -----------------------------------------------------------------------------
# 0. PATHS
# -----------------------------------------------------------------------------
# Set base_dir to your project folder before running

base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"

adi_file   <- file.path(base_dir, "US_2024_block_group_adi_v_4_0_1.csv")
gc_file    <- file.path(base_dir, "geocorr2022_2621602379.csv")
analytic_file <- file.path(base_dir, "Merge", "US_Dentist_SDI_Analytic_Final.csv")
output_dir <- file.path(base_dir, "Merge")

diagnostics <- list()

# =============================================================================
# STEP 1: LOAD ADI BLOCK-GROUP FILE; DROP SUPPRESSED
# =============================================================================
cat("================================================================\n")
cat("STEP 1: Loading national ADI block-group file...\n")
cat("================================================================\n")

adi_bg <- read_csv(adi_file,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE) %>%
  mutate(natrank = suppressWarnings(as.numeric(ADI_NATRANK)))

diagnostics$adi_bg_total      <- nrow(adi_bg)
diagnostics$adi_bg_suppressed <- sum(is.na(adi_bg$natrank))

suppression_tab <- adi_bg %>%
  filter(is.na(natrank)) %>%
  count(ADI_NATRANK, name = "n_block_groups")
cat("  Suppression codes:\n"); print(suppression_tab)

adi_bg <- adi_bg %>%
  filter(!is.na(natrank)) %>%
  select(FIPS, natrank)

cat(sprintf("  Block groups loaded:    %s\n",
            format(diagnostics$adi_bg_total, big.mark = ",")))
cat(sprintf("  Suppressed (excluded):  %s (%.1f%%)\n",
            format(diagnostics$adi_bg_suppressed, big.mark = ","),
            100 * diagnostics$adi_bg_suppressed / diagnostics$adi_bg_total))

# =============================================================================
# STEP 2: LOAD GEOCORR CROSSWALK; RECONSTRUCT 12-DIGIT FIPS
#   - Second row of the CSV is a descriptive-label row -> dropped after read
#   - FIPS = county(5) + tract(6, decimal removed, zero-padded) + block group(1)
#   - Rows with blank ZCTA (unassigned/unpopulated areas) dropped
#   - Weight = pop20 x afact = population the BG contributes to that ZCTA
# =============================================================================
cat("\n================================================================\n")
cat("STEP 2: Loading GeoCorr 2022 BG-to-ZCTA crosswalk...\n")
cat("================================================================\n")

gc_raw <- read_csv(gc_file,
                   col_types = cols(.default = col_character()),
                   locale = locale(encoding = "latin1"),
                   show_col_types = FALSE) %>%
  filter(county != "County code")          # drop the label row

gc_df <- gc_raw %>%
  mutate(
    tract6 = str_pad(str_replace(tract, fixed("."), ""), width = 6,
                     side = "left", pad = "0"),
    fips12 = paste0(str_pad(county, 5, "left", "0"), tract6, blockgroup),
    zcta   = trimws(zcta),
    afact  = as.numeric(afact),
    pop20  = as.numeric(pop20),
    wt     = pop20 * afact
  ) %>%
  filter(str_detect(zcta, "^\\d{5}$"), wt > 0) %>%
  select(fips12, zcta, wt)

diagnostics$gc_rows        <- nrow(gc_df)
diagnostics$gc_unique_bgs  <- n_distinct(gc_df$fips12)
diagnostics$gc_unique_zcta <- n_distinct(gc_df$zcta)

cat(sprintf("  Crosswalk rows (valid ZCTA, wt>0): %s\n",
            format(diagnostics$gc_rows, big.mark = ",")))
cat(sprintf("  Unique block groups: %s | Unique ZCTAs: %s\n",
            format(diagnostics$gc_unique_bgs, big.mark = ","),
            format(diagnostics$gc_unique_zcta, big.mark = ",")))

# Verify allocation factors: afact should sum to ~1 within each BG
afact_check <- gc_raw %>%
  mutate(afact = as.numeric(afact),
         tract6 = str_pad(str_replace(tract, fixed("."), ""), 6, "left", "0"),
         fips12 = paste0(str_pad(county, 5, "left", "0"), tract6, blockgroup)) %>%
  group_by(fips12) %>%
  summarise(s = sum(afact, na.rm = TRUE), .groups = "drop")
diagnostics$gc_afact_ok_pct <- round(100 * mean(afact_check$s > 0.99 & afact_check$s < 1.01), 1)
cat(sprintf("  BGs with afact summing to ~1.0: %.1f%%\n", diagnostics$gc_afact_ok_pct))

# =============================================================================
# STEP 3: POPULATION-WEIGHTED MEDIAN ADI PER ZCTA
# =============================================================================
cat("\n================================================================\n")
cat("STEP 3: Aggregating to population-weighted median ADI per ZCTA...\n")
cat("================================================================\n")

adi_join <- gc_df %>%
  inner_join(adi_bg, by = c("fips12" = "FIPS"))

diagnostics$joined_rows <- nrow(adi_join)

weighted_median <- function(x, w) {
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w)
  x[which(cw >= sum(w) / 2)[1]]
}

adi_zcta <- adi_join %>%
  group_by(zcta) %>%
  summarise(
    adi_natrank      = weighted_median(natrank, wt),
    n_bg_pieces      = n(),
    allocated_pop    = sum(wt),
    .groups = "drop"
  )

diagnostics$adi_zcta_constructed <- nrow(adi_zcta)
cat(sprintf("  ZCTAs with constructed ADI: %s\n",
            format(diagnostics$adi_zcta_constructed, big.mark = ",")))

# =============================================================================
# STEP 4: JOIN TO ANALYTIC SAMPLE
# =============================================================================
cat("\n================================================================\n")
cat("STEP 4: Joining ADI to the analytic sample...\n")
cat("================================================================\n")

analytic_df <- read_csv(analytic_file,
                        col_types = cols(zip5 = col_character()),
                        show_col_types = FALSE) %>%
  left_join(adi_zcta %>% select(zcta, adi_natrank),
            by = c("zip5" = "zcta"))

diagnostics$analytic_zctas    <- nrow(analytic_df)
diagnostics$analytic_with_adi <- sum(!is.na(analytic_df$adi_natrank))

cat(sprintf("  Analytic ZCTAs: %s | with ADI: %s (%.1f%%)\n",
            format(diagnostics$analytic_zctas, big.mark = ","),
            format(diagnostics$analytic_with_adi, big.mark = ","),
            100 * diagnostics$analytic_with_adi / diagnostics$analytic_zctas))

# Index concordance (for the Results/Supplement)
cc <- analytic_df %>% filter(!is.na(adi_natrank), !is.na(sdi_score))
diagnostics$corr_pearson  <- round(cor(cc$adi_natrank, cc$sdi_score), 3)
diagnostics$corr_spearman <- round(cor(cc$adi_natrank, cc$sdi_score,
                                       method = "spearman"), 3)
cat(sprintf("  ADI-SDI concordance: Pearson %.3f | Spearman %.3f\n",
            diagnostics$corr_pearson, diagnostics$corr_spearman))

# =============================================================================
# STEP 5: SAVE OUTPUTS
# =============================================================================
cat("\n================================================================\n")
cat(" SAVING OUTPUTS\n")
cat("================================================================\n")

write_csv(adi_zcta, file.path(output_dir, "US_ADI_ZCTA_PopWeighted.csv"))
cat("  1. US_ADI_ZCTA_PopWeighted.csv\n")

write_csv(analytic_df, file.path(output_dir, "US_Analytic_With_ADI.csv"))
cat("  2. US_Analytic_With_ADI.csv (analytic sample + adi_natrank)\n")

diagnostics_df <- tibble::tibble(
  step  = names(diagnostics),
  value = unlist(diagnostics)
)
write_csv(diagnostics_df, file.path(output_dir, "US_ADI_Construction_Diagnostics.csv"))
cat("  3. US_ADI_Construction_Diagnostics.csv\n")

cat("\n================================================================\n")
cat(" SCRIPT 2b COMPLETE\n")
cat(" Next: add SVI (2022, native ZCTA), then Script 3 - dual-index models\n")
cat("================================================================\n")
cat("\nDone.\n")
