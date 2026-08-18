# =============================================================================
# SCRIPT 10 (NATIONAL): Urban-Rural Descriptives at the 15-Mile Scale
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Purpose: The manuscript's Urban-Rural Distribution subsection (v7) reports
# 15-mile catchment descriptives by RUCA category. This script computes and
# saves them within the reproducible pipeline so every manuscript number
# traces to a pipeline output. Verify the console against the manuscript:
#   Mean 15-mi density:      Metro 7.9 | Micro 5.4 | Small town 4.6 | Rural 4.3
#   Desert prevalence:       Metro 0.9% | Rural 13.0%  (14-fold)
#   Desert composition:      70.4% of desert ZCTAs rural; 1.13M of 1.70M residents
#   Specialist deserts:      Metro 8.7% | Small town 56.5% | Rural 66.8%
#
# Inputs:  Merge/US_Analytic_With_ADI.csv, Models/US_Catchment_Supply.csv,
#          Models/US_Specialist_Catchment_Supply.csv
# Outputs: Models/US_UrbanRural_15mile_Descriptives.csv
#          Models/US_Desert_Composition_by_RUCA.csv
# =============================================================================

library(readr)
library(dplyr)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
catch_file    <- file.path(base_dir, "Models", "US_Catchment_Supply.csv")
spec_file     <- file.path(base_dir, "Models", "US_Specialist_Catchment_Supply.csv")
output_dir    <- file.path(base_dir, "Models")

catch <- read_csv(catch_file, col_types = cols(zip5 = col_character()),
                  show_col_types = FALSE) %>% select(zip5, dent_15, pop_15)
spec  <- read_csv(spec_file,  col_types = cols(zip5 = col_character()),
                  show_col_types = FALSE) %>% select(zip5, spec_15)

d <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
              show_col_types = FALSE) %>%
  left_join(catch, by = "zip5") %>%
  left_join(spec,  by = "zip5") %>%
  filter(!is.na(ruca_cat), !is.na(dent_15), pop_15 > 0) %>%
  mutate(
    ruca_cat      = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                                "Small town", "Rural")),
    dens15        = 10000 * dent_15 / pop_15,
    desert15      = dent_15 == 0,
    spec_desert15 = spec_15 == 0
  )

# --- Table 1: descriptives by RUCA ---
tab <- d %>%
  group_by(ruca_cat) %>%
  summarise(
    n_zctas               = n(),
    mean_dens15           = round(mean(dens15), 2),
    median_dens15         = round(median(dens15), 2),
    pct_desert15          = round(100 * mean(desert15), 2),
    pct_spec_desert15     = round(100 * mean(spec_desert15), 2),
    pop_in_deserts        = sum(population[desert15]),
    pop_in_spec_deserts   = sum(population[spec_desert15]),
    .groups = "drop"
  )
cat("Urban-rural descriptives, 15-mile scale:\n")
print(tab, n = Inf, width = Inf)
cat(sprintf("\nMetro-to-rural mean density ratio: %.2f\n",
            tab$mean_dens15[tab$ruca_cat == "Metropolitan"] /
            tab$mean_dens15[tab$ruca_cat == "Rural"]))
cat(sprintf("Rural-to-metro desert prevalence ratio: %.1f-fold\n",
            tab$pct_desert15[tab$ruca_cat == "Rural"] /
            tab$pct_desert15[tab$ruca_cat == "Metropolitan"]))
write_csv(tab, file.path(output_dir, "US_UrbanRural_15mile_Descriptives.csv"))

# --- Table 2: desert composition by RUCA (share of desert ZCTAs & residents) ---
comp <- d %>%
  filter(desert15) %>%
  group_by(ruca_cat) %>%
  summarise(n_desert_zctas = n(), desert_pop = sum(population), .groups = "drop") %>%
  mutate(
    pct_of_desert_zctas = round(100 * n_desert_zctas / sum(n_desert_zctas), 1),
    pct_of_desert_pop   = round(100 * desert_pop / sum(desert_pop), 1)
  )
cat("\nDesert composition by RUCA:\n")
print(comp, n = Inf, width = Inf)
write_csv(comp, file.path(output_dir, "US_Desert_Composition_by_RUCA.csv"))

cat("\nDone. Compare console values against the manuscript's Urban-Rural subsection.\n")
