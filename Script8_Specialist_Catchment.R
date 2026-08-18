# =============================================================================
# SCRIPT 8 (NATIONAL): Specialist vs General Dentist Availability
#                      at the 15-Mile Catchment Scale
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Rationale: Prior national accessibility work analyzed all dentists
# combined; whether disadvantage gradients differ for specialty care is
# unexamined nationally. This script goes beyond descriptives with a
# SPECIALIST-SHARE model: specialist count offset by TOTAL nearby dentists,
# which directly tests whether the specialist share of the local workforce
# declines with disadvantage (the formal version of "the gradient is
# steeper for specialists").
#
# Analyses (15-mile primary scale, ADI primary index):
#   A. General-dentist catchment gradient      (offset: log nearby population)
#   B. Specialist catchment gradient           (offset: log nearby population)
#   C. Specialist-share gradient               (offset: log nearby dentists;
#      catchments with >=1 dentist)  <- the key inferential test
#   D. Specialist deserts (no specialist within 15 miles): mixed logistic
#   E. Descriptives by ADI quartile (Table material)
#
# Inputs:  2025_Gaz_zcta_national.txt, Merge/US_Dentist_SDI_Merged_Full.csv,
#          Merge/US_Analytic_With_ADI.csv
# Outputs: Models/US_Specialist_Catchment_Supply.csv
#          Models/US_Specialist_Models.csv
#          Models/US_Specialist_by_ADI_Quartile.csv
#
# Runtime: one frNN pass (~1-2 min) + four glmmTMB fits (~10-20 min).
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(glmmTMB)
library(dbscan)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
gaz_file      <- file.path(base_dir, "2025_Gaz_zcta_national.txt")
universe_file <- file.path(base_dir, "Merge", "US_Dentist_SDI_Merged_Full.csv")
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
output_dir    <- file.path(base_dir, "Models")

R_EARTH_MI <- 3958.8
RADIUS     <- 15

tidy_fixed <- function(model) {
  b  <- fixef(model)$cond
  se <- sqrt(diag(vcov(model)$cond))
  tibble::tibble(
    term      = names(b),
    estimate  = exp(unname(b)),
    conf.low  = exp(unname(b) - 1.96 * unname(se)),
    conf.high = exp(unname(b) + 1.96 * unname(se)),
    p.value   = 2 * pnorm(-abs(unname(b) / unname(se)))
  )
}

# =============================================================================
# STEP 1: SUPPLY UNIVERSE WITH GENERAL / SPECIALIST SPLIT
# =============================================================================
cat("STEP 1: Building general/specialist supply universe...\n")

gaz <- read_delim(gaz_file, delim = "|", trim_ws = TRUE,
                  col_types = cols(.default = col_character())) %>%
  transmute(zcta = str_pad(str_trim(GEOID), 5, "left", "0"),
            lat = as.numeric(INTPTLAT), lon = as.numeric(INTPTLONG))

uni <- read_csv(universe_file, col_types = cols(zip5 = col_character()),
                show_col_types = FALSE) %>%
  mutate(
    n_gen  = as.numeric(tidyr::replace_na(n_general_dentistry, 0)),
    n_spec = as.numeric(tidyr::replace_na(n_specialty, 0))
  ) %>%
  select(zip5, population, n_gen, n_spec) %>%
  inner_join(gaz, by = c("zip5" = "zcta")) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(population))

cat(sprintf("  Universe: %s ZCTAs | %s general | %s specialists\n",
            format(nrow(uni), big.mark = ","),
            format(sum(uni$n_gen), big.mark = ","),
            format(sum(uni$n_spec), big.mark = ",")))

# =============================================================================
# STEP 2: 15-MILE CATCHMENTS (general, specialist, population)
# =============================================================================
cat("\nSTEP 2: Computing 15-mile catchments...\n")

lat_r <- uni$lat * pi / 180; lon_r <- uni$lon * pi / 180
xyz <- cbind(cos(lat_r) * cos(lon_r), cos(lat_r) * sin(lon_r), sin(lat_r))
nn  <- frNN(xyz, eps = 2 * sin(RADIUS / (2 * R_EARTH_MI)))

gen  <- uni$n_gen; spec <- uni$n_spec; pop <- uni$population
catch <- tibble(
  zip5        = uni$zip5,
  gen_15      = vapply(seq_along(nn$id), function(i) gen[i]  + sum(gen[nn$id[[i]]]),  numeric(1)),
  spec_15     = vapply(seq_along(nn$id), function(i) spec[i] + sum(spec[nn$id[[i]]]), numeric(1)),
  pop_15      = vapply(seq_along(nn$id), function(i) pop[i]  + sum(pop[nn$id[[i]]]),  numeric(1))
) %>%
  mutate(dent_15 = gen_15 + spec_15)
rm(nn); gc()

write_csv(catch, file.path(output_dir, "US_Specialist_Catchment_Supply.csv"))
cat(sprintf("  ZCTAs with no specialist within 15 mi (universe): %s\n",
            format(sum(catch$spec_15 == 0), big.mark = ",")))

# =============================================================================
# STEP 3: ANALYTIC DATASET
# =============================================================================
cat("\nSTEP 3: Assembling analytic dataset...\n")

d <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
              show_col_types = FALSE) %>%
  left_join(catch, by = "zip5") %>%
  filter(!is.na(adi_natrank), !is.na(ruca_cat), !is.na(dent_15), pop_15 > 0) %>%
  mutate(
    adi10    = adi_natrank / 10,
    y_gen    = as.integer(round(gen_15)),
    y_spec   = as.integer(round(spec_15)),
    log_pop  = log(pop_15),
    ruca_cat = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                           "Small town", "Rural")),
    state    = factor(state),
    no_spec15 = spec_15 == 0
  )
cat(sprintf("  Analytic ZCTAs: %s | no specialist within 15 mi: %s (%.1f%%)\n",
            format(nrow(d), big.mark = ","),
            format(sum(d$no_spec15), big.mark = ","),
            100 * mean(d$no_spec15)))

# =============================================================================
# STEP 4: MODELS
# =============================================================================
cat("\nSTEP 4: Fitting models (four glmmTMB fits)...\n")
results <- list()

# A. General-dentist gradient (per nearby population)
mA <- glmmTMB(y_gen ~ adi10 + ruca_cat + offset(log_pop) + (1 | state),
              data = d, family = nbinom2)
tA <- tidy_fixed(mA) %>% filter(term == "adi10") %>%
  mutate(model = "A_general_per_population", .before = 1)
results$A <- tA
cat(sprintf("  A General RR/10pct:          %.4f (%.4f-%.4f)\n",
            tA$estimate, tA$conf.low, tA$conf.high))

# B. Specialist gradient (per nearby population)
mB <- glmmTMB(y_spec ~ adi10 + ruca_cat + offset(log_pop) + (1 | state),
              data = d, family = nbinom2)
tB <- tidy_fixed(mB) %>% filter(term == "adi10") %>%
  mutate(model = "B_specialist_per_population", .before = 1)
results$B <- tB
cat(sprintf("  B Specialist RR/10pct:       %.4f (%.4f-%.4f)\n",
            tB$estimate, tB$conf.low, tB$conf.high))

# C. Specialist SHARE gradient (offset = log nearby dentists) — key test
dC <- d %>% filter(dent_15 > 0) %>% mutate(log_dent = log(dent_15))
mC <- glmmTMB(y_spec ~ adi10 + ruca_cat + offset(log_dent) + (1 | state),
              data = dC, family = nbinom2)
tC <- tidy_fixed(mC) %>% filter(term == "adi10") %>%
  mutate(model = "C_specialist_share", .before = 1)
results$C <- tC
cat(sprintf("  C Specialist-SHARE RR/10pct: %.4f (%.4f-%.4f)  <- key test\n",
            tC$estimate, tC$conf.low, tC$conf.high))

# D. Specialist deserts (no specialist within 15 miles)
mD <- glmmTMB(no_spec15 ~ adi10 + ruca_cat + (1 | state),
              data = d, family = binomial)
tD <- tidy_fixed(mD) %>% filter(term == "adi10") %>%
  mutate(model = "D_specialist_desert_OR", .before = 1)
results$D <- tD
cat(sprintf("  D Specialist-desert OR/10pct: %.4f (%.4f-%.4f)\n",
            tD$estimate, tD$conf.low, tD$conf.high))

write_csv(bind_rows(results), file.path(output_dir, "US_Specialist_Models.csv"))

# =============================================================================
# STEP 5: DESCRIPTIVES BY ADI QUARTILE (Table material)
# =============================================================================
cat("\nSTEP 5: Descriptives by ADI quartile...\n")

qtab <- d %>%
  mutate(q = cut(adi_natrank,
                 breaks = quantile(adi_natrank, probs = c(0, .25, .5, .75, 1)),
                 include.lowest = TRUE,
                 labels = c("Q1 (least)", "Q2", "Q3", "Q4 (most)"))) %>%
  group_by(q) %>%
  summarise(
    n_zctas                = n(),
    gen_per_10k_15mi       = round(mean(10000 * gen_15 / pop_15), 2),
    spec_per_10k_15mi      = round(mean(10000 * spec_15 / pop_15), 2),
    mean_specialist_share  = round(100 * mean(spec_15 / pmax(dent_15, 1)), 1),
    pct_no_specialist_15mi = round(100 * mean(no_spec15), 1),
    pop_no_specialist_15mi = sum(population[no_spec15]),
    .groups = "drop"
  )
print(qtab, n = Inf, width = Inf)
write_csv(qtab, file.path(output_dir, "US_Specialist_by_ADI_Quartile.csv"))

cat("\n================================================================\n")
cat(" SCRIPT 8 COMPLETE - specialist availability analyses\n")
cat("================================================================\n")
cat("\nDone.\n")
