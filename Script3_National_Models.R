# =============================================================================
# SCRIPT 3 (NATIONAL): Descriptives + Mixed-Effects Negative Binomial Models
# Study: National Dental Workforce & Neighborhood Deprivation
#
# Design:
#   Every model is fitted in parallel under FOUR deprivation exposures:
#     1. ADI   (adi_natrank, 1-100; population-weighted BG median, Script 2b)
#     2. SDI   (sdi_score, 1-100; RGC 2019, native ZCTA)
#     3. SVI   (RPL_THEMES x 100, 0-100; CDC/ATSDR 2022, native ZCTA)
#     4. SVI SES theme only (RPL_THEME1 x 100; socioeconomic-status theme)
#   All exposures scaled per 10 percentiles for comparable rate ratios.
#
# Model sequence per exposure (glmmTMB, NB2 family, offset log(population)):
#   M1: dep10 + (1 | state)                      — national gradient, state RI
#   M2: dep10 + ruca_cat + (1 | state)           — rurality-adjusted
#   M3: dep10 * ruca_cat + (1 | state)           — deprivation x rurality
#   M4: dep10 + ruca_cat + (dep10 | state)       — random slopes; state BLUPs
#   D1: no_dentists ~ dep10 * ruca_cat + (1|state), binomial — dental deserts
#
# Outputs (to Models/ directory):
#   US_Quartile_Descriptives_by_Index.csv  — Table 1/2 material
#   US_Model_FixedEffects.csv              — tidy fixed effects, all models
#   US_M3_Stratum_RRs.csv                  — stratum-specific RRs (M3)
#   US_State_Slopes_<INDEX>.csv            — per-state slopes (M4 BLUPs)
#   US_Desert_Models.csv                   — desert model odds ratios
#   US_Index_Concordance.csv               — Spearman matrix of exposures
#
# Runtime note: ~20 glmmTMB fits on 26k rows; expect 15-40 minutes total.
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(glmmTMB)
# NOTE: broom.mixed intentionally not required (dependency conflicts on some
# systems). Fixed effects are extracted directly from glmmTMB objects with
# the tidy_fixed() helper below; results are identical.

# -----------------------------------------------------------------------------
# 0. PATHS
# -----------------------------------------------------------------------------
# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"

analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
svi_file      <- file.path(base_dir, "SVI_2022_US_ZCTA.csv")
output_dir    <- file.path(base_dir, "Models")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 0b. HELPER: exponentiated fixed effects from a glmmTMB fit
#     (base-R replacement for broom tidying)
# -----------------------------------------------------------------------------
tidy_fixed <- function(model) {
  b  <- fixef(model)$cond
  se <- sqrt(diag(vcov(model)$cond))
  tibble::tibble(
    term      = names(b),
    estimate  = exp(unname(b)),
    std.error = unname(se),
    statistic = unname(b) / unname(se),
    p.value   = 2 * pnorm(-abs(unname(b) / unname(se))),
    conf.low  = exp(unname(b) - 1.96 * unname(se)),
    conf.high = exp(unname(b) + 1.96 * unname(se))
  )
}

# =============================================================================
# STEP 1: LOAD ANALYTIC SAMPLE; JOIN SVI; CONSTRUCT EXPOSURES
# =============================================================================
cat("================================================================\n")
cat("STEP 1: Loading analytic sample and joining SVI...\n")
cat("================================================================\n")

svi_df <- read_csv(svi_file,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE) %>%
  transmute(
    zip5        = str_pad(trimws(FIPS), 5, "left", "0"),
    svi_overall = as.numeric(RPL_THEMES),
    svi_ses     = as.numeric(RPL_THEME1)
  ) %>%
  mutate(across(c(svi_overall, svi_ses), ~ if_else(.x < 0, NA_real_, .x))) %>%  # -999 -> NA
  distinct(zip5, .keep_all = TRUE)

df <- read_csv(analytic_file,
               col_types = cols(zip5 = col_character()),
               show_col_types = FALSE) %>%
  left_join(svi_df, by = "zip5") %>%
  mutate(
    # all exposures on a per-10-percentile scale
    adi10      = adi_natrank / 10,
    sdi10      = sdi_score  / 10,
    svi10      = svi_overall * 10,   # 0-1 scale: x10 = per 10 percentiles
    svi_ses10  = svi_ses     * 10,
    ruca_cat   = factor(ruca_cat,
                        levels = c("Metropolitan", "Micropolitan",
                                   "Small town", "Rural")),
    state      = factor(state),
    logpop     = log(population)
  )

cat(sprintf("  Analytic ZCTAs: %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("  With ADI: %s | SDI: %s | SVI: %s\n",
            format(sum(!is.na(df$adi10)), big.mark = ","),
            format(sum(!is.na(df$sdi10)), big.mark = ","),
            format(sum(!is.na(df$svi10)), big.mark = ",")))

# Index concordance matrix (Spearman) — reported in Results
conc_vars <- df %>%
  select(ADI = adi_natrank, SDI = sdi_score,
         SVI_overall = svi_overall, SVI_SES = svi_ses) %>%
  filter(complete.cases(.))
conc <- cor(conc_vars, method = "spearman")
write_csv(as.data.frame(conc) %>% mutate(index = rownames(conc), .before = 1),
          file.path(output_dir, "US_Index_Concordance.csv"))
cat("\n  Spearman concordance matrix (n complete = ",
    format(nrow(conc_vars), big.mark = ","), "):\n", sep = "")
print(round(conc, 3))

# =============================================================================
# STEP 2: QUARTILE DESCRIPTIVES UNDER EACH INDEX
#   National empirical quartiles per index; density + desert prevalence
# =============================================================================
cat("\n================================================================\n")
cat("STEP 2: Quartile descriptives under each index...\n")
cat("================================================================\n")

index_specs <- tribble(
  ~index_name,   ~raw_var,
  "ADI",         "adi_natrank",
  "SDI",         "sdi_score",
  "SVI_overall", "svi_overall",
  "SVI_SES",     "svi_ses"
)

quartile_descriptives <- purrr::pmap_dfr(index_specs, function(index_name, raw_var) {
  d <- df %>% filter(!is.na(.data[[raw_var]]))
  d$q <- cut(d[[raw_var]],
             breaks = quantile(d[[raw_var]], probs = c(0, .25, .5, .75, 1)),
             include.lowest = TRUE,
             labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4 (most deprived)"))
  d %>%
    group_by(q) %>%
    summarise(
      n_zctas                 = n(),
      total_population        = sum(population),
      total_dentists          = sum(n_dentists_total),
      mean_dentists_per_10k   = round(mean(dentists_per_10k), 2),
      median_dentists_per_10k = round(median(dentists_per_10k), 2),
      popweighted_per_10k     = round(10000 * sum(n_dentists_total) / sum(population), 2),
      pct_zctas_no_dentist    = round(100 * mean(no_dentists), 1),
      pct_no_specialist       = round(100 * mean(no_specialist), 1),
      .groups = "drop"
    ) %>%
    mutate(index = index_name, .before = 1)
})

print(quartile_descriptives, n = Inf, width = Inf)
write_csv(quartile_descriptives,
          file.path(output_dir, "US_Quartile_Descriptives_by_Index.csv"))

# =============================================================================
# STEP 3: MIXED-EFFECTS NB MODEL SEQUENCE PER EXPOSURE
# =============================================================================
cat("\n================================================================\n")
cat("STEP 3: Fitting glmmTMB models (this is the long step)...\n")
cat("================================================================\n")

exposures <- c(ADI = "adi10", SDI = "sdi10",
               SVI_overall = "svi10", SVI_SES = "svi_ses10")

fixed_effects_all <- list()
stratum_rrs_all   <- list()
desert_all        <- list()

for (ix in names(exposures)) {
  v <- exposures[[ix]]
  d <- df %>% filter(!is.na(.data[[v]]), !is.na(ruca_cat))
  cat(sprintf("\n--- %s (n = %s ZCTAs) ---\n", ix, format(nrow(d), big.mark = ",")))

  f1 <- as.formula(paste0("n_dentists_total ~ ", v, " + (1 | state)"))
  f2 <- as.formula(paste0("n_dentists_total ~ ", v, " + ruca_cat + (1 | state)"))
  f3 <- as.formula(paste0("n_dentists_total ~ ", v, " * ruca_cat + (1 | state)"))
  f4 <- as.formula(paste0("n_dentists_total ~ ", v, " + ruca_cat + (", v, " | state)"))

  m1 <- glmmTMB(f1, data = d, family = nbinom2, offset = d$logpop)
  m2 <- glmmTMB(f2, data = d, family = nbinom2, offset = d$logpop)
  m3 <- glmmTMB(f3, data = d, family = nbinom2, offset = d$logpop)
  m4 <- glmmTMB(f4, data = d, family = nbinom2, offset = d$logpop)

  for (nm in c("m1","m2","m3","m4")) {
    tt <- tidy_fixed(get(nm)) %>%
      mutate(index = ix, model = toupper(nm), .before = 1)
    fixed_effects_all[[paste(ix, nm)]] <- tt
  }

  rr1 <- tidy_fixed(m1) %>% filter(term == v)
  rr2 <- tidy_fixed(m2) %>% filter(term == v)
  cat(sprintf("  M1 RR/10pct: %.4f (%.4f-%.4f) | M2 (+RUCA): %.4f (%.4f-%.4f)\n",
              rr1$estimate, rr1$conf.low, rr1$conf.high,
              rr2$estimate, rr2$conf.low, rr2$conf.high))

  # M3 stratum-specific RRs (metro = reference slope)
  b   <- fixef(m3)$cond
  Vc  <- vcov(m3)$cond
  strata <- c("Metropolitan" = NA, "Micropolitan" = "Micropolitan",
              "Small town" = "Small town", "Rural" = "Rural")
  for (s in names(strata)) {
    if (is.na(strata[[s]])) {
      est <- b[[v]]; se <- sqrt(Vc[v, v])
    } else {
      it  <- paste0(v, ":ruca_cat", strata[[s]])
      est <- b[[v]] + b[[it]]
      se  <- sqrt(Vc[v, v] + Vc[it, it] + 2 * Vc[v, it])
    }
    stratum_rrs_all[[paste(ix, s)]] <- tibble(
      index = ix, stratum = s,
      RR = exp(est), conf.low = exp(est - 1.96 * se),
      conf.high = exp(est + 1.96 * se)
    )
    cat(sprintf("    M3 %-12s RR/10pct: %.4f (%.4f-%.4f)\n",
                s, exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se)))
  }

  # M4 state-specific slopes: fixed slope + BLUP deviation
  re <- ranef(m4)$cond$state
  state_slopes <- tibble(
    state       = rownames(re),
    slope_blup  = fixef(m4)$cond[[v]] + re[[v]],
    RR_per10    = exp(slope_blup)
  ) %>% arrange(RR_per10)
  write_csv(state_slopes,
            file.path(output_dir, paste0("US_State_Slopes_", ix, ".csv")))
  cat(sprintf("    M4 state slopes: %d negative / %d positive (range RR %.3f-%.3f)\n",
              sum(state_slopes$RR_per10 < 1), sum(state_slopes$RR_per10 > 1),
              min(state_slopes$RR_per10), max(state_slopes$RR_per10)))

  # D1: dental desert model (binomial, ZCTA has zero dentists)
  fd <- as.formula(paste0("no_dentists ~ ", v, " * ruca_cat + (1 | state)"))
  d1 <- glmmTMB(fd, data = d, family = binomial)
  td <- tidy_fixed(d1) %>%
    mutate(index = ix, model = "D1_desert", .before = 1)
  desert_all[[ix]] <- td
  or_main <- td %>% filter(term == v)
  cat(sprintf("    D1 desert OR/10pct (metro): %.4f (%.4f-%.4f)\n",
              or_main$estimate, or_main$conf.low, or_main$conf.high))
}

# =============================================================================
# STEP 4: SAVE MODEL OUTPUTS
# =============================================================================
cat("\n================================================================\n")
cat(" SAVING MODEL OUTPUTS\n")
cat("================================================================\n")

write_csv(bind_rows(fixed_effects_all),
          file.path(output_dir, "US_Model_FixedEffects.csv"))
cat("  1. US_Model_FixedEffects.csv\n")

write_csv(bind_rows(stratum_rrs_all),
          file.path(output_dir, "US_M3_Stratum_RRs.csv"))
cat("  2. US_M3_Stratum_RRs.csv\n")

write_csv(bind_rows(desert_all),
          file.path(output_dir, "US_Desert_Models.csv"))
cat("  3. US_Desert_Models.csv\n")

cat("  4. US_State_Slopes_<INDEX>.csv (one per exposure, saved in loop)\n")
cat("  5. US_Quartile_Descriptives_by_Index.csv (saved in Step 2)\n")
cat("  6. US_Index_Concordance.csv (saved in Step 1)\n")

cat("\n================================================================\n")
cat(" SCRIPT 3 COMPLETE\n")
cat(" Next: Script 4 - figures (quartile gradients, state caterpillar plots)\n")
cat("================================================================\n")
cat("\nDone.\n")
