# =============================================================================
# SCRIPT 12 (NATIONAL): SVI Theme Decomposition at the 15-Mile Scale
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# PURPOSE AND FRAMING (important — read before interpreting)
#   The overall SVI shows essentially no association with 15-mile dental
#   workforce availability (RR 1.006). This script asks whether that null
#   reflects CANCELLATION of opposing component associations. The four SVI
#   themes are population characteristics, NOT causes of workforce shortage;
#   results are interpreted as a decomposition that informs which dimensions
#   of vulnerability track workforce scarcity (a targeting question), not as
#   an etiologic analysis.
#
#   Scale is FIXED at 15 miles — the paper's pre-specified primary scale.
#   No scale selection is performed for this subsection.
#
# SVI themes (CDC/ATSDR 2022):
#   Theme 1 (RPL_THEME1) Socioeconomic status
#   Theme 2 (RPL_THEME2) Household characteristics
#   Theme 3 (RPL_THEME3) Racial and ethnic minority status
#   Theme 4 (RPL_THEME4) Housing type and transportation
#
# Models (all: NB2, offset log(15-mile population), RUCA fixed effect,
#         state random intercept; exposure per 10 percentiles):
#   A. Four SEPARATE single-theme models (marginal associations)
#   B. One JOINT model with all four themes entered simultaneously
#      (mutually adjusted; interpret alongside the theme correlation matrix)
#   Also reported: theme intercorrelations and overall-SVI reference model.
#
# Inputs:  SVI_2022_US_ZCTA.csv, Merge/US_Analytic_With_ADI.csv,
#          Models/US_Catchment_Supply.csv
# Outputs: Models/US_SVI_Theme_Correlations.csv
#          Models/US_SVI_Theme_Models.csv
#
# Runtime: six glmmTMB fits (~20-35 min).
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(glmmTMB)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
svi_file      <- file.path(base_dir, "SVI_2022_US_ZCTA.csv")
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
catch_file    <- file.path(base_dir, "Models", "US_Catchment_Supply.csv")
output_dir    <- file.path(base_dir, "Models")

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

theme_labels <- c(
  svi_t1 = "Theme 1: Socioeconomic status",
  svi_t2 = "Theme 2: Household characteristics",
  svi_t3 = "Theme 3: Racial and ethnic minority status",
  svi_t4 = "Theme 4: Housing type and transportation"
)

# =============================================================================
# STEP 1: LOAD SVI THEMES + ASSEMBLE ANALYTIC DATASET
# =============================================================================
cat("================================================================\n")
cat("STEP 1: Assembling data (15-mile scale, four SVI themes)...\n")
cat("================================================================\n")

svi <- read_csv(svi_file, col_types = cols(.default = col_character()),
                show_col_types = FALSE) %>%
  transmute(
    zip5    = str_pad(str_trim(FIPS), 5, "left", "0"),
    svi_all = as.numeric(RPL_THEMES),
    svi_t1  = as.numeric(RPL_THEME1),
    svi_t2  = as.numeric(RPL_THEME2),
    svi_t3  = as.numeric(RPL_THEME3),
    svi_t4  = as.numeric(RPL_THEME4)
  ) %>%
  mutate(across(starts_with("svi"), ~ if_else(.x < 0, NA_real_, .x))) %>%
  distinct(zip5, .keep_all = TRUE)

catch <- read_csv(catch_file, col_types = cols(zip5 = col_character()),
                  show_col_types = FALSE) %>% select(zip5, dent_15, pop_15)

d <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
              show_col_types = FALSE) %>%
  left_join(svi,   by = "zip5") %>%
  left_join(catch, by = "zip5") %>%
  filter(!is.na(ruca_cat), !is.na(dent_15), pop_15 > 0,
         !is.na(svi_all), !is.na(svi_t1), !is.na(svi_t2),
         !is.na(svi_t3), !is.na(svi_t4)) %>%
  mutate(
    # per 10 percentiles: themes are 0-1 percentile ranks, so x10
    across(c(svi_all, svi_t1, svi_t2, svi_t3, svi_t4), ~ .x * 10,
           .names = "{.col}_10"),
    y        = as.integer(round(dent_15)),
    logoff   = log(pop_15),
    ruca_cat = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                           "Small town", "Rural")),
    state    = factor(state)
  )

cat(sprintf("  Analytic ZCTAs with complete theme data: %s\n",
            format(nrow(d), big.mark = ",")))

# =============================================================================
# STEP 2: THEME INTERCORRELATIONS (needed to interpret the joint model)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 2: Theme intercorrelations (Spearman)...\n")
cat("================================================================\n")

cm <- d %>%
  select(svi_t1, svi_t2, svi_t3, svi_t4, svi_all) %>%
  cor(method = "spearman")
print(round(cm, 3))
as.data.frame(cm) %>%
  mutate(variable = rownames(cm), .before = 1) %>%
  write_csv(file.path(output_dir, "US_SVI_Theme_Correlations.csv"))

# =============================================================================
# STEP 3: MODELS
# =============================================================================
cat("\n================================================================\n")
cat("STEP 3: Fitting models (six fits; the long step)...\n")
cat("================================================================\n")

results <- list()

# --- Reference: overall SVI at 15 miles (should reproduce ~1.006) ---
m_all <- glmmTMB(y ~ svi_all_10 + ruca_cat + offset(logoff) + (1 | state),
                 data = d, family = nbinom2)
results[["overall"]] <- tidy_fixed(m_all) %>%
  filter(term == "svi_all_10") %>%
  mutate(model = "Reference: overall SVI", theme = "SVI overall", .before = 1)
cat(sprintf("  Overall SVI (reference): RR %.4f (%.4f-%.4f)\n",
            results[["overall"]]$estimate, results[["overall"]]$conf.low,
            results[["overall"]]$conf.high))

# --- A. Four separate single-theme models (marginal) ---
cat("\n  Single-theme models (marginal associations):\n")
for (v in names(theme_labels)) {
  f <- as.formula(paste0("y ~ ", v, "_10 + ruca_cat + offset(logoff) + (1 | state)"))
  m <- glmmTMB(f, data = d, family = nbinom2)
  tt <- tidy_fixed(m) %>% filter(term == paste0(v, "_10")) %>%
    mutate(model = "Single-theme (marginal)", theme = theme_labels[[v]], .before = 1)
  results[[paste0("marg_", v)]] <- tt
  cat(sprintf("    %-45s RR %.4f (%.4f-%.4f) p = %.3g\n",
              theme_labels[[v]], tt$estimate, tt$conf.low, tt$conf.high, tt$p.value))
}

# --- B. Joint model: all four themes simultaneously (mutually adjusted) ---
cat("\n  Joint model (all four themes, mutually adjusted):\n")
m_joint <- glmmTMB(
  y ~ svi_t1_10 + svi_t2_10 + svi_t3_10 + svi_t4_10 +
      ruca_cat + offset(logoff) + (1 | state),
  data = d, family = nbinom2)
tj <- tidy_fixed(m_joint) %>%
  filter(str_detect(term, "^svi_t")) %>%
  mutate(theme = theme_labels[str_remove(term, "_10")],
         model = "Joint (mutually adjusted)", .before = 1)
results[["joint"]] <- tj
for (i in seq_len(nrow(tj))) {
  cat(sprintf("    %-45s RR %.4f (%.4f-%.4f) p = %.3g\n",
              tj$theme[i], tj$estimate[i], tj$conf.low[i], tj$conf.high[i], tj$p.value[i]))
}

out <- bind_rows(results) %>%
  select(model, theme, term, estimate, conf.low, conf.high, p.value)
write_csv(out, file.path(output_dir, "US_SVI_Theme_Models.csv"))

# =============================================================================
# STEP 4: CANCELLATION CHECK (for the Results narrative)
# =============================================================================
cat("\n================================================================\n")
cat(" INTERPRETATION AIDS\n")
cat("================================================================\n")
marg <- bind_rows(results[grep("^marg_", names(results))])
cat(sprintf("  Themes with inverse marginal association (RR < 1): %d of 4\n",
            sum(marg$estimate < 1)))
cat(sprintf("  Themes with positive marginal association (RR > 1): %d of 4\n",
            sum(marg$estimate > 1)))
cat("  If themes run in opposing directions while overall SVI is ~null,\n")
cat("  the null reflects cancellation of component associations.\n")
cat("\n  NOTE: themes are population characteristics, not causes of shortage.\n")
cat("  Report as decomposition for targeting, not etiology.\n")

cat("\n================================================================\n")
cat(" SCRIPT 12 COMPLETE\n")
cat("================================================================\n")
cat("\nDone.\n")
