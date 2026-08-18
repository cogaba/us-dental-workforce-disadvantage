# =============================================================================
# SCRIPT 6 (NATIONAL): Exploratory Analysis — Medicaid Adult Dental Benefit
#                      Generosity and the Catchment-Scale Disadvantage Gradient
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# FRAMING (important): This is a pre-specified EXPLORATORY, ASSOCIATIONAL
# analysis. With 51 state-level policy units, cross-sectional data, and
# benefit generosity correlated with state wealth, urbanization, and policy
# environment, no causal interpretation is warranted. Reported as one
# results paragraph + supplementary table/figure.
#
# Design:
#   Exposure scale: 15-mile catchment supply (primary scale), ADI exposure
#   (the index with the robust gradient at travel-relevant scales).
#   Moderator: state Medicaid adult dental benefit category (KFF Fig 2 /
#   NASHP tracker, Oct 2022), collapsed to three levels because only three
#   states have "none": Extensive (26) / Limited (14) / Emergency-None (11).
#
#   Model A (pooled cross-level interaction):
#     dent_15 ~ adi10 * benefit_cat3 + ruca_cat + (1 | state),
#     NB2, offset log(pop_15). Random intercepts absorb within-state
#     clustering; the category is a between-state fixed effect.
#     Category-specific gradients derived with delta-method CIs.
#   Model B (descriptive): state-specific 15-mile ADI slopes from a
#     random-slope model, summarized by benefit category.
#
# Known temporal caveat (documented in source file): HI, KY, MD, NH, TN
# report benefit changes after the Oct 2022 classification; sensitivity
# excluding these five states is included.
#
# Inputs:  medicaid_dental_2022.csv, Merge/US_Analytic_With_ADI.csv,
#          Models/US_Catchment_Supply.csv
# Outputs: Models/US_Medicaid_Moderation_Results.csv
#          Models/US_State_Slopes15_by_Medicaid.csv
#          Figures/FigS3_State_Slopes15_by_Medicaid.(png|pdf)
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(glmmTMB)

# -----------------------------------------------------------------------------
# 0. PATHS + HELPER
# -----------------------------------------------------------------------------
# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"

medicaid_file <- file.path(base_dir, "medicaid_dental_2022.csv")
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
catch_file    <- file.path(base_dir, "Models", "US_Catchment_Supply.csv")
models_dir    <- file.path(base_dir, "Models")
fig_dir       <- file.path(base_dir, "Figures")

# =============================================================================
# STEP 1: ASSEMBLE DATASET
# =============================================================================
cat("STEP 1: Assembling dataset...\n")

med <- read_csv(medicaid_file, show_col_types = FALSE) %>%
  mutate(benefit_cat3 = case_when(
    benefit_cat == "extensive"                 ~ "Extensive",
    benefit_cat == "limited"                   ~ "Limited",
    benefit_cat %in% c("emergency", "none")    ~ "Emergency-None"
  ))
cat("  Category counts:\n"); print(count(med, benefit_cat3))

catch <- read_csv(catch_file, col_types = cols(zip5 = col_character()),
                  show_col_types = FALSE) %>%
  select(zip5, dent_15, pop_15)

df <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
               show_col_types = FALSE) %>%
  left_join(catch, by = "zip5") %>%
  left_join(med %>% select(state_abbr, benefit_cat3),
            by = c("state" = "state_abbr")) %>%
  filter(!is.na(adi_natrank), !is.na(ruca_cat), !is.na(benefit_cat3),
         !is.na(dent_15), pop_15 > 0) %>%
  mutate(
    adi10        = adi_natrank / 10,
    y            = as.integer(round(dent_15)),
    logoff       = log(pop_15),
    ruca_cat     = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                               "Small town", "Rural")),
    state        = factor(state),
    benefit_cat3 = factor(benefit_cat3,
                          levels = c("Extensive", "Limited", "Emergency-None")),
    changed_post2022 = state %in% c("HI", "KY", "MD", "NH", "TN")
  )
cat(sprintf("  Analytic ZCTAs: %s across %d states\n",
            format(nrow(df), big.mark = ","), n_distinct(df$state)))

# =============================================================================
# STEP 2: MODEL A — POOLED CROSS-LEVEL INTERACTION
# =============================================================================
cat("\nSTEP 2: Model A (cross-level interaction, glmmTMB)...\n")

fit_moderation <- function(d, label) {
  mA <- glmmTMB(y ~ adi10 * benefit_cat3 + ruca_cat + offset(logoff) + (1 | state),
                data = d, family = nbinom2)
  b  <- fixef(mA)$cond
  Vc <- vcov(mA)$cond
  out <- list()
  for (cat in levels(d$benefit_cat3)) {
    if (cat == "Extensive") {
      est <- b[["adi10"]]; se <- sqrt(Vc["adi10", "adi10"]); pint <- NA_real_
    } else {
      it  <- paste0("adi10:benefit_cat3", cat)
      est <- b[["adi10"]] + b[[it]]
      se  <- sqrt(Vc["adi10", "adi10"] + Vc[it, it] + 2 * Vc["adi10", it])
      pint <- 2 * pnorm(-abs(b[[it]] / sqrt(Vc[it, it])))
    }
    out[[cat]] <- tibble(
      analysis = label, benefit_cat3 = cat,
      RR_per10 = exp(est),
      conf.low = exp(est - 1.96 * se), conf.high = exp(est + 1.96 * se),
      interaction_p = pint
    )
  }
  bind_rows(out)
}

resA  <- fit_moderation(df, "Primary (all states)")
resA2 <- fit_moderation(filter(df, !changed_post2022),
                        "Sensitivity (excl. HI/KY/MD/NH/TN)")
results <- bind_rows(resA, resA2)
print(results, n = Inf)

write_csv(results, file.path(models_dir, "US_Medicaid_Moderation_Results.csv"))
cat("  Saved US_Medicaid_Moderation_Results.csv\n")

# =============================================================================
# STEP 3: MODEL B — STATE SLOPES AT 15-MILE SCALE, BY CATEGORY (descriptive)
# =============================================================================
cat("\nSTEP 3: Model B (random slopes; state BLUPs by category)...\n")

mB <- glmmTMB(y ~ adi10 + ruca_cat + offset(logoff) + (adi10 | state),
              data = df, family = nbinom2)
re <- ranef(mB)$cond$state
state_slopes15 <- tibble(
  state    = rownames(re),
  RR15     = exp(fixef(mB)$cond[["adi10"]] + re[["adi10"]])
) %>%
  left_join(med %>% select(state_abbr, benefit_cat3),
            by = c("state" = "state_abbr")) %>%
  mutate(benefit_cat3 = factor(benefit_cat3,
                               levels = c("Extensive", "Limited", "Emergency-None")))

cat("  Mean state slope by category:\n")
print(state_slopes15 %>% group_by(benefit_cat3) %>%
        summarise(n = n(), mean_RR15 = round(mean(RR15), 4),
                  median_RR15 = round(median(RR15), 4), .groups = "drop"))

write_csv(state_slopes15, file.path(models_dir, "US_State_Slopes15_by_Medicaid.csv"))
cat("  Saved US_State_Slopes15_by_Medicaid.csv\n")

# =============================================================================
# STEP 4: FIGURE S3 — STATE SLOPES BY MEDICAID CATEGORY
# =============================================================================
cat("\nSTEP 4: Figure S3...\n")

figS3 <- ggplot(state_slopes15,
                aes(x = benefit_cat3, y = RR15, color = benefit_cat3)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_jitter(width = 0.12, size = 2.2, alpha = 0.85) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.42,
               linewidth = 0.45, color = "grey20") +
  scale_color_manual(values = c("Extensive" = "#009E73",
                                "Limited" = "#E69F00",
                                "Emergency-None" = "#D55E00"), guide = "none") +
  labs(
    x = "State Medicaid adult dental benefit (Oct 2022)",
    y = "State-specific ADI gradient, 15-mile supply (RR per 10 percentiles)",
    title = "Exploratory: disadvantage gradients by Medicaid adult dental generosity",
    subtitle = "Each point is a state (random-slope estimates at the 15-mile catchment scale; crossbar = mean).\nStates with emergency-only or no adult dental benefits show steeper shortage gradients.\nAssociational only: 51 state-level units; generosity correlates with state wealth and policy context."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9.5, color = "grey30"))

ggsave(file.path(fig_dir, "FigS3_State_Slopes15_by_Medicaid.png"), figS3,
       width = 7, height = 5.5, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "FigS3_State_Slopes15_by_Medicaid.pdf"), figS3,
       width = 7, height = 5.5, bg = "white")
cat("  Saved FigS3_State_Slopes15_by_Medicaid\n")

cat("\n================================================================\n")
cat(" SCRIPT 6 COMPLETE - exploratory Medicaid moderation\n")
cat("================================================================\n")
cat("\nDone.\n")
