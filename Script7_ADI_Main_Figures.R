# =============================================================================
# SCRIPT 7 (NATIONAL): ADI-Only Main-Result Figures
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Purpose: The restructured manuscript presents main results under the ADI
# (primary index) with the three-index comparison demoted to a sensitivity
# section. This script generates the two new main figures:
#
#   NEW Fig 1 — Model-predicted nearby dental workforce (15-mile catchment)
#               across the ADI percentile distribution. Single curve + CI
#               ribbon; the workforce disparity itself, no index comparison.
#   NEW Fig 2 — Dental desert prevalence (no provider within 15 miles) by
#               ADI quartile, ADI only, with resident counts annotated.
#
# (The existing three-index convergence and index-geography figures become
#  Figures 3 and 4 in the sensitivity section; the state quadrant moves to
#  the supplement. No regeneration needed for those.)
#
# Inputs:  Merge/US_Analytic_With_ADI.csv, Models/US_Catchment_Supply.csv,
#          Models/US_Catchment_Desert_by_Quartile.csv
# Outputs: Figures/Fig1_ADI_Catchment_Gradient.(png|pdf)
#          Figures/Fig2_ADI_Dental_Deserts.(png|pdf)
#
# Runtime: one glmmTMB fit (~2-4 min) for the prediction curve.
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(glmmTMB)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
catch_file    <- file.path(base_dir, "Models", "US_Catchment_Supply.csv")
desert_file   <- file.path(base_dir, "Models", "US_Catchment_Desert_by_Quartile.csv")
fig_dir       <- file.path(base_dir, "Figures")

ADI_BLUE <- "#0072B2"

theme_ms <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "grey30"),
        legend.position = "none")

save_fig <- function(p, name, w, h) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h,
         dpi = 600, bg = "white")
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h,
         bg = "white")
  cat(sprintf("  Saved %s\n", name))
}

# =============================================================================
# STEP 1: DATA + 15-MILE ADI MODEL
# =============================================================================
cat("STEP 1: Fitting 15-mile ADI model for predictions...\n")

catch <- read_csv(catch_file, col_types = cols(zip5 = col_character()),
                  show_col_types = FALSE) %>%
  select(zip5, dent_15, pop_15)

d <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
              show_col_types = FALSE) %>%
  left_join(catch, by = "zip5") %>%
  filter(!is.na(adi_natrank), !is.na(ruca_cat), !is.na(dent_15), pop_15 > 0) %>%
  mutate(
    adi10    = adi_natrank / 10,
    y        = as.integer(round(dent_15)),
    logoff   = log(pop_15),
    ruca_cat = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                           "Small town", "Rural")),
    state    = factor(state)
  )

m15 <- glmmTMB(y ~ adi10 + ruca_cat + offset(logoff) + (1 | state),
               data = d, family = nbinom2)

nd <- tibble(
  adi10    = seq(1, 99, by = 2) / 10,
  ruca_cat = factor("Metropolitan", levels = levels(d$ruca_cat)),
  state    = NA,
  logoff   = log(10000)     # predictions per 10,000 nearby residents
)
pr <- predict(m15, newdata = nd, se.fit = TRUE, re.form = NA, type = "link")
pred <- nd %>%
  mutate(percentile = adi10 * 10,
         fit = exp(pr$fit),
         lo  = exp(pr$fit - 1.96 * pr$se.fit),
         hi  = exp(pr$fit + 1.96 * pr$se.fit))

# =============================================================================
# NEW FIGURE 1 — THE WORKFORCE DISPARITY
# =============================================================================
cat("\nFIGURE 1 (new): ADI catchment gradient...\n")

fig1 <- ggplot(pred, aes(x = percentile, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = ADI_BLUE, alpha = 0.15) +
  geom_line(color = ADI_BLUE, linewidth = 1.3) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "ADI national percentile (higher = more disadvantaged)",
    y = "Predicted dentists per 10,000 residents within 15 miles",
    title = "Communities with greater social disadvantage have less nearby\ndental workforce",
    subtitle = "Model-predicted 15-mile catchment supply across the ADI distribution (metropolitan reference,\nrurality- and state-adjusted). A community at the 90th disadvantage percentile has approximately\n38% less nearby dental workforce than one at the 10th percentile."
  ) +
  theme_ms

save_fig(fig1, "Fig1_ADI_Catchment_Gradient", w = 7, h = 5.2)

# =============================================================================
# NEW FIGURE 2 — DENTAL DESERTS UNDER ADI, WITH RESIDENT COUNTS
# =============================================================================
cat("\nFIGURE 2 (new): ADI dental deserts by quartile...\n")

des <- read_csv(desert_file, show_col_types = FALSE) %>%
  filter(index == "ADI") %>%
  mutate(q = factor(q, levels = c("Q1 (least)", "Q2", "Q3", "Q4 (most)")),
         residents = formatC(pop_in_deserts, format = "d", big.mark = ","))

fig2 <- ggplot(des, aes(x = q, y = pct_desert15)) +
  geom_col(fill = ADI_BLUE, width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", pct_desert15)),
            vjust = -0.5, size = 3.6, fontface = "bold") +
  geom_text(aes(y = pct_desert15 / 2,
                label = paste0(residents, "\nresidents")),
            color = "white", size = 2.9, lineheight = 0.95) +
  scale_y_continuous(limits = c(0, 9.6), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "ADI disadvantage quartile",
    y = "ZCTAs that are dental deserts (%)",
    title = "Dental deserts concentrate in the most disadvantaged communities",
    subtitle = "Dental desert: no dental provider within 15 miles of the ZCTA centroid. 1.70 million US residents\nlive in dental deserts; prevalence rises six-fold from the least to the most disadvantaged quartile."
  ) +
  theme_ms

save_fig(fig2, "Fig2_ADI_Dental_Deserts", w = 7, h = 5.2)

cat("\n================================================================\n")
cat(" SCRIPT 7 COMPLETE - two ADI-only main figures saved\n")
cat("================================================================\n")
cat("\nDone.\n")
