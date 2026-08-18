# =============================================================================
# SCRIPT 11 v5 (NATIONAL): Figure 3 — Journal-Minimal, Harmonized Axis
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Changes from v3 (journal-minimal text policy):
#   - No in-image overall title; no panel titles (panels tagged A and B).
#   - Grey indexing note removed; 24.3M annotation removed — both live in
#     the caption. Bar labels trimmed to values only ("6.5%" over "1.9M").
#   - Retained in-figure text is strictly functional: axis labels, panel
#     tags, curve name labels (legend replacement), and data-value labels.
#

#
# Speed-up: the two glmmTMB fits are cached to Models/fig3_models.rds on the
# first run; subsequent runs (e.g., styling tweaks) reload them instantly.
#
# Inputs:  Merge/US_Analytic_With_ADI.csv,
#          Models/US_Specialist_Catchment_Supply.csv,
#          Models/US_Specialist_by_ADI_Quartile.csv
# Outputs: Figures/Fig3_Specialist_Inequity_v5.(png|pdf)
# =============================================================================

library(readr)
library(dplyr)
library(ggplot2)
library(glmmTMB)
if (!requireNamespace("patchwork", quietly = TRUE))
  stop("Please run install.packages('patchwork') first.")
library(patchwork)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
spec_file     <- file.path(base_dir, "Models", "US_Specialist_Catchment_Supply.csv")
quart_file    <- file.path(base_dir, "Models", "US_Specialist_by_ADI_Quartile.csv")
cache_file    <- file.path(base_dir, "Models", "fig3_models.rds")
fig_dir       <- file.path(base_dir, "Figures")

COL_GEN  <- "#1F77B4"
COL_SPEC <- "#E69F00"
COL_DES  <- "#C44E52"

YLAB_B  <- "Specialist dental deserts (% of ZCTAs)"

theme_panel <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        plot.title    = element_text(face = "bold", size = 11, hjust = 0),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "none")

# =============================================================================
# STEP 1: MODELS (cached) + NORMALIZED PREDICTIONS
# =============================================================================
if (file.exists(cache_file)) {
  cat("STEP 1: Loading cached models...\n")
  mods <- readRDS(cache_file); mA <- mods$mA; mB <- mods$mB
} else {
  cat("STEP 1: Fitting general and specialist 15-mile models...\n")
  catch <- read_csv(spec_file, col_types = cols(zip5 = col_character()),
                    show_col_types = FALSE) %>%
    select(zip5, gen_15, spec_15, pop_15)
  d <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
                show_col_types = FALSE) %>%
    left_join(catch, by = "zip5") %>%
    filter(!is.na(adi_natrank), !is.na(ruca_cat), !is.na(gen_15), pop_15 > 0) %>%
    mutate(adi10 = adi_natrank / 10,
           y_gen = as.integer(round(gen_15)),
           y_spec = as.integer(round(spec_15)),
           logoff = log(pop_15),
           ruca_cat = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                                  "Small town", "Rural")),
           state = factor(state))
  mA <- glmmTMB(y_gen  ~ adi10 + ruca_cat + offset(logoff) + (1 | state),
                data = d, family = nbinom2)
  mB <- glmmTMB(y_spec ~ adi10 + ruca_cat + offset(logoff) + (1 | state),
                data = d, family = nbinom2)
  saveRDS(list(mA = mA, mB = mB), cache_file)
  cat("  Models fitted and cached.\n")
}

predict_norm <- function(model, label) {
  nd <- tibble(
    adi10    = seq(1, 99, by = 1) / 10,
    ruca_cat = factor("Metropolitan",
                      levels = c("Metropolitan", "Micropolitan",
                                 "Small town", "Rural")),
    state    = NA,
    logoff   = log(10000)
  )
  pr <- predict(model, newdata = nd, se.fit = TRUE, re.form = NA, type = "link")
  out <- nd %>%
    mutate(percentile = adi10 * 10,
           fit = exp(pr$fit),
           lo  = exp(pr$fit - 1.96 * pr$se.fit),
           hi  = exp(pr$fit + 1.96 * pr$se.fit),
           group = label)
  ref <- out$fit[out$percentile == 10]
  out %>% mutate(fit = 100 * fit / ref, lo = 100 * lo / ref, hi = 100 * hi / ref)
}

pd <- bind_rows(predict_norm(mA, "General dentists"),
                predict_norm(mB, "Specialists"))

# =============================================================================
# STEP 2: PANEL A — OVERLAID CURVES, DIRECT END LABELS
# =============================================================================
end_lab <- pd %>%
  group_by(group) %>%
  filter(percentile == max(percentile)) %>%
  ungroup()

ymax <- max(pd$hi) * 1.04

panelA <- ggplot(pd, aes(x = percentile, y = fit,
                         color = group, fill = group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.16, color = NA) +
  geom_line(linewidth = 1.25) +
  geom_text(data = end_lab,
            aes(x = percentile + 2, y = fit, label = group),
            hjust = 0, size = 3.2, fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(values = c("General dentists" = COL_GEN,
                                "Specialists" = COL_SPEC)) +
  scale_fill_manual(values = c("General dentists" = COL_GEN,
                               "Specialists" = COL_SPEC)) +
  scale_x_continuous(limits = c(0, 126), breaks = seq(0, 100, 25)) +
  scale_y_continuous(limits = c(0, ymax), expand = expansion(mult = c(0, 0))) +
  labs(x = "ADI national percentile (higher = more disadvantaged)",
       y = "Relative workforce availability (%)") +
  theme_panel

# =============================================================================
# STEP 3: PANEL B — SPECIALIST DESERTS BY QUARTILE
# =============================================================================
qt <- read_csv(quart_file, show_col_types = FALSE) %>%
  mutate(
    q = factor(c("Q1", "Q2", "Q3", "Q4"),
               levels = c("Q1", "Q2", "Q3", "Q4")),
    pct_lab = sprintf("%.1f%%", pct_no_specialist_15mi),
    pop_lab = sprintf("%.2fM\nresidents",
                      pop_no_specialist_15mi / 1e6)
  )

total_m <- sum(qt$pop_no_specialist_15mi) / 1e6

panelB <- ggplot(qt, aes(x = q, y = pct_no_specialist_15mi)) +
  geom_col(fill = COL_DES, width = 0.62) +
  
  # Percentage above the bar
  geom_text(aes(label = pct_lab),
            vjust = -0.5,
            size = 3.0,
            fontface = "bold") +
  
  # Resident population inside the bar
  geom_text(aes(y = pct_no_specialist_15mi / 2,
                label = pop_lab),
            color = "white",
            size = 2.5,
            fontface = "bold",
            lineheight = 0.95) +
  
  scale_y_continuous(
    limits = c(0, 63),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    x = "ADI quartile",
    y = YLAB_B
  ) +
  
  theme_panel
# =============================================================================
# STEP 4: ASSEMBLE AND SAVE
# =============================================================================
fig3 <- panelA + panelB +
  plot_layout(widths = c(1.25, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(fig_dir, "Fig3_Specialist_Inequity_v5.png"), fig3,
       width = 10.5, height = 4.5, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Fig3_Specialist_Inequity_v5.pdf"), fig3,
       width = 10.5, height = 4.5, bg = "white")

cat(sprintf("\nSaved Fig3_Specialist_Inequity_v5 (total desert residents: %.2fM)\n", total_m))
cat("Done.\n")
