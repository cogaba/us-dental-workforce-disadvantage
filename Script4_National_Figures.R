# =============================================================================
# SCRIPT 4 v2 (NATIONAL): Manuscript Figures — Catchment-Primary Framing
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Figure plan (revised after catchment analysis):
#   Fig 1 — CONVERGENCE (centerpiece): gradient RR under each index across
#           supply scales (within-ZCTA -> 10 -> 15 -> 25 mi). Positive
#           gradients decay to/through null; ADI negative at every scale.
#   Fig 2 — MECHANISM: mean disadvantage percentile by RUCA category
#           (corrected subtitle: ADI's ranking is steeply tied to rurality;
#           SDI/SVI comparatively flat).
#   Fig 3 — DESERTS: 15-mile catchment desert prevalence by disadvantage
#           quartile under each index (monotone under ADI; flat otherwise).
#   Fig 4 — FRAGILITY: state quadrant scatter of within-ZCTA slopes
#           (ADI vs SDI), DC labeled, framed as the fine-scale caution.
#
# Supplement:
#   FigS1 — Within-ZCTA predicted-density curves (four indices)
#   FigS2 — State caterpillar plots (four indices), legend fixed
#
# Inputs: Models/US_Catchment_Gradient_Table.csv,
#         Models/US_Catchment_Desert_by_Quartile.csv,
#         Models/US_State_Slopes_*.csv,
#         Merge/US_Analytic_With_ADI.csv, SVI_2022_US_ZCTA.csv
# Outputs: Figures/ (PNG 600 dpi + PDF)
#
# Runtime: FigS1 refits four M2 models (~5-10 min); all else is instant.
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(glmmTMB)

# -----------------------------------------------------------------------------
# 0. PATHS & SHARED AESTHETICS
# -----------------------------------------------------------------------------
# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"

analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
svi_file      <- file.path(base_dir, "SVI_2022_US_ZCTA.csv")
models_dir    <- file.path(base_dir, "Models")
fig_dir       <- file.path(base_dir, "Figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

index_colors <- c("ADI" = "#0072B2", "SDI" = "#E69F00", "SVI" = "#CC79A7",
                  "SVI SES" = "#009E73")
index_labels <- c("ADI" = "ADI (Area Deprivation Index)",
                  "SDI" = "SDI (Social Deprivation Index)",
                  "SVI" = "SVI (Social Vulnerability Index, overall)",
                  "SVI SES" = "SVI SES theme only")

theme_ms <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.title  = element_blank())

save_fig <- function(p, name, w, h) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h,
         dpi = 600, bg = "white")
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h,
         bg = "white")
  cat(sprintf("  Saved %s\n", name))
}

# =============================================================================
# FIGURE 1 — CONVERGENCE ACROSS SUPPLY SCALES (centerpiece)
# =============================================================================
cat("FIGURE 1: scale convergence...\n")

grad <- read_csv(file.path(models_dir, "US_Catchment_Gradient_Table.csv"),
                 show_col_types = FALSE) %>%
  mutate(
    supply_scale = factor(supply_scale,
                          levels = c("Within-ZCTA", "10-mile", "15-mile", "25-mile")),
    index = factor(index, levels = c("ADI", "SDI", "SVI"))
  )

pd <- position_dodge(width = 0.35)
fig1 <- ggplot(grad, aes(x = supply_scale, y = RR_per10,
                         color = index, group = index)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(position = pd, linewidth = 0.8, alpha = 0.7) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = pd, size = 0.55, linewidth = 0.7) +
  scale_color_manual(values = index_colors, labels = index_labels) +
  scale_y_continuous(breaks = seq(0.85, 1.10, 0.05)) +
  labs(
    x = "Supply measurement scale",
    y = "Rate ratio per 10 disadvantage percentiles",
    title = "Disadvantage gradients converge at travel-relevant supply scales",
    subtitle = "Within-ZCTA density yields index-dependent, opposite-signed gradients; measuring supply within\n10\u201325 miles eliminates the apparent surplus in disadvantaged areas under every index.\nRR < 1 = less dental workforce with greater disadvantage. CIs do not account for catchment overlap."
  ) +
  guides(color = guide_legend(nrow = 2)) +
  theme_ms

save_fig(fig1, "Fig1_Gradient_Convergence_by_Scale", w = 7.5, h = 5.5)

# =============================================================================
# FIGURE 2 — MECHANISM: INDEX GEOGRAPHY BY RURALITY (corrected subtitle)
# =============================================================================
cat("\nFIGURE 2: index geography by rurality...\n")

svi_df <- read_csv(svi_file, col_types = cols(.default = col_character()),
                   show_col_types = FALSE) %>%
  transmute(zip5 = str_pad(trimws(FIPS), 5, "left", "0"),
            svi_overall = as.numeric(RPL_THEMES) * 100,
            svi_ses     = as.numeric(RPL_THEME1) * 100) %>%
  mutate(across(c(svi_overall, svi_ses), ~ if_else(.x < 0, NA_real_, .x))) %>%
  distinct(zip5, .keep_all = TRUE)

df <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
               show_col_types = FALSE) %>%
  left_join(svi_df, by = "zip5") %>%
  mutate(ruca_cat = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                                "Small town", "Rural")))

exposure_vars <- c("ADI" = "adi_natrank", "SDI" = "sdi_score",
                   "SVI" = "svi_overall", "SVI SES" = "svi_ses")

mech_df <- purrr::map_dfr(names(exposure_vars), function(ix) {
  v <- exposure_vars[[ix]]
  df %>%
    filter(!is.na(.data[[v]]), !is.na(ruca_cat)) %>%
    group_by(ruca_cat) %>%
    summarise(mean_pct = mean(.data[[v]]),
              se = sd(.data[[v]]) / sqrt(n()), .groups = "drop") %>%
    mutate(index = factor(ix, levels = names(exposure_vars)))
})

fig2 <- ggplot(mech_df, aes(x = ruca_cat, y = mean_pct,
                            color = index, group = index)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_pct - 1.96 * se, ymax = mean_pct + 1.96 * se),
                width = 0.08, linewidth = 0.5) +
  scale_color_manual(values = index_colors, labels = index_labels) +
  labs(
    x = NULL,
    y = "Mean disadvantage percentile of ZCTAs",
    title = "What each index considers disadvantaged",
    subtitle = "ADI ranks nonmetropolitan ZCTAs far more disadvantaged than metropolitan ones;\nSDI and SVI vary comparatively little across the urban\u2013rural continuum"
  ) +
  guides(color = guide_legend(nrow = 2)) +
  theme_ms

save_fig(fig2, "Fig2_Index_Geography_by_RUCA", w = 7, h = 5)

# =============================================================================
# FIGURE 3 — 15-MILE CATCHMENT DESERTS BY QUARTILE, THREE INDICES
# =============================================================================
cat("\nFIGURE 3: catchment desert prevalence...\n")

des <- read_csv(file.path(models_dir, "US_Catchment_Desert_by_Quartile.csv"),
                show_col_types = FALSE) %>%
  mutate(q = factor(q, levels = c("Q1 (least)", "Q2", "Q3", "Q4 (most)")),
         index = factor(index, levels = c("ADI", "SDI", "SVI")))

fig3 <- ggplot(des, aes(x = q, y = pct_desert15, fill = index)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct_desert15)),
            position = position_dodge(width = 0.78),
            vjust = -0.4, size = 2.9) +
  scale_fill_manual(values = index_colors[c("ADI", "SDI", "SVI")],
                    labels = index_labels[c("ADI", "SDI", "SVI")]) +
  scale_y_continuous(limits = c(0, 10.5), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Disadvantage quartile",
    y = "ZCTAs with no dental provider within 15 miles (%)",
    title = "Areas without nearby dental providers concentrate where ADI\nidentifies disadvantage",
    subtitle = "1.70 million residents live in ZCTAs with no dental provider within 15 miles. Their prevalence\nrises six-fold across ADI quartiles but is flat under SDI and SVI \u2014 index choice determines\nwhich communities are identified for workforce targeting."
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_ms

save_fig(fig3, "Fig3_Catchment_Deserts_by_Quartile", w = 7.5, h = 5.5)

# =============================================================================
# FIGURE 4 — FRAGILITY: WITHIN-ZCTA STATE QUADRANT (DC labeled, framed as caution)
# =============================================================================
cat("\nFIGURE 4: within-ZCTA state quadrant...\n")

sl_adi <- read_csv(file.path(models_dir, "US_State_Slopes_ADI.csv"),
                   show_col_types = FALSE) %>% select(state, RR_ADI = RR_per10)
sl_sdi <- read_csv(file.path(models_dir, "US_State_Slopes_SDI.csv"),
                   show_col_types = FALSE) %>% select(state, RR_SDI = RR_per10)

quad <- inner_join(sl_adi, sl_sdi, by = "state") %>%
  mutate(
    quadrant = case_when(
      RR_ADI < 1 & RR_SDI < 1 ~ "Concordant negative",
      RR_ADI > 1 & RR_SDI > 1 ~ "Concordant positive",
      TRUE                    ~ "Discordant (index-dependent)"
    ),
    lab = state %in% c("DC", "CA", "TX", "NY", "FL", "HI", "VT", "ND", "AK",
                       "NJ", "WV", "ME", "SC")
  )

fig4 <- ggplot(quad, aes(x = RR_ADI, y = RR_SDI)) +
  annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf,
           fill = "grey85", alpha = 0.45) +
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1,
           fill = "grey85", alpha = 0.45) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_point(aes(color = quadrant), size = 2.6, alpha = 0.9) +
  geom_text(data = filter(quad, lab), aes(label = state),
            size = 3, vjust = -0.8, check_overlap = TRUE) +
  scale_color_manual(values = c("Concordant negative" = "#0072B2",
                                "Concordant positive" = "#E69F00",
                                "Discordant (index-dependent)" = "#D55E00")) +
  labs(
    x = "State gradient under ADI (RR per 10 percentiles)",
    y = "State gradient under SDI (RR per 10 percentiles)",
    title = "Fine-scale fragility: within-ZCTA state gradients flip with the index",
    subtitle = "Each point is a state (within-ZCTA random-slope estimates). In 35 of 51 jurisdictions the\ngradient direction depends on the index \u2014 always ADI-negative/SDI-positive, never the reverse.\nOnly NJ, CA, and FL are negative, and only UT, WY, SD, MT, and ND positive, under all indices."
  ) +
  guides(color = guide_legend(nrow = 2)) +
  theme_ms

save_fig(fig4, "Fig4_WithinZCTA_State_Quadrant", w = 7, h = 6)

# =============================================================================
# SUPPLEMENT S1 — WITHIN-ZCTA PREDICTED DENSITY CURVES (four indices)
# =============================================================================
cat("\nFIG S1: within-ZCTA predicted curves (refits four M2 models)...\n")

df_m <- df %>%
  mutate(state = factor(state), logpop = log(population))

pred_all <- list()
for (ix in names(exposure_vars)) {
  v <- exposure_vars[[ix]]
  d <- df_m %>% filter(!is.na(.data[[v]]), !is.na(ruca_cat))
  d$dep10 <- d[[v]] / 10
  m2 <- glmmTMB(n_dentists_total ~ dep10 + ruca_cat + offset(logpop) + (1 | state),
                data = d, family = nbinom2)
  nd <- tibble(dep10 = seq(1, 99, by = 2) / 10,
               ruca_cat = factor("Metropolitan", levels = levels(d$ruca_cat)),
               state = NA, logpop = log(10000))
  pr <- predict(m2, newdata = nd, se.fit = TRUE, re.form = NA, type = "link")
  pred_all[[ix]] <- nd %>%
    mutate(index = factor(ix, levels = names(exposure_vars)),
           percentile = dep10 * 10,
           fit = exp(pr$fit),
           lo = exp(pr$fit - 1.96 * pr$se.fit),
           hi = exp(pr$fit + 1.96 * pr$se.fit))
  cat(sprintf("  %s done\n", ix))
}

figS1 <- ggplot(bind_rows(pred_all),
                aes(x = percentile, y = fit, color = index, fill = index)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = index_colors, labels = index_labels) +
  scale_fill_manual(values = index_colors, labels = index_labels) +
  labs(x = "Disadvantage percentile (higher = more disadvantaged)",
       y = "Predicted dentists per 10,000 residents (within-ZCTA)",
       title = "Within-ZCTA density: same communities, opposite conclusions",
       subtitle = "Model-predicted within-ZCTA provider density by index (metropolitan reference,\nrurality- and state-adjusted); the fine-scale pattern motivating the catchment analysis") +
  guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
  theme_ms

save_fig(figS1, "FigS1_WithinZCTA_Predicted_Curves", w = 7, h = 5.5)

# =============================================================================
# SUPPLEMENT S2 — STATE CATERPILLARS (legend fixed: stacked, wider canvas)
# =============================================================================
cat("\nFIG S2: state caterpillar plots...\n")

slope_files <- c("ADI" = "US_State_Slopes_ADI.csv",
                 "SDI" = "US_State_Slopes_SDI.csv",
                 "SVI" = "US_State_Slopes_SVI_overall.csv",
                 "SVI SES" = "US_State_Slopes_SVI_SES.csv")

for (ix in names(slope_files)) {
  d <- read_csv(file.path(models_dir, slope_files[[ix]]),
                show_col_types = FALSE) %>%
    mutate(state = reorder(state, RR_per10))
  p <- ggplot(d, aes(x = RR_per10, y = state, color = RR_per10 < 1)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
    geom_point(size = 1.8) +
    scale_color_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#E69F00"),
                       labels = c("TRUE"  = "Negative (less workforce with disadvantage)",
                                  "FALSE" = "Positive (more workforce with disadvantage)")) +
    labs(x = "State-specific RR per 10 percentiles", y = NULL,
         title = paste0("Within-ZCTA state gradients: ", ix), color = NULL) +
    guides(color = guide_legend(nrow = 2)) +
    theme_ms +
    theme(axis.text.y = element_text(size = 6))
  save_fig(p, paste0("FigS2_Caterpillar_", gsub(" ", "_", ix)), w = 6, h = 7.5)
}

cat("\n================================================================\n")
cat(" SCRIPT 4 v2 COMPLETE - figure set for catchment-primary framing\n")
cat("================================================================\n")
cat("\nDone.\n")
