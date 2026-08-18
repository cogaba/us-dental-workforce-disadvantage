# =============================================================================
# SCRIPT 13 v2 (NATIONAL): State-Level Correlates of Dental Workforce Supply
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# REPLACES the previous cross-level interaction analysis (Script 9) with a
# simpler, policy-legible question set:
#   Do states with ...
#     (1) more generous adult Medicaid dental benefits,
#     (2) higher dentist Medicaid/CHIP participation,
#     (3) higher Medicaid FFS reimbursement relative to private insurance,
#   ... have higher dentist supply and fewer dental deserts?
#
# DESIGN
#   Unit of analysis: state (n = 51).
#   Outcomes (both computed from this study's own data):
#     O1: dentists per 10,000 residents (state total / state population)
#     O2: percentage of the state's ZCTAs that are dental deserts (no
#         dentist within 15 miles)
#   Exposures (ADA HPI, December 2025 data tables):
#     E1: adult Medicaid dental benefit level 2025 (Enhanced / Limited /
#         Emergency-or-None; also coded ordinally 3/2/1)
#     E2: % of dentists enrolled as Medicaid/CHIP providers, 2024
#     E3: adult Medicaid FFS reimbursement as % of private payment, 2024
#         (42 states with adult FFS benefits)
#   Context variable:
#     C1: state rural population share (share of state population living in
#         rural or small-town RUCA ZCTAs) - computed from this study's data,
#         included because rurality confounds every policy correlation.
#
# ANALYSIS
#   Spearman correlations (rank-based; robust to outliers and non-normality),
#   reported unadjusted AND adjusted for rural population share (partial
#   Spearman computed from rank residuals - no extra packages required).
#   Benefit level (categorical) summarized as group medians with a
#   Kruskal-Wallis test.
#
#   Interpretation: associational only. n = 51 states, cross-sectional.
#   Reverse causation is plausible for reimbursement and participation
#   (states may raise rates or recruit participants where supply is short).
#
# Inputs:  Dental_Care_in_Medicaid_Programs_Data_Tables.xlsx,
#          Merge/US_Analytic_With_ADI.csv,
#          Models/US_Catchment_Supply.csv
# Outputs: Models/US_State_Correlates_Table.csv   (state-level dataset)
#          Models/US_State_Correlations.csv       (correlation results)
#          Models/US_State_Benefit_Group_Summary.csv
#          Figures/Fig4_State_Correlates.(png|pdf)
#
# Runtime: seconds (no mixed models).
#
# v2 FIXES (supersede v1 — run this file, do not patch v1):
#   1. summarise() name collision: v1 created a column named `population`
#      before computing rural_pop_share, so `population[ruca_cat %in% ...]`
#      indexed a length-1 scalar and returned NA for all states except DC.
#      The state total is now named `total_population`.
#   2. Partial Spearman is computed from the three-correlation formula
#      (no lm/residuals), with guards so a thin variable yields NA instead
#      of aborting the loop.
#   3. Added explicit diagnostics after Steps 1 and 3 so a silent failure
#      cannot pass unnoticed.
#   Outcomes were unaffected by the v1 bug and are unchanged
#   (dentists per 10,000: median 7.63; % deserts: median 3.0).
# =============================================================================

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
if (!requireNamespace("patchwork", quietly = TRUE))
  stop("Please run install.packages('patchwork') first.")
library(patchwork)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
hpi_file      <- file.path(base_dir, "Dental_Care_in_Medicaid_Programs_Data_Tables.xlsx")
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
catch_file    <- file.path(base_dir, "Models", "US_Catchment_Supply.csv")
models_dir    <- file.path(base_dir, "Models")
fig_dir       <- file.path(base_dir, "Figures")

state_lookup <- tibble(state_name = c(state.name, "District of Columbia"),
                       state_abbr = c(state.abb, "DC"))

# =============================================================================
# STEP 1: STATE-LEVEL OUTCOMES FROM THIS STUDY'S DATA
# =============================================================================
cat("================================================================\n")
cat("STEP 1: Building state-level outcomes...\n")
cat("================================================================\n")

catch <- read_csv(catch_file, col_types = cols(zip5 = col_character()),
                  show_col_types = FALSE) %>% select(zip5, dent_15)

d <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
              show_col_types = FALSE) %>%
  left_join(catch, by = "zip5") %>%
  filter(!is.na(ruca_cat), !is.na(dent_15))

state_out <- d %>%
  group_by(state) %>%
  summarise(
    n_zctas          = n(),
    total_population = sum(population),
    dentists         = sum(n_dentists_total),
    dentists_per_10k = 10000 * sum(n_dentists_total) / sum(population),
    pct_deserts      = 100 * mean(dent_15 == 0),
    rural_pop_share  = 100 * sum(population[ruca_cat %in% c("Rural", "Small town")]) /
                       sum(population),
    .groups = "drop"
  )

# guard: rural share must be computed for all 51 states (v1 returned 1)
if (sum(is.finite(state_out$rural_pop_share)) != nrow(state_out))
  stop("rural_pop_share failed for some states - check ruca_cat values")

cat(sprintf("  States: %d | dentists per 10k: median %.2f (range %.2f-%.2f)\n",
            nrow(state_out), median(state_out$dentists_per_10k),
            min(state_out$dentists_per_10k), max(state_out$dentists_per_10k)))
cat(sprintf("  %% ZCTAs that are dental deserts: median %.1f%% (range %.1f-%.1f)\n",
            median(state_out$pct_deserts), min(state_out$pct_deserts),
            max(state_out$pct_deserts)))
cat(sprintf("  Rural/small-town population share: median %.1f%% (range %.1f-%.1f), all %d states\n",
            median(state_out$rural_pop_share), min(state_out$rural_pop_share),
            max(state_out$rural_pop_share), nrow(state_out)))

# =============================================================================
# STEP 2: STATE POLICY VARIABLES (ADA HPI, December 2025)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 2: Parsing HPI policy variables...\n")
cat("================================================================\n")

ben <- read_excel(hpi_file, sheet = "Adult Dental Benefits",
                  skip = 5, col_names = FALSE, col_types = "text") %>%
  setNames(c("state_name", "y2021", "y2022", "y2023", "y2024", "y2025")) %>%
  mutate(state_name = str_trim(state_name)) %>%
  inner_join(state_lookup, by = "state_name") %>%
  mutate(benefit25 = case_when(
    str_detect(tolower(coalesce(y2025, "")), "enhanced")  ~ "Enhanced",
    str_detect(tolower(coalesce(y2025, "")), "limited")   ~ "Limited",
    TRUE                                                  ~ "Emergency-or-None")) %>%
  select(state_abbr, benefit25)

par <- read_excel(hpi_file, sheet = "Dentist Medicaid Participation",
                  skip = 5, col_names = FALSE, col_types = "text") %>%
  setNames(c("state_name", "y2015", "y2016", "y2017", "y2019", "y2020",
             "y2021", "y2022", "y2023", "y2024")) %>%
  mutate(state_name = str_trim(state_name)) %>%
  inner_join(state_lookup, by = "state_name") %>%
  transmute(state_abbr, participation = 100 * as.numeric(y2024))

rmb <- read_excel(hpi_file, sheet = "FFS Reimbursement Adult",
                  skip = 5, col_names = FALSE, col_types = "text") %>%
  setNames(c("state_name", "chg2022", "chg2024", "chg2025",
             "prv2022", "prv2024", "prv2025")) %>%
  mutate(state_name = str_trim(state_name)) %>%
  inner_join(state_lookup, by = "state_name") %>%
  transmute(state_abbr, reimbursement = 100 * as.numeric(prv2024))

st <- state_out %>%
  left_join(ben, by = c("state" = "state_abbr")) %>%
  left_join(par, by = c("state" = "state_abbr")) %>%
  left_join(rmb, by = c("state" = "state_abbr")) %>%
  mutate(benefit_ord = case_when(benefit25 == "Enhanced" ~ 3,
                                 benefit25 == "Limited" ~ 2,
                                 TRUE ~ 1),
         benefit25 = factor(benefit25, levels = c("Enhanced", "Limited",
                                                  "Emergency-or-None")))

stopifnot(nrow(st) == 51)
cat("  Benefit level counts:\n"); print(count(st, benefit25))
cat(sprintf("  Participation: median %.1f%% | Reimbursement: %d states, median %.0f%%\n",
            median(st$participation), sum(!is.na(st$reimbursement)),
            median(st$reimbursement, na.rm = TRUE)))
write_csv(st, file.path(models_dir, "US_State_Correlates_Table.csv"))

# =============================================================================
# STEP 3: SPEARMAN CORRELATIONS, UNADJUSTED AND RURALITY-ADJUSTED
# =============================================================================
cat("\n================================================================\n")
cat("STEP 3: Correlations...\n")
cat("================================================================\n")

exposures <- list(
  "Adult Medicaid dental benefit level (ordinal)"   = st$benefit_ord,
  "Dentist Medicaid/CHIP participation, % (2024)"   = st$participation,
  "Medicaid FFS reimbursement, % of private (2024)" = st$reimbursement,
  "Rural/small-town population share, %"            = st$rural_pop_share
)
outcomes <- list(
  "Dentists per 10,000 residents"      = st$dentists_per_10k,
  "% of ZCTAs that are dental deserts" = st$pct_deserts
)

cat("  Variable check (finite values of 51):\n")
for (nm in c(names(exposures), names(outcomes))) {
  v <- if (nm %in% names(exposures)) exposures[[nm]] else outcomes[[nm]]
  cat(sprintf("    %-50s n = %2d\n", nm, sum(is.finite(v))))
}

# simple Spearman
sp <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 4) return(c(rho = NA_real_, p = NA_real_, n = sum(ok)))
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman"))
  c(rho = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}

# partial Spearman via the three-correlation formula (no lm, no residuals)
psp <- function(x, y, z) {
  ok <- is.finite(x) & is.finite(y) & is.finite(z)
  n  <- sum(ok)
  if (n < 6) return(c(rho = NA_real_, p = NA_real_, n = n))
  rxy <- cor(x[ok], y[ok], method = "spearman")
  rxz <- cor(x[ok], z[ok], method = "spearman")
  ryz <- cor(y[ok], z[ok], method = "spearman")
  den <- sqrt((1 - rxz^2) * (1 - ryz^2))
  if (!is.finite(den) || den == 0) return(c(rho = NA_real_, p = NA_real_, n = n))
  r <- (rxy - rxz * ryz) / den
  tstat <- r * sqrt((n - 3) / (1 - r^2))
  c(rho = r, p = 2 * pt(-abs(tstat), df = n - 3), n = n)
}

rows <- list()
for (on in names(outcomes)) {
  for (en in names(exposures)) {
    s_un <- sp(exposures[[en]], outcomes[[on]])
    s_ad <- if (en == "Rural/small-town population share, %") {
      c(rho = NA_real_, p = NA_real_, n = s_un[["n"]])   # no self-adjustment
    } else {
      psp(exposures[[en]], outcomes[[on]], st$rural_pop_share)
    }
    rows[[paste(on, en)]] <- tibble(
      outcome = on, exposure = en, n = s_un[["n"]],
      rho_unadjusted        = round(s_un[["rho"]], 3),
      p_unadjusted          = signif(s_un[["p"]], 3),
      rho_rurality_adjusted = round(s_ad[["rho"]], 3),
      p_rurality_adjusted   = signif(s_ad[["p"]], 3)
    )
  }
}
cors <- bind_rows(rows)
cat("\n")
print(cors, n = Inf, width = Inf)
if (all(is.na(cors$rho_rurality_adjusted[cors$exposure != "Rural/small-town population share, %"])))
  warning("All rurality-adjusted correlations are NA - check rural_pop_share")
write_csv(cors, file.path(models_dir, "US_State_Correlations.csv"))

# =============================================================================
# STEP 4: BENEFIT-LEVEL GROUP SUMMARY (categorical exposure)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 4: Benefit-level group summary...\n")
cat("================================================================\n")

grp <- st %>%
  group_by(benefit25) %>%
  summarise(n_states = n(),
            median_dentists_per_10k = round(median(dentists_per_10k), 2),
            median_pct_deserts      = round(median(pct_deserts), 1),
            median_rural_share      = round(median(rural_pop_share), 1),
            .groups = "drop")
print(grp)
kw1 <- kruskal.test(dentists_per_10k ~ benefit25, data = st)
kw2 <- kruskal.test(pct_deserts ~ benefit25, data = st)
cat(sprintf("  Kruskal-Wallis: dentist density p = %.3f | %% deserts p = %.3f\n",
            kw1$p.value, kw2$p.value))
write_csv(grp, file.path(models_dir, "US_State_Benefit_Group_Summary.csv"))

# =============================================================================
# STEP 5: FIGURE 4 — THREE SCATTERPLOTS (one per policy question)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 5: Figure 4...\n")
cat("================================================================\n")

theme_sc <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none")

pA <- ggplot(st, aes(x = benefit25, y = dentists_per_10k)) +
  geom_boxplot(outlier.shape = NA, width = 0.55, color = "grey35") +
  geom_jitter(width = 0.14, size = 2, alpha = 0.75, color = "#0072B2") +
  labs(x = "Adult Medicaid dental benefit, 2025",
       y = "Dentists per 10,000 residents") +
  theme_sc + theme(axis.text.x = element_text(size = 8))

pB <- ggplot(st, aes(x = participation, y = dentists_per_10k)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey35",
              linewidth = 0.6, alpha = 0.12) +
  geom_point(size = 2, alpha = 0.85, color = "#0072B2") +
  labs(x = "Dentists participating in Medicaid/CHIP, 2024 (%)",
       y = NULL) +
  theme_sc

pC <- ggplot(filter(st, !is.na(reimbursement)),
             aes(x = reimbursement, y = dentists_per_10k)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey35",
              linewidth = 0.6, alpha = 0.12) +
  geom_point(size = 2, alpha = 0.85, color = "#0072B2") +
  labs(x = "Medicaid FFS reimbursement, % of private (2024)",
       y = NULL) +
  theme_sc

fig4 <- pA + pB + pC +
  plot_layout(nrow = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(fig_dir, "Fig4_State_Correlates.png"), fig4,
       width = 11, height = 3.9, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Fig4_State_Correlates.pdf"), fig4,
       width = 11, height = 3.9, bg = "white")
cat("  Saved Fig4_State_Correlates\n")

cat("\n================================================================\n")
cat(" SCRIPT 13 v2 COMPLETE (supersedes Script 9 and Script 13 v1)\n")
cat("================================================================\n")
cat("\nDone.\n")
