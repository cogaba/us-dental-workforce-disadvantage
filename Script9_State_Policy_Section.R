# =============================================================================
# SCRIPT 9 (NATIONAL): State Policy Environment and Workforce Distribution
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Section 3 of the paper. Single source: ADA Health Policy Institute,
# "Dental Care in Medicaid Programs" data tables (published December 2025).
# The earlier KFF-based analysis (Script 6) is SUPERSEDED and not used.
#
# Three state policy variables:
#   1. Adult Medicaid dental benefit level, 2025
#      (Enhanced / Limited / Emergency-only / [blank = no benefit];
#       collapsed to Enhanced / Limited / Emergency-or-None)
#   2. Dentist Medicaid/CHIP participation, 2024
#      (% of dentists enrolled as a Medicaid/CHIP provider; per 10 pp)
#      NOTE: enrollment-based measure - carries HPI's own caveat that
#      enrollment overstates engagement in "wide but shallow" states.
#   3. Adult Medicaid FFS reimbursement as % of private insurance payment,
#      2024 (per 10 pp; missing where states lack adult FFS dental benefits -
#      model runs on the reduced state set, reported)
#
# For each policy variable, two exploratory estimands at the 15-mile scale:
#   (a) MAIN EFFECT: association with overall nearby supply (between-state)
#   (b) MODERATION: cross-level interaction with the within-state ADI gradient
# Framed as descriptive/associational: 51 state-level units; policy variables
# are intercorrelated (correlation matrix reported) and correlated with
# broader state context.
#
# XLSX PARSING NOTES (verified against file structure):
#   - Every tab has a 4-row banner before the header block
#   - Year row sits BELOW the measure-label row (data starts after skip = 5)
#   - "United States" row must be dropped
#   - Benefit tab: blank cell = no adult dental benefit
#   - Reimbursement tab: TWO metric blocks side by side
#     (cols 2-4: % of charges 2022/2024/2025; cols 5-7: % of private 2022/2024/2025)
#
# Inputs:  Dental_Care_in_Medicaid_Programs_Data_Tables.xlsx,
#          Merge/US_Analytic_With_ADI.csv, Models/US_Catchment_Supply.csv,
#          Models/US_State_Slopes15_by_Medicaid.csv (RR15 column only;
#          the KFF grouping column in that file is ignored)
# Outputs: Models/US_State_Policy_Table.csv
#          Models/US_Policy_Correlations.csv
#          Models/US_Policy_Models.csv
#          Figures/FigS5_State_Slopes_by_Participation.(png|pdf)
#
# Runtime: three glmmTMB fits (~10-20 min).
# =============================================================================

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(glmmTMB)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
hpi_file      <- file.path(base_dir, "Dental_Care_in_Medicaid_Programs_Data_Tables.xlsx")
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
catch_file    <- file.path(base_dir, "Models", "US_Catchment_Supply.csv")
slopes_file   <- file.path(base_dir, "Models", "US_State_Slopes15_by_Medicaid.csv")
models_dir    <- file.path(base_dir, "Models")
fig_dir       <- file.path(base_dir, "Figures")

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

state_lookup <- tibble(
  state_name = c(state.name, "District of Columbia"),
  state_abbr = c(state.abb, "DC")
)

# =============================================================================
# STEP 1: PARSE THE THREE HPI TABS (defensive)
# =============================================================================
cat("================================================================\n")
cat("STEP 1: Parsing HPI data tables...\n")
cat("================================================================\n")

# --- 1a. Adult benefit level, 2025 ---
ben_raw <- read_excel(hpi_file, sheet = "Adult Dental Benefits",
                      skip = 5, col_names = FALSE, col_types = "text")
ben <- ben_raw %>%
  setNames(c("state_name", "y2021", "y2022", "y2023", "y2024", "y2025")[seq_len(ncol(ben_raw))]) %>%
  mutate(state_name = str_trim(state_name)) %>%
  inner_join(state_lookup, by = "state_name") %>%
  mutate(
    benefit25 = case_when(
      str_detect(tolower(coalesce(y2025, "")), "enhanced")  ~ "Enhanced",
      str_detect(tolower(coalesce(y2025, "")), "limited")   ~ "Limited",
      str_detect(tolower(coalesce(y2025, "")), "emergency") ~ "Emergency-or-None",
      TRUE                                                   ~ "Emergency-or-None"  # blank = no benefit
    )
  ) %>%
  select(state_abbr, benefit25)

stopifnot(nrow(ben) == 51)
cat("  Benefit level 2025 category counts:\n")
print(count(ben, benefit25))

# --- 1b. Participation, 2024 ---
par_raw <- read_excel(hpi_file, sheet = "Dentist Medicaid Participation",
                      skip = 5, col_names = FALSE, col_types = "text")
par <- par_raw %>%
  setNames(c("state_name", "y2015", "y2016", "y2017", "y2019", "y2020",
             "y2021", "y2022", "y2023", "y2024")[seq_len(ncol(par_raw))]) %>%
  mutate(state_name = str_trim(state_name)) %>%
  inner_join(state_lookup, by = "state_name") %>%
  transmute(state_abbr, particip24 = as.numeric(y2024))

stopifnot(nrow(par) == 51, all(!is.na(par$particip24)))
cat(sprintf("  Participation 2024: mean %.1f%% | range %.0f%%-%.0f%%\n",
            100 * mean(par$particip24),
            100 * min(par$particip24), 100 * max(par$particip24)))

# --- 1c. Adult FFS reimbursement as %% of private payment, 2024 ---
rmb_raw <- read_excel(hpi_file, sheet = "FFS Reimbursement Adult",
                      skip = 5, col_names = FALSE, col_types = "text")
rmb <- rmb_raw %>%
  setNames(c("state_name", "chg2022", "chg2024", "chg2025",
             "prv2022", "prv2024", "prv2025")[seq_len(ncol(rmb_raw))]) %>%
  mutate(state_name = str_trim(state_name)) %>%
  inner_join(state_lookup, by = "state_name") %>%
  transmute(state_abbr, reimb24 = as.numeric(prv2024))

stopifnot(nrow(rmb) == 51)
cat(sprintf("  Reimbursement (%% of private, 2024): %d states with data | mean %.0f%% | range %.0f%%-%.0f%%\n",
            sum(!is.na(rmb$reimb24)), 100 * mean(rmb$reimb24, na.rm = TRUE),
            100 * min(rmb$reimb24, na.rm = TRUE), 100 * max(rmb$reimb24, na.rm = TRUE)))
cat("  States missing reimbursement (no adult FFS benefit / managed care only):\n")
cat("   ", paste(rmb$state_abbr[is.na(rmb$reimb24)], collapse = ", "), "\n")

policy <- ben %>% left_join(par, by = "state_abbr") %>% left_join(rmb, by = "state_abbr")
write_csv(policy, file.path(models_dir, "US_State_Policy_Table.csv"))
cat("  Saved US_State_Policy_Table.csv\n")

# =============================================================================
# STEP 2: POLICY VARIABLE CORRELATION STRUCTURE
# =============================================================================
cat("\n================================================================\n")
cat("STEP 2: Correlations among policy variables...\n")
cat("================================================================\n")

pol_num <- policy %>%
  mutate(benefit_ord = case_when(benefit25 == "Emergency-or-None" ~ 1,
                                 benefit25 == "Limited" ~ 2,
                                 benefit25 == "Enhanced" ~ 3))
cors <- tibble(
  pair = c("benefit (ordinal) vs participation",
           "benefit (ordinal) vs reimbursement",
           "participation vs reimbursement"),
  spearman = c(
    cor(pol_num$benefit_ord, pol_num$particip24, method = "spearman", use = "complete.obs"),
    cor(pol_num$benefit_ord, pol_num$reimb24,   method = "spearman", use = "complete.obs"),
    cor(pol_num$particip24,  pol_num$reimb24,   method = "spearman", use = "complete.obs")
  )
)
print(cors)
write_csv(cors, file.path(models_dir, "US_Policy_Correlations.csv"))

# =============================================================================
# STEP 3: ANALYTIC DATASET
# =============================================================================
cat("\n================================================================\n")
cat("STEP 3: Assembling analytic dataset...\n")
cat("================================================================\n")

catch <- read_csv(catch_file, col_types = cols(zip5 = col_character()),
                  show_col_types = FALSE) %>% select(zip5, dent_15, pop_15)

d <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
              show_col_types = FALSE) %>%
  left_join(catch, by = "zip5") %>%
  left_join(policy, by = c("state" = "state_abbr")) %>%
  filter(!is.na(adi_natrank), !is.na(ruca_cat), !is.na(dent_15), pop_15 > 0) %>%
  mutate(
    adi10       = adi_natrank / 10,
    y           = as.integer(round(dent_15)),
    logoff      = log(pop_15),
    ruca_cat    = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                              "Small town", "Rural")),
    state       = factor(state),
    benefit25   = factor(benefit25, levels = c("Enhanced", "Limited",
                                               "Emergency-or-None")),
    particip10  = particip24 * 10,   # per 10 percentage points
    reimb10     = reimb24 * 10       # per 10 percentage points
  )
cat(sprintf("  Analytic ZCTAs: %s across %d states\n",
            format(nrow(d), big.mark = ","), n_distinct(d$state)))

# =============================================================================
# STEP 4: ONE MODEL PER POLICY VARIABLE (main effect + ADI interaction)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 4: Fitting policy models (three glmmTMB fits)...\n")
cat("================================================================\n")

all_results <- list()

# --- 4a. Benefit level 2025 (categorical) ---
mB <- glmmTMB(y ~ adi10 * benefit25 + ruca_cat + offset(logoff) + (1 | state),
              data = d, family = nbinom2)
tB <- tidy_fixed(mB) %>%
  filter(str_detect(term, "adi10|benefit25")) %>%
  mutate(policy_variable = "Benefit level 2025", .before = 1)
all_results$benefit <- tB
b <- fixef(mB)$cond; Vc <- vcov(mB)$cond
cat("  Benefit level 2025:\n")
for (cat_lab in c("Limited", "Emergency-or-None")) {
  me <- paste0("benefit25", cat_lab)
  it <- paste0("adi10:benefit25", cat_lab)
  est <- b[["adi10"]] + b[[it]]
  se  <- sqrt(Vc["adi10","adi10"] + Vc[it,it] + 2 * Vc["adi10",it])
  cat(sprintf("    %-18s supply level RR vs Enhanced: %.3f | ADI gradient: %.4f (%.4f-%.4f) | interaction p = %.3g\n",
              cat_lab, exp(b[[me]]), exp(est), exp(est - 1.96*se), exp(est + 1.96*se),
              2 * pnorm(-abs(b[[it]] / sqrt(Vc[it,it])))))
}
cat(sprintf("    Enhanced (reference)  ADI gradient: %.4f\n", exp(b[["adi10"]])))

# --- 4b. Participation 2024 (continuous, per 10 pp) ---
mP <- glmmTMB(y ~ adi10 * particip10 + ruca_cat + offset(logoff) + (1 | state),
              data = d, family = nbinom2)
tP <- tidy_fixed(mP) %>%
  filter(str_detect(term, "adi10|particip10")) %>%
  mutate(policy_variable = "Participation 2024", .before = 1)
all_results$particip <- tP
cat("\n  Participation 2024 (per 10 pp): main-effect and interaction rows above saved;\n")
print(tP %>% select(term, estimate, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))))

# --- 4c. Reimbursement 2024 (continuous, per 10 pp; reduced state set) ---
dR <- d %>% filter(!is.na(reimb10))
cat(sprintf("\n  Reimbursement model: %s ZCTAs across %d states with reimbursement data\n",
            format(nrow(dR), big.mark = ","), n_distinct(dR$state)))
mR <- glmmTMB(y ~ adi10 * reimb10 + ruca_cat + offset(logoff) + (1 | state),
              data = dR, family = nbinom2)
tR <- tidy_fixed(mR) %>%
  filter(str_detect(term, "adi10|reimb10")) %>%
  mutate(policy_variable = "Reimbursement 2024 (% of private)", .before = 1)
all_results$reimb <- tR
print(tR %>% select(term, estimate, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))))

write_csv(bind_rows(all_results), file.path(models_dir, "US_Policy_Models.csv"))
cat("\n  Saved US_Policy_Models.csv\n")

# =============================================================================
# STEP 5: FIGURE S5 — STATE ADI SLOPES vs PARTICIPATION, BY BENEFIT LEVEL
#   (RR15 slopes are policy-agnostic random-slope estimates from Script 6's
#    Model B; only that column is reused — the old KFF grouping is ignored)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 5: Figure S5...\n")
cat("================================================================\n")

slopes <- read_csv(slopes_file, show_col_types = FALSE) %>%
  select(state, RR15) %>%
  left_join(policy, by = c("state" = "state_abbr")) %>%
  mutate(benefit25 = factor(benefit25, levels = c("Enhanced", "Limited",
                                                  "Emergency-or-None")))

figS5 <- ggplot(slopes, aes(x = 100 * particip24, y = RR15, color = benefit25)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE,
              color = "grey30", linewidth = 0.6, alpha = 0.12) +
  geom_point(size = 2.6, alpha = 0.9) +
  geom_text(aes(label = state), size = 2.4, vjust = -0.9, check_overlap = TRUE,
            show.legend = FALSE) +
  scale_color_manual(values = c("Enhanced" = "#009E73", "Limited" = "#E69F00",
                                "Emergency-or-None" = "#D55E00")) +
  labs(
    x = "Dentists enrolled as Medicaid/CHIP providers, 2024 (%)",
    y = "State-specific ADI gradient, 15-mile supply (RR per 10 percentiles)",
    color = "Adult dental benefit, 2025",
    title = "Exploratory: state policy environment and disadvantage gradients",
    subtitle = "Each point is a state (random-slope estimates at the 15-mile scale). Associational only:\n51 state-level units; policy variables are intercorrelated and reflect broader state context."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9.5, color = "grey30"),
        legend.position = "bottom")

ggsave(file.path(fig_dir, "FigS5_State_Slopes_by_Participation.png"), figS5,
       width = 7.2, height = 5.8, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "FigS5_State_Slopes_by_Participation.pdf"), figS5,
       width = 7.2, height = 5.8, bg = "white")
cat("  Saved FigS5_State_Slopes_by_Participation\n")

cat("\n================================================================\n")
cat(" SCRIPT 9 COMPLETE - state policy environment section\n")
cat(" (Supersedes Script 6; KFF classification no longer used)\n")
cat("================================================================\n")
cat("\nDone.\n")
