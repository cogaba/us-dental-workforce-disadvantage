# =============================================================================
# SCRIPT 14 v2 (NATIONAL): Figure 4 — State-Level Correlates (two panels)
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Replaces the three-panel Figure 4 from Script 13. Rationale: presenting
# raw inverse slopes for participation and reimbursement, then explaining
# in the text that they vanish after rurality adjustment, buries the
# take-home message. This figure shows only the two associations that hold:
#
#   Panel A: adult Medicaid dental benefit level vs dentist supply
#            (rho = 0.52 unadjusted; 0.56 rurality-adjusted, p < 0.001 -
#             the association strengthens with adjustment, so the raw
#             comparison is the conservative presentation)
#   Panel B: state rural/small-town population share vs percentage of
#            communities with no dentist within 15 miles
#            (rho = 0.37, p = 0.007) - the only state characteristic
#             associated with the geography of shortage
#
# Null findings (participation, reimbursement; both outcomes; adjusted and
# unadjusted) are reported in the manuscript table, not the figure.
#
# Input:  Models/US_State_Correlates_Table.csv  (written by Script 13 v2)
# Output: Figures/Fig4_State_Correlates_v3.(png|pdf)
#
# Runtime: seconds.
#
# v2: Panel B axis labels simplified for readability -
#     x = "Rural population share (%)" (rural + small-town RUCA population,
#         specified in the caption)
#     y = "Dental deserts (% of ZCTAs)" (no dentist within 15 miles;
#         definition in the caption, matching Figure 2's construction)
# =============================================================================

library(readr)
library(dplyr)
library(ggplot2)
if (!requireNamespace("patchwork", quietly = TRUE))
  stop("Please run install.packages('patchwork') first.")
library(patchwork)

# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
state_file <- file.path(base_dir, "Models", "US_State_Correlates_Table.csv")
fig_dir    <- file.path(base_dir, "Figures")

COL_MAIN <- "#0072B2"
COL_RUR  <- "#C44E52"

st <- read_csv(state_file, show_col_types = FALSE) %>%
  mutate(benefit25 = factor(benefit25,
                            levels = c("Enhanced", "Limited", "Emergency-or-None")))
stopifnot(nrow(st) == 51)

theme_panel <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position  = "none")

# --- Panel A: benefit generosity and how many dentists a state has ---
panelA <- ggplot(st, aes(x = benefit25, y = dentists_per_10k)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, color = "grey35",
               linewidth = 0.4) +
  geom_jitter(width = 0.13, size = 2.1, alpha = 0.75, color = COL_MAIN) +
  scale_y_continuous(limits = c(4.8, 12.6)) +
  labs(x = "Adult Medicaid dental benefit, 2025",
       y = "Dentists per 10,000 residents") +
  theme_panel +
  theme(axis.text.x = element_text(size = 8.5))

# --- Panel B: rural composition and where dentists are missing ---
panelB <- ggplot(st, aes(x = rural_pop_share, y = pct_deserts)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey35",
              linewidth = 0.6, alpha = 0.12) +
  geom_point(size = 2.1, alpha = 0.8, color = COL_RUR) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Rural population share (%)",
       y = "Dental deserts (% of ZCTAs)") +
  theme_panel

fig4 <- panelA + panelB +
  plot_layout(widths = c(1, 1.15)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(fig_dir, "Fig4_State_Correlates_v3.png"), fig4,
       width = 9.5, height = 4.1, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Fig4_State_Correlates_v3.pdf"), fig4,
       width = 9.5, height = 4.1, bg = "white")

cat("Saved Fig4_State_Correlates_v3\n")
cat(sprintf("  Panel A: median dentists per 10k by benefit level: %s\n",
            paste(sprintf("%s %.2f", levels(st$benefit25),
                          tapply(st$dentists_per_10k, st$benefit25, median)),
                  collapse = " | ")))
cat(sprintf("  Panel B: rural share vs %% deserts, Spearman rho = %.3f (p = %.4f)\n",
            cor(st$rural_pop_share, st$pct_deserts, method = "spearman"),
            cor.test(st$rural_pop_share, st$pct_deserts,
                     method = "spearman")$p.value))
cat("Done.\n")
