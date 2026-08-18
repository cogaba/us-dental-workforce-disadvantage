# Script manifest

Scripts run in the order listed. Each writes outputs to disk that later scripts read.

## Scripts producing results reported in the manuscript

| Order | Script | Produces |
|---|---|---|
| 1 | `Script1_National_US_Dentist_Extraction.R` | Dentist file from NPPES (14 dental taxonomy codes); counts for Supplementary Table S1 |
| 2 | `Script2_National_FourWay_Merge.R` | Analytic sample (26,285 ZCTAs); exclusion counts used in Figure S1 |
| 3 | `Script2b_National_ADI_Construction.R` | ZCTA-level ADI via GeoCorr block-group crosswalk (26,123 with valid ADI) |
| 4 | `Script3_National_Models.R` | Within-ZCTA models: Table 3 (within-ZCTA row), state-specific gradients (35 of 51), index concordance (Supplementary Table S4) |
| 5 | `Script5_Catchment_Models.R` | Catchment supply measures; Table 3 (10/15/25-mile rows); dental deserts (1,178 ZCTAs; OR 1.38) |
| 6 | `Script8_Specialist_Catchment.R` | Specialist and general-dentist models; Table 2; inputs for Figure 3 |
| 7 | `Script10_UrbanRural_Descriptives.R` | Urban–rural results at 15 miles (14-fold desert difference; 70.4%; 24.3 million) |
| 8 | `Script12_SVI_Theme_Decomposition.R` | SVI theme models; Table 4; Supplementary Table S5 |
| 9 | `Script13_v2_State_Correlates.R` | State-level correlations; Supplementary Table S6 |
| 10 | `Script7_ADI_Main_Figures.R` | Figure 1; Figure 2 |
| 11 | `Script11_v5_Figure3_Harmonized.R` | Figure 3 |
| 12 | `Script14_v2_Figure4_TwoPanel.R` | Figure 4 |
| 13 | `Script15_STROBE_Flow_fixed.R` | Figure S1 (STROBE sample selection) |

## Scripts documented for transparency (analyses not retained in the final manuscript)

| Script | What it did | Why it is not in the results |
|---|---|---|
| `Script4_National_Figures.R` | Index-comparison and state-gradient figures (scale convergence, caterpillar plots, quadrant scatter) | Figures were consolidated; final manuscript reports these results in Table 3 and text |
| `Script6_Medicaid_Moderation.R` | Medicaid benefit generosity as a moderator of the disadvantage gradient, using the KFF 2022 adult dental benefit classification | Superseded by the ADA HPI December 2025 classification and by a simpler correlation design (`Script13_v2`) |
| `Script9_State_Policy_Section.R` | Cross-level interaction models for three ADA HPI policy measures | Replaced by the state-level correlation analysis reported in the manuscript (`Script13_v2`) |

## Analyses estimated but not reported

- Disadvantage × rurality interaction models (`Script3_National_Models.R`, Model M3). Estimates are
  saved to `US_M3_Stratum_RRs.csv` but are not reported in the manuscript.
