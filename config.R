# ---------------------------------------------------------------------------
# config.R - single place to set the project directory
# Every script sources this file, so paths need changing in one place only.
# ---------------------------------------------------------------------------

# Set this to your project folder (the folder containing the downloaded data files)
base_dir <- "~/dental-workforce"          # <-- EDIT THIS

# Subfolders (created if absent)
merge_dir  <- file.path(base_dir, "Merge")
models_dir <- file.path(base_dir, "Models")
fig_dir    <- file.path(base_dir, "Figures")
for (d in c(merge_dir, models_dir, fig_dir))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Raw input files (edit filenames if yours differ)
npi_raw        <- file.path(base_dir, "npidata_pfile_20050523-20260712.csv")
acs_pop_file   <- file.path(base_dir, "ACSDT5Y2024_B01003-Data.csv")
ruca_file      <- file.path(base_dir, "RUCA-codes-2020-zipcode.xlsx")
gazetteer_file <- file.path(base_dir, "2025_Gaz_zcta_national.txt")
sdi_file       <- file.path(base_dir, "asset_rgc_sdi_2015_through_2019_zcta.csv")
svi_file       <- file.path(base_dir, "SVI_2022_US_ZCTA.csv")
adi_bg_file    <- file.path(base_dir, "US_2024_block_group_adi_v_4_0_1.csv")
geocorr_file   <- file.path(base_dir, "geocorr2022_national_bg_to_zcta.csv")
hpi_file       <- file.path(base_dir, "Dental_Care_in_Medicaid_Programs_Data_Tables.xlsx")
