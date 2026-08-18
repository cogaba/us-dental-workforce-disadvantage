# =============================================================================
# SCRIPT 5 (NATIONAL): Catchment-Based Supply Measures + Primary Models
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Rationale:
#   Within-ZCTA provider density is fragile at fine geography: dental practices
#   concentrate in commercial cores whose *residential* populations skew
#   renter/carless/crowded, producing spurious positive "deprivation" gradients
#   under SDI/SVI. The primary access measure is therefore CATCHMENT supply:
#   dentists and population within R miles of each ZCTA's internal-point
#   centroid (R = 15 primary; 10 and 25 sensitivity), so that
#   catchment density = 10,000 x (dentists within R) / (population within R).
#
#   Supply universe = ALL U.S. ZCTAs (including those excluded from the
#   analytic sample): their dentists and residents still count as nearby
#   supply and demand. Analytic units remain the 26,285 sample ZCTAs.
#
# Analyses:
#   A. Catchment construction (dbscan::frNN fixed-radius search on
#      unit-sphere 3D coordinates; haversine-equivalent distances)
#   B. Scale x index gradient table: NB mixed models (glmmTMB) of supply
#      count with log(catchment population) offset, exposure = ADI / SDI /
#      SVI overall per 10 percentiles, adjusted for RUCA + state random
#      intercept, at each supply scale (within-ZCTA, 10, 15, 25 mi)
#   C. Catchment deserts (zero dentists within 15 miles): prevalence by
#      disadvantage quartile per index + mixed logistic models
#
# Note: overlapping catchments induce spatial correlation between nearby
# ZCTAs; state random effects absorb part of this, and residual spatial
# autocorrelation is acknowledged as a limitation (Moran's I check possible
# with centroid coordinates if a reviewer requests it).
#
# Inputs:
#   2025_Gaz_zcta_national.txt            (Census Gazetteer, PIPE-delimited)
#   Merge/US_Dentist_SDI_Merged_Full.csv  (supply universe, Script 2)
#   Merge/US_Analytic_With_ADI.csv        (analytic sample, Script 2b)
#   SVI_2022_US_ZCTA.csv
#
# Outputs (Models/ directory):
#   US_Catchment_Supply.csv               — per-ZCTA catchment measures
#   US_Catchment_Gradient_Table.csv       — the scale x index RR table
#   US_Catchment_Desert_by_Quartile.csv   — desert prevalence descriptives
#   US_Catchment_Desert_Models.csv        — mixed logistic ORs
#
# Runtime: catchment construction ~2-5 min; 12 glmmTMB fits ~20-40 min.
# Requires: install.packages("dbscan") if not present.
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(glmmTMB)
library(dbscan)

# -----------------------------------------------------------------------------
# 0. PATHS + HELPER
# -----------------------------------------------------------------------------
# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"

gaz_file      <- file.path(base_dir, "2025_Gaz_zcta_national.txt")
universe_file <- file.path(base_dir, "Merge", "US_Dentist_SDI_Merged_Full.csv")
analytic_file <- file.path(base_dir, "Merge", "US_Analytic_With_ADI.csv")
svi_file      <- file.path(base_dir, "SVI_2022_US_ZCTA.csv")
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

R_EARTH_MI <- 3958.8

# =============================================================================
# STEP 1: LOAD GAZETTEER CENTROIDS (2025 file is PIPE-delimited)
# =============================================================================
cat("================================================================\n")
cat("STEP 1: Loading Gazetteer ZCTA centroids...\n")
cat("================================================================\n")

gaz <- read_delim(gaz_file, delim = "|", trim_ws = TRUE,
                  col_types = cols(.default = col_character())) %>%
  transmute(
    zcta = str_pad(str_trim(GEOID), 5, "left", "0"),
    lat  = as.numeric(INTPTLAT),
    lon  = as.numeric(INTPTLONG)
  )
cat(sprintf("  Gazetteer ZCTAs: %s (missing coords: %d)\n",
            format(nrow(gaz), big.mark = ","), sum(is.na(gaz$lat) | is.na(gaz$lon))))

# =============================================================================
# STEP 2: SUPPLY UNIVERSE (all U.S. ZCTAs with coordinates)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 2: Building supply universe...\n")
cat("================================================================\n")

uni <- read_csv(universe_file, col_types = cols(zip5 = col_character()),
                show_col_types = FALSE) %>%
  select(zip5, population, n_dentists_total) %>%
  inner_join(gaz, by = c("zip5" = "zcta")) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(population))

cat(sprintf("  Supply universe: %s ZCTAs | %s dentists | %.1fM residents\n",
            format(nrow(uni), big.mark = ","),
            format(sum(uni$n_dentists_total), big.mark = ","),
            sum(uni$population) / 1e6))

# =============================================================================
# STEP 3: CATCHMENT CONSTRUCTION (fixed-radius neighbor search)
#   Unit-sphere 3D coordinates; radius converted to chord length so
#   Euclidean search = great-circle search. frNN excludes the point
#   itself, so each ZCTA's own dentists/population are added back.
# =============================================================================
cat("\n================================================================\n")
cat("STEP 3: Computing catchments (10 / 15 / 25 miles)...\n")
cat("================================================================\n")

lat_r <- uni$lat * pi / 180
lon_r <- uni$lon * pi / 180
xyz <- cbind(cos(lat_r) * cos(lon_r),
             cos(lat_r) * sin(lon_r),
             sin(lat_r))

dent <- uni$n_dentists_total
pop  <- uni$population

catch <- tibble(zip5 = uni$zip5)

for (R in c(10, 15, 25)) {
  eps_chord <- 2 * sin(R / (2 * R_EARTH_MI))
  nn <- frNN(xyz, eps = eps_chord)
  dsum <- vapply(seq_along(nn$id),
                 function(i) dent[i] + sum(dent[nn$id[[i]]]), numeric(1))
  psum <- vapply(seq_along(nn$id),
                 function(i) pop[i]  + sum(pop[nn$id[[i]]]),  numeric(1))
  catch[[paste0("dent_", R)]] <- dsum
  catch[[paste0("pop_",  R)]] <- psum
  catch[[paste0("dens_", R)]] <- 10000 * dsum / psum
  cat(sprintf("  R = %2d mi: median nearby dentists %s | ZCTAs with zero nearby: %s\n",
              R, format(median(dsum), big.mark = ","),
              format(sum(dsum == 0), big.mark = ",")))
  rm(nn); gc()
}

write_csv(catch, file.path(output_dir, "US_Catchment_Supply.csv"))
cat("  Saved US_Catchment_Supply.csv\n")

# =============================================================================
# STEP 4: ANALYTIC DATASET (sample ZCTAs + three indices + catchments)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 4: Assembling analytic dataset...\n")
cat("================================================================\n")

svi_df <- read_csv(svi_file, col_types = cols(.default = col_character()),
                   show_col_types = FALSE) %>%
  transmute(
    zip5        = str_pad(str_trim(FIPS), 5, "left", "0"),
    svi_overall = as.numeric(RPL_THEMES)
  ) %>%
  mutate(svi_overall = if_else(svi_overall < 0, NA_real_, svi_overall)) %>%
  distinct(zip5, .keep_all = TRUE)

df <- read_csv(analytic_file, col_types = cols(zip5 = col_character()),
               show_col_types = FALSE) %>%
  left_join(svi_df, by = "zip5") %>%
  left_join(catch,  by = "zip5") %>%
  mutate(
    adi10    = adi_natrank / 10,
    sdi10    = sdi_score  / 10,
    svi10    = svi_overall * 10,
    ruca_cat = factor(ruca_cat, levels = c("Metropolitan", "Micropolitan",
                                           "Small town", "Rural")),
    state    = factor(state)
  )
cat(sprintf("  Analytic ZCTAs with catchments: %s\n",
            format(sum(!is.na(df$dens_15)), big.mark = ",")))

# =============================================================================
# STEP 5: SCALE x INDEX GRADIENT TABLE (the paper's central result)
#   NB mixed model per (scale, index): supply count ~ exposure + RUCA +
#   (1 | state) + offset(log catchment population)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 5: Fitting scale x index models (12 fits; the long step)...\n")
cat("================================================================\n")

scales <- tribble(
  ~scale_label,   ~count_var,          ~pop_var,
  "Within-ZCTA",  "n_dentists_total",  "population",
  "10-mile",      "dent_10",           "pop_10",
  "15-mile",      "dent_15",           "pop_15",
  "25-mile",      "dent_25",           "pop_25"
)
indices <- c(ADI = "adi10", SDI = "sdi10", SVI = "svi10")

grad_rows <- list()

for (i in seq_len(nrow(scales))) {
  sl <- scales$scale_label[i]
  cv <- scales$count_var[i]
  pv <- scales$pop_var[i]

  d <- df %>%
    filter(!is.na(adi10), !is.na(sdi10), !is.na(svi10),
           !is.na(ruca_cat), !is.na(.data[[cv]]), .data[[pv]] > 0) %>%
    mutate(
      y      = as.integer(round(.data[[cv]])),
      logoff = log(.data[[pv]])
    )

  for (ix in names(indices)) {
    v <- indices[[ix]]
    f <- as.formula(paste0("y ~ ", v, " + ruca_cat + offset(logoff) + (1 | state)"))
    m <- glmmTMB(f, data = d, family = nbinom2)
    tt <- tidy_fixed(m) %>% filter(term == v)
    grad_rows[[paste(sl, ix)]] <- tibble(
      supply_scale = sl, index = ix,
      RR_per10 = tt$estimate, conf.low = tt$conf.low,
      conf.high = tt$conf.high, p.value = tt$p.value,
      n_zctas = nrow(d)
    )
    cat(sprintf("  %-11s | %-3s : RR %.4f (%.4f-%.4f)\n",
                sl, ix, tt$estimate, tt$conf.low, tt$conf.high))
  }
}

grad_table <- bind_rows(grad_rows)
write_csv(grad_table, file.path(output_dir, "US_Catchment_Gradient_Table.csv"))
cat("  Saved US_Catchment_Gradient_Table.csv\n")

# =============================================================================
# STEP 6: CATCHMENT DESERTS (15-mile primary)
# =============================================================================
cat("\n================================================================\n")
cat("STEP 6: Catchment desert analyses...\n")
cat("================================================================\n")

dd <- df %>%
  filter(!is.na(adi10), !is.na(sdi10), !is.na(svi10), !is.na(ruca_cat),
         !is.na(dent_15)) %>%
  mutate(desert15 = dent_15 == 0)

cat(sprintf("  15-mile deserts: %s ZCTAs (%.1f%%), population %.2fM\n",
            format(sum(dd$desert15), big.mark = ","),
            100 * mean(dd$desert15),
            sum(dd$population[dd$desert15]) / 1e6))

raw_index_vars <- c(ADI = "adi_natrank", SDI = "sdi_score", SVI = "svi_overall")

desert_desc <- purrr::map_dfr(names(raw_index_vars), function(ix) {
  v <- raw_index_vars[[ix]]
  dd %>%
    mutate(q = cut(.data[[v]],
                   breaks = quantile(.data[[v]], probs = c(0, .25, .5, .75, 1)),
                   include.lowest = TRUE,
                   labels = c("Q1 (least)", "Q2", "Q3", "Q4 (most)"))) %>%
    group_by(q) %>%
    summarise(n_zctas = n(),
              pct_desert15 = round(100 * mean(desert15), 1),
              pop_in_deserts = sum(population[desert15]),
              .groups = "drop") %>%
    mutate(index = ix, .before = 1)
})
print(desert_desc, n = Inf)
write_csv(desert_desc, file.path(output_dir, "US_Catchment_Desert_by_Quartile.csv"))

desert_models <- purrr::map_dfr(names(indices), function(ix) {
  v <- indices[[ix]]
  f <- as.formula(paste0("desert15 ~ ", v, " + ruca_cat + (1 | state)"))
  m <- glmmTMB(f, data = dd, family = binomial)
  tidy_fixed(m) %>% filter(term == v) %>%
    transmute(index = ix, OR_per10 = estimate, conf.low, conf.high, p.value)
})
print(desert_models)
write_csv(desert_models, file.path(output_dir, "US_Catchment_Desert_Models.csv"))

cat("\n================================================================\n")
cat(" SCRIPT 5 COMPLETE\n")
cat(" Next: Script 4 revision - figures for the catchment-primary framing\n")
cat("================================================================\n")
cat("\nDone.\n")
