# =============================================================================
# SCRIPT 15 (NATIONAL): STROBE Sample-Selection Flow Diagram (Figure S1)
# Study: National Dental Workforce & Neighborhood Social Disadvantage
#
# Draws the flow diagram from the pipeline diagnostics files so every count
# in the figure traces to a saved output rather than being typed by hand.
# Two parallel chains are shown side by side:
#
#   ZCTA chain      33,772 -> 26,285   (territories, population, SDI)
#   Provider chain  270,136 -> 264,345 (ZIP match, then ZCTA exclusions)
#
# Both chains are verified to reconcile before plotting; the script stops
# if any arithmetic fails, so a diagram can never be produced from
# inconsistent numbers.
#
# Inputs:  Merge/US_Merge_Diagnostics.csv
#          Merge/US_ADI_Construction_Diagnostics.csv
# Output:  Figures/FigS1_STROBE_Flow.(png|pdf)
#
# Runtime: seconds. Uses base R graphics only (no new packages).
# =============================================================================
# Set base_dir to your project folder before running
base_dir   <- r"(C:\path\to\National Dental Workforce Paper)"
diag_file <- file.path(base_dir, "Merge", "US_Merge_Diagnostics.csv")
adi_file  <- file.path(base_dir, "Merge", "US_ADI_Construction_Diagnostics.csv")
fig_dir   <- file.path(base_dir, "Figures")

dg  <- read.csv(diag_file, stringsAsFactors = FALSE)
adi <- read.csv(adi_file,  stringsAsFactors = FALSE)
gv  <- function(pattern) as.numeric(dg$value[grepl(pattern, dg$step, fixed = TRUE)][1])
av  <- function(nm)      as.numeric(adi$value[adi$step == nm][1])

# ---- pull counts ----
z_acs    <- gv("2.0  Population")
z_terr   <- gv("2.1  Population")
z_frame  <- gv("2.2  Population")
z_pop0   <- gv("7.0  ZCTA")
z_pop500 <- gv("7.1  ZCTA")
z_sdi    <- gv("7.2  ZCTA")
z_final  <- gv("8.0  Final")

d_tot    <- gv("1.0  NPI")
d_nozcta <- gv("3.0  NPI")
d_inc    <- gv("3.1  NPI")
d_pop0   <- gv("7.3  Dentists")
d_pop500 <- gv("7.4  Dentists")
d_sdi    <- gv("7.5  Dentists")
d_final  <- gv("8.2  Final")

pop_final  <- gv("8.1  Final")
n_states   <- gv("8.4  Final")
adi_cover  <- av("analytic_with_adi")
adi_bg     <- av("adi_bg_total")
adi_supp   <- av("adi_bg_suppressed")

# ---- verify arithmetic (halt if inconsistent) ----
stopifnot(z_acs - z_terr == z_frame,
          z_frame - z_pop0 - z_pop500 - z_sdi == z_final,
          d_tot - d_nozcta == d_inc,
          d_inc - d_pop0 - d_pop500 - d_sdi == d_final)
cat("Arithmetic verified: ZCTA and provider chains both reconcile.\n")

fmt <- function(x) formatC(x, format = "d", big.mark = ",")

# ---- drawing helpers ----
box <- function(x, y, w, h, lines, cex = 0.72, fill = "white") {
  rect(x - w/2, y - h/2, x + w/2, y + h/2, col = fill, border = "grey30", lwd = 1.1)
  text(x, y, paste(lines, collapse = "\n"), cex = cex)
}
arrow_down <- function(x, y0, y1) arrows(x, y0, x, y1, length = 0.07, lwd = 1.1, col = "grey30")
arrow_side <- function(x0, x1, y) arrows(x0, y, x1, y, length = 0.07, lwd = 1.1, col = "grey30")

for (dev_type in c("png", "pdf")) {
  if (dev_type == "png") {
    png(file.path(fig_dir, "FigS1_STROBE_Flow.png"), width = 2400, height = 3000, res = 300)
  } else {
    pdf(file.path(fig_dir, "FigS1_STROBE_Flow.pdf"), width = 8, height = 10)
  }
  par(mar = c(0.4, 0.4, 0.4, 0.4))
  plot(NA, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")

  # Three columns: ZCTA main chain (LX), ZCTA exclusion notes (MX), provider
  # chain (RX). MX is new -- previously the exclusion notes shared RX with the
  # provider chain, so the provider chain's long connecting arrow ran straight
  # down through the exclusion boxes (they were drawn on top of it, giving the
  # appearance of the arrow crossing through them). Separating the columns
  # removes that overlap.
  LX <- 17; BW <- 30    # ZCTA main chain
  MX <- 50; MW <- 27    # ZCTA exclusion notes (middle column)
  RX <- 82; EW <- 28    # Provider chain

  # --- headers ---
  text((LX + MX) / 2, 98, "Geographic units (ZCTAs)", font = 2, cex = 0.8)
  text(RX, 98, "Dental providers", font = 2, cex = 0.8)

  # --- ZCTA chain ---
  box(LX, 92, BW, 7, c("ZCTAs in ACS 2020-2024 file",
                       paste0("(table B01003): n = ", fmt(z_acs))), cex = 0.62)
  arrow_down(LX, 88.5, 83.5)
  box(LX, 80, BW, 7, c("U.S. ZCTAs (50 states + DC)",
                       paste0("n = ", fmt(z_frame))), cex = 0.62)
  box(MX, 80, MW, 6.5, c(paste0("Excluded: territory ZCTAs (PR/VI), n = ", fmt(z_terr))), cex = 0.58,
      fill = "grey96")
  arrow_side(LX + BW/2, MX - MW/2, 80)

  arrow_down(LX, 76.5, 71.5)
  box(LX, 68, BW, 7, c("ZCTAs with population >= 500",
                       paste0("n = ", fmt(z_frame - z_pop0 - z_pop500))), cex = 0.62)
  box(MX, 68, MW, 8, c(paste0("Excluded: population = 0, n = ", fmt(z_pop0)),
                       paste0("Excluded: population < 500, n = ", fmt(z_pop500))), cex = 0.58,
      fill = "grey96")
  arrow_side(LX + BW/2, MX - MW/2, 68)

  arrow_down(LX, 64.5, 59.5)
  box(LX, 56, BW, 7, c("ZCTAs with valid SDI score",
                       paste0("n = ", fmt(z_final))), cex = 0.62)
  box(MX, 56, MW, 6.5, c(paste0("Excluded: no valid SDI score, n = ", fmt(z_sdi))), cex = 0.58,
      fill = "grey96")
  arrow_side(LX + BW/2, MX - MW/2, 56)

  # --- provider chain: own column (RX), clear of the exclusion notes above ---
  box(RX, 92, EW + 4, 7, c("Active dentists extracted from NPPES",
                           paste0("(July 2026): n = ", fmt(d_tot))), cex = 0.6)
  arrow_down(RX, 88.5, 47.5)
  box(RX, 44, EW + 4, 8, c("Providers with practice ZIP matching",
                           "a Census ZCTA",
                           paste0("n = ", fmt(d_inc))), cex = 0.6)
  box(RX, 33, EW + 4, 7, c(paste0("Excluded: ZIP not a Census ZCTA, n = ", fmt(d_nozcta)),
                           paste0("Excluded within removed ZCTAs, n = ",
                                  fmt(d_pop0 + d_pop500 + d_sdi))), cex = 0.58, fill = "grey96")
  arrow_down(RX, 40, 36.5)

  # --- convergence: analytic sample ---
  arrow_down(LX, 52.5, 26)
  arrow_side(RX - (EW + 4)/2, LX + BW/2 + 1, 44)
  box(50, 21, 72, 12,
      c("ANALYTIC SAMPLE",
        paste0(fmt(z_final), " ZCTAs in ", fmt(n_states), " states and DC"),
        paste0(fmt(pop_final), " residents"),
        paste0(fmt(d_final), " dentists")), cex = 0.76)

  # --- ADI construction note ---
  arrow_down(50, 15, 11.5)
  box(50, 8, 78, 8,
      c(paste0("Area Deprivation Index linked to ", fmt(adi_cover), " ZCTAs (99.4%)"),
        paste0("from ", fmt(adi_bg), " census block groups (", fmt(adi_supp),
               " suppressed) via GeoCorr 2022 crosswalk")), cex = 0.66, fill = "grey98")

  dev.off()
}

cat("Saved FigS1_STROBE_Flow (.png 300 dpi + .pdf)\n")
cat(sprintf("  ZCTAs: %s -> %s | Providers: %s -> %s\n",
            fmt(z_acs), fmt(z_final), fmt(d_tot), fmt(d_final)))
cat("Done.\n")
