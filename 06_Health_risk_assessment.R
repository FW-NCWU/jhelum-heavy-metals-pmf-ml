# ============================================
# HEALTH RISK ASSESSMENT - Dry and Wet Seasons
# ============================================

# ============================================
# FUNCTION TO CALCULATE HEALTH RISKS
# ============================================

calculate_health_risks <- function(As_conc, Ni_conc, Cr_conc, Cd_conc,
                                   Cu_conc, Zn_conc, Pb_conc, Hg_conc,
                                   season_name) {
  
  # US EPA parameters
  IRW_child <- 0.78
  IRW_adult <- 2.5
  BW_child <- 15
  BW_adult <- 70
  EF <- 350
  ED_child <- 6
  ED_adult <- 20
  AT_child <- 2190
  AT_adult <- 9490
  AT_cancer <- 25550
  
  # RfD values (mg/kg/day)
  RfD_As <- 3e-4
  RfD_Ni <- 2e-2
  RfD_Cr <- 3e-3
  RfD_Cd <- 5e-4
  RfD_Cu <- 4e-2
  RfD_Zn <- 3e-1
  RfD_Pb <- 1.4e-3
  RfD_Hg <- 3e-4
  
  # CSF values (mg/kg/day)^-1
  CSF_As <- 1.5
  CSF_Ni <- 0.91
  CSF_Cr <- 0.5
  CSF_Cd <- 6.1
  
  # Calculate Hazard Quotients (HQ)
  HQ_As_child <- (As_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_As)
  HQ_Ni_child <- (Ni_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Ni)
  HQ_Cr_child <- (Cr_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Cr)
  HQ_Cd_child <- (Cd_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Cd)
  HQ_Cu_child <- (Cu_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Cu)
  HQ_Zn_child <- (Zn_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Zn)
  HQ_Pb_child <- (Pb_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Pb)
  HQ_Hg_child <- (Hg_conc * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Hg)
  
  HQ_As_adult <- (As_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_As)
  HQ_Ni_adult <- (Ni_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Ni)
  HQ_Cr_adult <- (Cr_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Cr)
  HQ_Cd_adult <- (Cd_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Cd)
  HQ_Cu_adult <- (Cu_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Cu)
  HQ_Zn_adult <- (Zn_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Zn)
  HQ_Pb_adult <- (Pb_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Pb)
  HQ_Hg_adult <- (Hg_conc * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Hg)
  
  # Calculate Hazard Index (HI)
  HI_child <- HQ_As_child + HQ_Ni_child + HQ_Cr_child + HQ_Cd_child +
              HQ_Cu_child + HQ_Zn_child + HQ_Pb_child + HQ_Hg_child
  
  HI_adult <- HQ_As_adult + HQ_Ni_adult + HQ_Cr_adult + HQ_Cd_adult +
              HQ_Cu_adult + HQ_Zn_adult + HQ_Pb_adult + HQ_Hg_adult
  
  # Calculate Carcinogenic Risk (CR)
  CR_As_child <- (As_conc * IRW_child * CSF_As * EF * ED_child) / (BW_child * AT_cancer)
  CR_Ni_child <- (Ni_conc * IRW_child * CSF_Ni * EF * ED_child) / (BW_child * AT_cancer)
  CR_Cr_child <- (Cr_conc * IRW_child * CSF_Cr * EF * ED_child) / (BW_child * AT_cancer)
  CR_Cd_child <- (Cd_conc * IRW_child * CSF_Cd * EF * ED_child) / (BW_child * AT_cancer)
  
  CR_As_adult <- (As_conc * IRW_adult * CSF_As * EF * ED_adult) / (BW_adult * AT_cancer)
  CR_Ni_adult <- (Ni_conc * IRW_adult * CSF_Ni * EF * ED_adult) / (BW_adult * AT_cancer)
  CR_Cr_adult <- (Cr_conc * IRW_adult * CSF_Cr * EF * ED_adult) / (BW_adult * AT_cancer)
  CR_Cd_adult <- (Cd_conc * IRW_adult * CSF_Cd * EF * ED_adult) / (BW_adult * AT_cancer)
  
  TCR_child <- CR_As_child + CR_Ni_child + CR_Cr_child + CR_Cd_child
  TCR_adult <- CR_As_adult + CR_Ni_adult + CR_Cr_adult + CR_Cd_adult
  
  return(list(HI_Child = HI_child, HI_Adult = HI_adult,
              TCR_Child = TCR_child, TCR_Adult = TCR_adult))
}

# ============================================
# EXAMPLE USAGE WITH YOUR DATA
# ============================================

# Dry season concentrations (site S1 example)
results <- calculate_health_risks(
  As_conc = 0.011, Ni_conc = 0.019, Cr_conc = 0.028, Cd_conc = 0.0023,
  Cu_conc = 0.023, Zn_conc = 0.085, Pb_conc = 0.048, Hg_conc = 0.00022,
  season_name = "Dry"
)

cat("\n=== HEALTH RISK RESULTS (Dry Season - S1) ===\n")
cat("HI (Children):", round(results$HI_Child, 3), "\n")
cat("HI (Adults):", round(results$HI_Adult, 3), "\n")
cat("TCR (Children):", format(results$TCR_Child, scientific = TRUE), "\n")
cat("TCR (Adults):", format(results$TCR_Adult, scientific = TRUE), "\n")
