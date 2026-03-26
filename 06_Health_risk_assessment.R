# ============================================
# HEALTH RISK ASSESSMENT - Full Calculation for All Sites
# Dry and Wet Seasons
# Based on US EPA methodology (Section 2.5)
# ============================================

library(dplyr)

# ============================================
# METAL CONCENTRATIONS (From Table 1 and your data)
# ============================================

# Dry Season Concentrations (mg/L)
dry_concentrations <- data.frame(
  Site = paste0("S", 1:15),
  As = c(0.011, 0.013, 0.012, 0.012, 0.010, 0.014, 0.013, 0.015, 0.014, 0.014, 0.012, 0.011, 0.013, 0.011, 0.010),
  Ni = c(0.019, 0.021, 0.020, 0.018, 0.017, 0.023, 0.022, 0.024, 0.021, 0.023, 0.020, 0.019, 0.021, 0.018, 0.017),
  Cr = c(0.028, 0.032, 0.029, 0.031, 0.027, 0.035, 0.033, 0.037, 0.034, 0.036, 0.030, 0.029, 0.031, 0.028, 0.026),
  Cd = c(0.0023, 0.0027, 0.0025, 0.0026, 0.0022, 0.0030, 0.0028, 0.0032, 0.0029, 0.0031, 0.0025, 0.0024, 0.0026, 0.0023, 0.0021),
  Cu = c(0.023, 0.027, 0.025, 0.026, 0.024, 0.030, 0.028, 0.032, 0.029, 0.031, 0.026, 0.025, 0.027, 0.024, 0.023),
  Zn = c(0.085, 0.095, 0.092, 0.088, 0.083, 0.102, 0.098, 0.108, 0.100, 0.105, 0.090, 0.087, 0.093, 0.086, 0.082),
  Pb = c(0.048, 0.052, 0.049, 0.051, 0.047, 0.056, 0.054, 0.058, 0.055, 0.057, 0.050, 0.049, 0.052, 0.048, 0.046),
  Hg = c(0.00022, 0.00028, 0.00025, 0.00026, 0.00023, 0.00032, 0.00030, 0.00035, 0.00031, 0.00033, 0.00027, 0.00024, 0.00029, 0.00023, 0.00021)
)

# Wet Season Concentrations (mg/L)
wet_concentrations <- data.frame(
  Site = paste0("S", 1:15),
  As = c(0.007, 0.009, 0.008, 0.008, 0.007, 0.010, 0.009, 0.011, 0.009, 0.010, 0.008, 0.007, 0.009, 0.008, 0.006),
  Ni = c(0.015, 0.017, 0.016, 0.015, 0.014, 0.018, 0.017, 0.019, 0.017, 0.018, 0.016, 0.015, 0.017, 0.015, 0.013),
  Cr = c(0.020, 0.024, 0.021, 0.023, 0.019, 0.026, 0.025, 0.028, 0.024, 0.027, 0.022, 0.020, 0.023, 0.021, 0.018),
  Cd = c(0.0016, 0.0020, 0.0018, 0.0019, 0.0015, 0.0023, 0.0021, 0.0025, 0.0020, 0.0024, 0.0019, 0.0017, 0.0020, 0.0018, 0.0014),
  Cu = c(0.018, 0.022, 0.020, 0.021, 0.019, 0.024, 0.023, 0.026, 0.022, 0.025, 0.021, 0.019, 0.022, 0.020, 0.018),
  Zn = c(0.065, 0.075, 0.070, 0.072, 0.066, 0.080, 0.078, 0.085, 0.076, 0.082, 0.073, 0.068, 0.075, 0.069, 0.063),
  Pb = c(0.038, 0.042, 0.040, 0.041, 0.037, 0.045, 0.044, 0.048, 0.043, 0.047, 0.041, 0.038, 0.042, 0.039, 0.036),
  Hg = c(0.00015, 0.00020, 0.00018, 0.00019, 0.00016, 0.00023, 0.00022, 0.00026, 0.00021, 0.00025, 0.00019, 0.00017, 0.00021, 0.00018, 0.00014)
)

# ============================================
# US EPA HEALTH RISK PARAMETERS
# ============================================

# Exposure parameters
IRW_child <- 0.78   # L/day
IRW_adult <- 2.5    # L/day
BW_child <- 15      # kg
BW_adult <- 70      # kg
EF <- 350           # days/year
ED_child <- 6       # years
ED_adult <- 20      # years
AT_child <- 2190    # days (6 × 365)
AT_adult <- 9490    # days (26 × 365)
AT_cancer <- 25550  # days (70 × 365)

# Reference Doses (RfD) - mg/kg/day
RfD_As <- 3e-4
RfD_Ni <- 2e-2
RfD_Cr <- 3e-3
RfD_Cd <- 5e-4
RfD_Cu <- 4e-2
RfD_Zn <- 3e-1
RfD_Pb <- 1.4e-3
RfD_Hg <- 3e-4

# Cancer Slope Factors (CSF) - (mg/kg/day)^-1
CSF_As <- 1.5
CSF_Ni <- 0.91
CSF_Cr <- 0.5
CSF_Cd <- 6.1

# ============================================
# FUNCTION TO CALCULATE HEALTH RISKS
# ============================================

calculate_health_risks <- function(conc_data) {
  
  results <- conc_data
  
  # Calculate Hazard Quotients (HQ) for Children
  results$HQ_As_child <- (conc_data$As * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_As)
  results$HQ_Ni_child <- (conc_data$Ni * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Ni)
  results$HQ_Cr_child <- (conc_data$Cr * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Cr)
  results$HQ_Cd_child <- (conc_data$Cd * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Cd)
  results$HQ_Cu_child <- (conc_data$Cu * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Cu)
  results$HQ_Zn_child <- (conc_data$Zn * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Zn)
  results$HQ_Pb_child <- (conc_data$Pb * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Pb)
  results$HQ_Hg_child <- (conc_data$Hg * IRW_child * EF * ED_child) / (BW_child * AT_child * RfD_Hg)
  
  # Hazard Quotients (HQ) for Adults
  results$HQ_As_adult <- (conc_data$As * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_As)
  results$HQ_Ni_adult <- (conc_data$Ni * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Ni)
  results$HQ_Cr_adult <- (conc_data$Cr * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Cr)
  results$HQ_Cd_adult <- (conc_data$Cd * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Cd)
  results$HQ_Cu_adult <- (conc_data$Cu * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Cu)
  results$HQ_Zn_adult <- (conc_data$Zn * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Zn)
  results$HQ_Pb_adult <- (conc_data$Pb * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Pb)
  results$HQ_Hg_adult <- (conc_data$Hg * IRW_adult * EF * ED_adult) / (BW_adult * AT_adult * RfD_Hg)
  
  # Hazard Index (HI)
  results$HI_child <- results$HQ_As_child + results$HQ_Ni_child + results$HQ_Cr_child + 
                      results$HQ_Cd_child + results$HQ_Cu_child + results$HQ_Zn_child + 
                      results$HQ_Pb_child + results$HQ_Hg_child
  
  results$HI_adult <- results$HQ_As_adult + results$HQ_Ni_adult + results$HQ_Cr_adult + 
                      results$HQ_Cd_adult + results$HQ_Cu_adult + results$HQ_Zn_adult + 
                      results$HQ_Pb_adult + results$HQ_Hg_adult
  
  # Carcinogenic Risk (CR) - Children
  results$CR_As_child <- (conc_data$As * IRW_child * CSF_As * EF * ED_child) / (BW_child * AT_cancer)
  results$CR_Ni_child <- (conc_data$Ni * IRW_child * CSF_Ni * EF * ED_child) / (BW_child * AT_cancer)
  results$CR_Cr_child <- (conc_data$Cr * IRW_child * CSF_Cr * EF * ED_child) / (BW_child * AT_cancer)
  results$CR_Cd_child <- (conc_data$Cd * IRW_child * CSF_Cd * EF * ED_child) / (BW_child * AT_cancer)
  
  # Carcinogenic Risk (CR) - Adults
  results$CR_As_adult <- (conc_data$As * IRW_adult * CSF_As * EF * ED_adult) / (BW_adult * AT_cancer)
  results$CR_Ni_adult <- (conc_data$Ni * IRW_adult * CSF_Ni * EF * ED_adult) / (BW_adult * AT_cancer)
  results$CR_Cr_adult <- (conc_data$Cr * IRW_adult * CSF_Cr * EF * ED_adult) / (BW_adult * AT_cancer)
  results$CR_Cd_adult <- (conc_data$Cd * IRW_adult * CSF_Cd * EF * ED_adult) / (BW_adult * AT_cancer)
  
  # Total Carcinogenic Risk (TCR)
  results$TCR_child <- results$CR_As_child + results$CR_Ni_child + results$CR_Cr_child + results$CR_Cd_child
  results$TCR_adult <- results$CR_As_adult + results$CR_Ni_adult + results$CR_Cr_adult + results$CR_Cd_adult
  
  return(results)
}

# ============================================
# CALCULATE FOR DRY AND WET SEASONS
# ============================================

dry_results <- calculate_health_risks(dry_concentrations)
wet_results <- calculate_health_risks(wet_concentrations)

# ============================================
# CREATE TABLE S8: DRY SEASON HEALTH RISK ASSESSMENT
# ============================================

dry_table <- dry_results %>%
  select(Site, 
         CR_As_child, CR_Ni_child, CR_Cr_child, CR_Cd_child, TCR_child,
         CR_As_adult, CR_Ni_adult, CR_Cr_adult, CR_Cd_adult, TCR_adult,
         HI_child, HI_adult)

cat("\n===========================================\n")
cat("TABLE S8: DRY SEASON HEALTH RISK ASSESSMENT\n")
cat("===========================================\n")
print(dry_table)

# ============================================
# CREATE TABLE S9: WET SEASON HEALTH RISK ASSESSMENT
# ============================================

wet_table <- wet_results %>%
  select(Site, 
         CR_As_child, CR_Ni_child, CR_Cr_child, CR_Cd_child, TCR_child,
         CR_As_adult, CR_Ni_adult, CR_Cr_adult, CR_Cd_adult, TCR_adult,
         HI_child, HI_adult)

cat("\n===========================================\n")
cat("TABLE S9: WET SEASON HEALTH RISK ASSESSMENT\n")
cat("===========================================\n")
print(wet_table)

# ============================================
# SUMMARY STATISTICS
# ============================================

cat("\n===========================================\n")
cat("SEASONAL COMPARISON SUMMARY\n")
cat("===========================================\n")

cat("\nDry Season:\n")
cat("  HI > 1 (Children):", sum(dry_table$HI_child > 1), "sites\n")
cat("  TCR > 1e-4 (Children):", sum(dry_table$TCR_child > 1e-4), "sites\n")
cat("  Average HI (Children):", round(mean(dry_table$HI_child), 3), "\n")
cat("  Average TCR (Children):", format(mean(dry_table$TCR_child), scientific = TRUE), "\n")

cat("\nWet Season:\n")
cat("  HI > 1 (Children):", sum(wet_table$HI_child > 1), "sites\n")
cat("  TCR > 1e-4 (Children):", sum(wet_table$TCR_child > 1e-4), "sites\n")
cat("  Average HI (Children):", round(mean(wet_table$HI_child), 3), "\n")
cat("  Average TCR (Children):", format(mean(wet_table$TCR_child), scientific = TRUE), "\n")
