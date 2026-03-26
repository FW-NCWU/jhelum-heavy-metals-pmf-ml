
# ============================================
# PCA ANALYSIS - Dry and Wet Seasons
# ============================================

library(ggplot2)
library(dplyr)

# ============================================
# DRY SEASON DATA (8 metals + 4 physicochemical)
# ============================================

dry_data <- data.frame(
  Cr = c(0.028, 0.032, 0.029, 0.031, 0.027, 0.035, 0.033, 0.037, 0.034, 0.036, 0.030, 0.029, 0.031, 0.028, 0.026),
  Ni = c(0.019, 0.021, 0.020, 0.018, 0.017, 0.023, 0.022, 0.024, 0.021, 0.023, 0.020, 0.019, 0.021, 0.018, 0.017),
  Cu = c(0.023, 0.027, 0.025, 0.026, 0.024, 0.030, 0.028, 0.032, 0.029, 0.031, 0.026, 0.025, 0.027, 0.024, 0.023),
  Zn = c(0.085, 0.095, 0.092, 0.088, 0.083, 0.102, 0.098, 0.108, 0.100, 0.105, 0.090, 0.087, 0.093, 0.086, 0.082),
  Pb = c(0.048, 0.052, 0.049, 0.051, 0.047, 0.056, 0.054, 0.058, 0.055, 0.057, 0.050, 0.049, 0.052, 0.048, 0.046),
  Hg = c(0.00022, 0.00028, 0.00025, 0.00026, 0.00023, 0.00032, 0.00030, 0.00035, 0.00031, 0.00033, 0.00027, 0.00024, 0.00029, 0.00023, 0.00021),
  As = c(0.011, 0.013, 0.012, 0.012, 0.010, 0.014, 0.013, 0.015, 0.014, 0.014, 0.012, 0.011, 0.013, 0.011, 0.010),
  Cd = c(0.0023, 0.0027, 0.0025, 0.0026, 0.0022, 0.0030, 0.0028, 0.0032, 0.0029, 0.0031, 0.0025, 0.0024, 0.0026, 0.0023, 0.0021),
  pH = c(7.3, 7.4, 7.4, 7.5, 7.5, 8.2, 8.3, 8.5, 8.0, 8.1, 7.8, 7.7, 7.6, 7.5, 7.2),
  EC = c(480, 520, 550, 620, 680, 1100, 1150, 1200, 950, 1050, 850, 780, 720, 650, 450),
  TDS = c(280, 300, 320, 360, 400, 680, 710, 750, 590, 650, 520, 480, 440, 390, 260),
  TH = c(130, 140, 150, 170, 190, 320, 330, 350, 280, 310, 250, 220, 200, 180, 120)
)

dry_site <- paste0("S", 1:15)

# ============================================
# WET SEASON DATA (8 metals + 4 physicochemical)
# ============================================

wet_data <- data.frame(
  Cr = c(0.020, 0.024, 0.021, 0.023, 0.019, 0.026, 0.025, 0.028, 0.024, 0.027, 0.022, 0.020, 0.023, 0.021, 0.018),
  Ni = c(0.015, 0.017, 0.016, 0.015, 0.014, 0.018, 0.017, 0.019, 0.017, 0.018, 0.016, 0.015, 0.017, 0.015, 0.013),
  Cu = c(0.018, 0.022, 0.020, 0.021, 0.019, 0.024, 0.023, 0.026, 0.022, 0.025, 0.021, 0.019, 0.022, 0.020, 0.018),
  Zn = c(0.065, 0.075, 0.070, 0.072, 0.066, 0.080, 0.078, 0.085, 0.076, 0.082, 0.073, 0.068, 0.075, 0.069, 0.063),
  Pb = c(0.038, 0.042, 0.040, 0.041, 0.037, 0.045, 0.044, 0.048, 0.043, 0.047, 0.041, 0.038, 0.042, 0.039, 0.036),
  Hg = c(0.00015, 0.00020, 0.00018, 0.00019, 0.00016, 0.00023, 0.00022, 0.00026, 0.00021, 0.00025, 0.00019, 0.00017, 0.00021, 0.00018, 0.00014),
  As = c(0.007, 0.009, 0.008, 0.008, 0.007, 0.010, 0.009, 0.011, 0.009, 0.010, 0.008, 0.007, 0.009, 0.008, 0.006),
  Cd = c(0.0016, 0.0020, 0.0018, 0.0019, 0.0015, 0.0023, 0.0021, 0.0025, 0.0020, 0.0024, 0.0019, 0.0017, 0.0020, 0.0018, 0.0014),
  pH = c(7.0, 7.1, 7.1, 7.2, 7.2, 7.9, 8.0, 8.1, 7.7, 7.8, 7.5, 7.4, 7.3, 7.2, 6.9),
  EC = c(400, 430, 450, 500, 540, 900, 940, 950, 780, 850, 700, 650, 590, 540, 380),
  TDS = c(230, 250, 260, 290, 310, 550, 570, 580, 480, 520, 430, 400, 360, 330, 220),
  TH = c(100, 110, 115, 130, 140, 260, 270, 280, 220, 245, 195, 180, 165, 150, 90)
)

wet_site <- paste0("S", 1:15)

# ============================================
# PCA ANALYSIS
# ============================================

# Dry Season PCA
dry_pca <- prcomp(dry_data, scale. = TRUE, center = TRUE)
dry_var <- round((dry_pca$sdev^2) / sum(dry_pca$sdev^2) * 100, 1)

# Wet Season PCA
wet_pca <- prcomp(wet_data, scale. = TRUE, center = TRUE)
wet_var <- round((wet_pca$sdev^2) / sum(wet_pca$sdev^2) * 100, 1)

# ============================================
# PCA BIPLOT FUNCTION
# ============================================

create_pca_biplot <- function(data, sites, season_name, var_exp) {
  
  pca <- prcomp(data, scale. = TRUE, center = TRUE)
  
  scores <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], Site = sites)
  
  loadings <- data.frame(
    PC1 = pca$rotation[,1],
    PC2 = pca$rotation[,2],
    Variable = rownames(pca$rotation)
  )
  
  scale_f <- max(abs(scores[,1:2])) / max(abs(loadings[,1:2])) * 0.8
  loadings$PC1s <- loadings$PC1 * scale_f
  loadings$PC2s <- loadings$PC2 * scale_f
  
  ggplot() +
    geom_point(data = scores, aes(x = PC1, y = PC2), color = "black", size = 4, alpha = 0.7) +
    geom_text(data = scores, aes(x = PC1, y = PC2, label = Site), 
              size = 4.5, fontface = "bold", vjust = -1, color = "black") +
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1s, yend = PC2s),
                 arrow = arrow(length = unit(0.25, "cm")), color = "darkred", linewidth = 1) +
    geom_text(data = loadings, aes(x = PC1s * 1.1, y = PC2s * 1.1, label = Variable),
              color = "darkred", size = 4.8, fontface = "bold") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.6) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.6) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 12, face = "bold"),
          axis.title = element_text(color = "black", size = 14, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    labs(title = paste(season_name, "- PCA Biplot"),
         x = paste0("PC1 (", var_exp[1], "%)"),
         y = paste0("PC2 (", var_exp[2], "%)")) +
    coord_fixed(ratio = 1.2)
}

# ============================================
# CREATE AND SAVE BIPLOTS
# ============================================

dry_biplot <- create_pca_biplot(dry_data, dry_site, "Dry Season", dry_var)
wet_biplot <- create_pca_biplot(wet_data, wet_site, "Wet Season", wet_var)

print(dry_biplot)
print(wet_biplot)

ggsave("Dry_Season_PCA_Biplot.tiff", dry_biplot, width = 11, height = 9, dpi = 600)
ggsave("Wet_Season_PCA_Biplot.tiff", wet_biplot, width = 11, height = 9, dpi = 600)

# ============================================
# PRINT LOADINGS TABLES
# ============================================

dry_loadings <- data.frame(
  Variable = rownames(dry_pca$rotation),
  PC1 = round(dry_pca$rotation[,1], 3),
  PC2 = round(dry_pca$rotation[,2], 3)
)

wet_loadings <- data.frame(
  Variable = rownames(wet_pca$rotation),
  PC1 = round(wet_pca$rotation[,1], 3),
  PC2 = round(wet_pca$rotation[,2], 3)
)

cat("\n=== DRY SEASON PCA LOADINGS ===\n")
print(dry_loadings[order(abs(dry_loadings$PC1), decreasing = TRUE),])

cat("\n=== WET SEASON PCA LOADINGS ===\n")
print(wet_loadings[order(abs(wet_loadings$PC1), decreasing = TRUE),])
