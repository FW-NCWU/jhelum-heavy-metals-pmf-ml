# ============================================
# MULTIPLE LINEAR REGRESSION - Zn Prediction
# ============================================

library(ggplot2)

# ============================================
# DATA
# ============================================

# Dry season data
dry_data <- data.frame(
  Zn = c(0.085, 0.095, 0.092, 0.088, 0.083, 0.102, 0.098, 0.108, 0.100, 0.105, 0.090, 0.087, 0.093, 0.086, 0.082),
  TH = c(130, 140, 150, 170, 190, 320, 330, 350, 280, 310, 250, 220, 200, 180, 120),
  EC = c(480, 520, 550, 620, 680, 1100, 1150, 1200, 950, 1050, 850, 780, 720, 650, 450)
)

# ============================================
# MLR MODEL
# ============================================

mlr_model <- lm(Zn ~ TH + EC, data = dry_data)

# Model summary
cat("\n=== MLR MODEL SUMMARY ===\n")
print(summary(mlr_model))

# ============================================
# PREDICTIONS AND PLOT
# ============================================

dry_data$Predicted <- predict(mlr_model, dry_data)

ggplot(dry_data, aes(x = Predicted, y = Zn)) +
  geom_point(size = 3, color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
  labs(title = "MLR Model: Predicted vs Actual Zn Concentration",
       x = "Predicted Zn (mg/L)", y = "Actual Zn (mg/L)") +
  annotate("text", x = 0.07, y = 0.11, 
           label = paste("R² =", round(summary(mlr_model)$r.squared, 3)),
           size = 5, fontface = "bold") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"))

ggsave("MLR_Model_Validation.tiff", width = 8, height = 6, dpi = 600)

# ============================================
# EQUATION
# ============================================

coef <- coef(mlr_model)
cat("\n=== MLR EQUATION ===\n")
cat(sprintf("Zn (mg/l) = %.3f + %.3f × TH + %.3f × EC\n", 
            coef[1], coef[2], coef[3]))
