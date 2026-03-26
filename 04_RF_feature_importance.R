# ============================================
# RANDOM FOREST FEATURE IMPORTANCE
# ============================================

library(randomForest)
library(ggplot2)
library(dplyr)

# ============================================
# DATA (Combined with PMF factors)
# ============================================

# Note: This is a template. Replace with your actual PMF factor contributions.
set.seed(123)

sample_data <- data.frame(
  SampleID = paste0("S", 1:100),
  Factor1_PointSources = runif(100, 0.01, 0.1),
  Factor2_Agricultural = runif(100, 0.2, 0.5),
  Factor3_Geogenic = runif(100, 0.1, 0.3),
  Factor4_UrbanIndustrial = runif(100, 0.3, 0.6),
  EC = runif(100, 200, 800),
  TH = runif(100, 100, 400),
  TDS = runif(100, 150, 600),
  pH = runif(100, 6.5, 8.5),
  Season = sample(c("Dry", "Wet"), 100, replace = TRUE)
)

# Normalize to sum to 1
sample_data <- sample_data %>%
  mutate(Total = Factor1_PointSources + Factor2_Agricultural + 
           Factor3_Geogenic + Factor4_UrbanIndustrial,
         across(Factor1_PointSources:Factor4_UrbanIndustrial, ~ ./Total)) %>%
  select(-Total)

sample_data$Season <- as.factor(sample_data$Season)

# ============================================
# RANDOM FOREST MODELS
# ============================================

rf_model1 <- randomForest(Factor1_PointSources ~ EC + TH + TDS + pH + Season,
                          data = sample_data, ntree = 500, importance = TRUE)

rf_model2 <- randomForest(Factor2_Agricultural ~ EC + TH + TDS + pH + Season,
                          data = sample_data, ntree = 500, importance = TRUE)

rf_model3 <- randomForest(Factor3_Geogenic ~ EC + TH + TDS + pH + Season,
                          data = sample_data, ntree = 500, importance = TRUE)

rf_model4 <- randomForest(Factor4_UrbanIndustrial ~ EC + TH + TDS + pH + Season,
                          data = sample_data, ntree = 500, importance = TRUE)

# ============================================
# FEATURE IMPORTANCE PLOT FUNCTION
# ============================================

create_importance_plot <- function(model, source_name) {
  
  imp_data <- importance(model)
  imp_df <- data.frame(
    Feature = rownames(imp_data),
    Importance = imp_data[, "%IncMSE"]
  ) %>% arrange(desc(Importance))
  
  imp_df$Feature <- recode(imp_df$Feature,
                           "EC" = "Electrical Conductivity",
                           "TH" = "Total Hardness",
                           "TDS" = "Total Dissolved Solids",
                           "pH" = "pH",
                           "Season" = "Season")
  
  ggplot(imp_df, aes(x = Importance, y = reorder(Feature, Importance))) +
    geom_col(fill = "#2c7bb6", alpha = 0.8, width = 0.7) +
    labs(title = source_name, x = "Importance (% Increase in MSE)", y = NULL) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          axis.text.y = element_text(size = 11, face = "bold", color = "black"),
          axis.text.x = element_text(size = 10, color = "black"),
          axis.title.x = element_text(size = 11, face = "bold", color = "black"),
          axis.line = element_line(color = "black", linewidth = 0.8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"))
}

# ============================================
# CREATE PLOTS
# ============================================

p1 <- create_importance_plot(rf_model1, "Factor 1: Point Sources")
p2 <- create_importance_plot(rf_model2, "Factor 2: Agricultural")
p3 <- create_importance_plot(rf_model3, "Factor 3: Geogenic")
p4 <- create_importance_plot(rf_model4, "Factor 4: Urban/Industrial")

# ============================================
# COMBINE PLOTS
# ============================================

library(patchwork)

combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Key Predictors of PMF Factors Identified by Machine Learning",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

print(combined_plot)

ggsave("RF_Feature_Importance.tiff", combined_plot, width = 14, height = 12, dpi = 600)
