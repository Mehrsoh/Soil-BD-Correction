# Scatter Plot of Predicted vs. Observed Bulk Density (BD) includes Pearson correlation statistics.
# E-mail: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load Packages ----
library(ggplot2)
library(ggpubr)

# 2. Set working directory ----
setwd("H:/Analysis/Finalized RF and Uncertainty_Gabor")

# 3. Load Data ----
data <- read.csv("SIMS_BD_Predictions_with_Uncertainty.csv")

# 4. Scatter Plot with Pearson Correlation and Large Axis Labels ----
p <- ggplot(data, aes(x = BD, y = BD.pred)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred", linewidth = 1) +
  labs(
    title = "Scatter Plot of Unbiased vs. Biased Bulk Density",
    x = "Biased Bulk Density",
    y = "Unbiased Bulk Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  stat_cor(method = "pearson", aes(label = after_stat(r.label)), label.x = 1, label.y = 1.85, size = 5)

# 4.1. Show Plot ====
print(p)

# 4.2. Save Plot ====
ggsave("Scatter_BD_unbiased_vs_biased_correlation_pearson.jpeg", plot = p, width = 7, height = 6, dpi = 300)