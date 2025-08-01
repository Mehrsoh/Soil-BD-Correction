# 2D Hexbin Plot of Predicted vs. Observed Bulk Density (BD) with Pearson Correlation and 1:1 Line
# E-mail: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load Packages ----
library(ggplot2)     # for plotting
library(ggpubr)      # for stat_cor (Pearson correlation)
library(viridis)     # for color scale 
library(hexbin)      # for geom_hex

# 2. Set Working Directory ----
setwd("H:/Analysis/Finalized RF and Uncertainty_Gabor")

# 3. Load Data ----
data <- read.csv("SIMS_BD_Predictions_with_Uncertainty.csv")

# 4. Set Manual Axis Limits and Breaks ----
axis_min <- 0
axis_max <- 2
axis_breaks <- seq(0, 2, by = 0.5)

# 5. Hexbin Plot ----
p <- ggplot(data, aes(x = BD, y = BD.pred)) +
  geom_hex(bins = 70) +
  scale_fill_viridis_c(option = "D", name = "Count") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred", linewidth = 1) +
  stat_cor(method = "pearson", aes(label = after_stat(r.label)), 
           label.x = 0.1, label.y = 1.9, size = 5) +
  labs(
    x = "Biased Bulk Density",
    y = "Unbiased Bulk Density"
  ) +
  scale_x_continuous(limits = c(axis_min, axis_max), breaks = axis_breaks) +
  scale_y_continuous(limits = c(axis_min, axis_max), breaks = axis_breaks) +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 16, margin = margin(t = 12)),
    axis.title.y = element_text(size = 16, margin = margin(r = 12)),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# 5.1. Show Plot ====
print(p)

# 5.2. Save Plot ====
ggsave("Hexbin_BD_unbiased_vs_biased.jpeg", plot = p, width = 7, height = 6, dpi = 300)
