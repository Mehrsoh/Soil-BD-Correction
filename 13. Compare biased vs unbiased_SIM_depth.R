# compute summary statistics by depth intervals: mean, median, SD, min, max, and count (n)
# for both biased (original) and unbiased (predicted) BD
# Author: Mehrmanzar Sohrab
# E-mail: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load Packages ----
library(dplyr)
library(tidyr)

# 2. Set Working Directory ----
setwd("H:/Analysis/Finalized RF and Uncertainty_Gabor")

# 3. Load Data ----
data <- read.csv("SIMS_BD_Predictions_with_Uncertainty.csv")

# 4. Create Depth Groups ----
data <- data %>%
  mutate(
    depth_group = case_when(
      Average_depth >= 0   & Average_depth < 30  ~ "0-30",
      Average_depth >= 30  & Average_depth < 60  ~ "30-60",
      Average_depth >= 60  & Average_depth < 100 ~ "60-100",
      Average_depth >= 100 & Average_depth <= 200 ~ "100-200",
      TRUE ~ NA_character_
    )
  )

# 5. Compute Summary Statistics by Depth ----
summary_by_depth <- data %>%
  filter(!is.na(depth_group)) %>%
  group_by(depth_group) %>%
  summarise(
    Mean_BD       = mean(BD, na.rm = TRUE),
    Median_BD     = median(BD, na.rm = TRUE),
    SD_BD         = sd(BD, na.rm = TRUE),
    Min_BD        = min(BD, na.rm = TRUE),
    Max_BD        = max(BD, na.rm = TRUE),
    N_BD          = sum(!is.na(BD)),
    
    Mean_BD_pred   = mean(BD.pred, na.rm = TRUE),
    Median_BD_pred = median(BD.pred, na.rm = TRUE),
    SD_BD_pred     = sd(BD.pred, na.rm = TRUE),
    Min_BD_pred    = min(BD.pred, na.rm = TRUE),
    Max_BD_pred    = max(BD.pred, na.rm = TRUE),
    N_BD_pred      = sum(!is.na(BD.pred))
  )

# 5.1. View Result ----
print(summary_by_depth)

# 5.2. Save to CSV File ----
write.csv(summary_by_depth, "BD_comparison_by_depth.csv", row.names = FALSE)
