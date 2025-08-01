# compute summary statistics: mean, median, SD, min, max, and count (n) for both biased (original) and unbiased (predicted) BD
# Author:Mehrmanzar Sohrab
# E-mail:sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load Packages ----
library(dplyr)

# 2. Set working directory ----
setwd("H:/Analysis/Finalized RF and Uncertainty_Gabor")

# 3. Load Data ----

data <- read.csv("SIMS_BD_Predictions_with_Uncertainty.csv")

# 4. Summary Statistics ----
summary_table <- bind_rows(
  data %>%
    summarise(
      Variable = "BD",
      Mean     = mean(BD, na.rm = TRUE),
      Median   = median(BD, na.rm = TRUE),
      SD       = sd(BD, na.rm = TRUE),
      Min      = min(BD, na.rm = TRUE),
      Max      = max(BD, na.rm = TRUE),
      N        = sum(!is.na(BD))
    ),
  data %>%
    summarise(
      Variable = "BD_pred",
      Mean     = mean(BD.pred, na.rm = TRUE),
      Median   = median(BD.pred, na.rm = TRUE),
      SD       = sd(BD.pred, na.rm = TRUE),
      Min      = min(BD.pred, na.rm = TRUE),
      Max      = max(BD.pred, na.rm = TRUE),
      N        = sum(!is.na(BD.pred))
    )
)

# 4.1. View Result ----
print(summary_table)

# 4.2. save to csv file ----

write.csv(summary_table, "BD_comparison_summary.csv", row.names = FALSE)

