# Testing existing pedotransfer functions (PTFs) on the MARTHA dataset  
# Author: Mehrmanzar Sohrab  
# E-mail: sohrab.mehrmanzar@atk.hun-ren.hu  

# 1. Load necessary libraries ----
library(dplyr)
library(ggplot2)
library(tidyr)

# 2. Set working directory ----
setwd("H:/Analysis")

# 3. Load the MARTHA dataset ----
martha <- readRDS("MARTHA_with_EnvCov.rds")

# 4. Data preparation ----
martha <- martha %>%
  select(-CaCO3) %>%  # Drop CaCO3 column
  drop_na(SOC, Average_depth, Sand, Clay, BD)  # Remove rows with missing values

# 5. Apply existing PTFs ----

# 5.1. Option 1: Alexander (1980) and Hossain et al. (2015) ----
martha <- martha %>%
  mutate(
    BD_predicted = ifelse(
      SOC < 12,
      1.72 - 0.294 * sqrt(SOC),
      0.074 + 2.632 * exp(-0.076 * SOC)
    )
  )

# 5.1.1. Plot: Observed vs Predicted BD ====
ggplot(martha, aes(x = BD, y = BD_predicted)) +
  geom_point(alpha = 0.7, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Observed vs Predicted BD (Option 1)",
    x = "Observed BD (g/cm³)",
    y = "Predicted BD (g/cm³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# 5.1.2. Model performance (Option 1) ====
ME <- mean(martha$BD - martha$BD_predicted, na.rm = TRUE)
MAE <- mean(abs(martha$BD - martha$BD_predicted), na.rm = TRUE)
RMSE <- sqrt(mean((martha$BD - martha$BD_predicted)^2, na.rm = TRUE))
ss_res <- sum((martha$BD - martha$BD_predicted)^2, na.rm = TRUE)
ss_tot <- sum((martha$BD - mean(martha$BD, na.rm = TRUE))^2, na.rm = TRUE)
NSE <- 1 - (ss_res / ss_tot)

cat("\nOption 1 - PTF Performance:\n")
cat("ME  :", ME, "\n")
cat("MAE :", MAE, "\n")
cat("RMSE:", RMSE, "\n")
cat("NSE :", NSE, "\n")

## 5.2. Option 2: Hollis et al. (2012) ----
martha <- martha %>%
  mutate(
    BD_Hollis = ifelse(
      SOC > 12,
      1.4903 + 0.33293 * log(SOC),
      ifelse(
        Average_depth <= 30,
        0.80806 + (0.823844 * exp(-0.27993 * SOC)) + 0.0014065 * Sand - 0.0010299 * Clay,
        0.69794 + (0.750636 * exp(-0.230355 * SOC)) + 0.0008687 * Sand - 0.0005164 * Clay
      )
    )
  )

# 5.2.1. Plot: Observed vs Predicted (Hollis) ====
ggplot(martha, aes(x = BD, y = BD_Hollis)) +
  geom_point(alpha = 0.7, color = "#2ca02c") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Observed vs Predicted BD (Hollis et al., 2012)",
    x = "Observed BD (g/cm³)",
    y = "Predicted BD (g/cm³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# 5.2.2. Model performance (Hollis)
ME_Hollis <- mean(martha$BD - martha$BD_Hollis, na.rm = TRUE)
MAE_Hollis <- mean(abs(martha$BD - martha$BD_Hollis), na.rm = TRUE)
RMSE_Hollis <- sqrt(mean((martha$BD - martha$BD_Hollis)^2, na.rm = TRUE))
ss_res_Hollis <- sum((martha$BD - martha$BD_Hollis)^2, na.rm = TRUE)
ss_tot_Hollis <- sum((martha$BD - mean(martha$BD, na.rm = TRUE))^2, na.rm = TRUE)
NSE_Hollis <- 1 - (ss_res_Hollis / ss_tot_Hollis)

cat("\nHollis PTF Performance:\n")
cat("ME  :", ME_Hollis, "\n")
cat("MAE :", MAE_Hollis, "\n")
cat("RMSE:", RMSE_Hollis, "\n")
cat("NSE :", NSE_Hollis, "\n")