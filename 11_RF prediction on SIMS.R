# Predicting Bulk Density on SIMS Dataset Using Final RF Model  
# Author: Mehrmanzar Sohrab
# Email: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load libraries ----
library(dplyr)
library(ranger)

# 2. Set working directory ----
setwd("H:/Analysis")

# 3. Load SIMS dataset ----
sims <- readRDS("SIMS_with_EnvCov.rds")

# 4. Clean dataset: remove CaCO3 and rows with NAs ----
sims_clean <- sims %>%
  select(-CaCO3) %>%
  na.omit()

# 5. Load trained RF model ----
RF.model <- readRDS("H:/Analysis/Final RF Model/Final_RF_FullModel.rds")

# 6. Predict BD (mean prediction) ----
sims_clean$BD.pred <- predict(RF.model, newdata = sims_clean)

# 7. Predict 5th and 95th percentiles (uncertainty) ----
sims_clean$q05 <- as.vector(predict(RF.model$finalModel, sims_clean, type="quantiles", quantiles=0.05)$predictions)
sims_clean$q95 <- as.vector(predict(RF.model$finalModel, sims_clean, type="quantiles", quantiles=0.95)$predictions)

# 7.1. Select final columns for report----
final_table <- sims_clean %>%
  select(Profile_ID, Layer_ID, TOP, BOTTOM, Average_depth,
         BD, BD.pred, q05, q95)

# 7.2. Save results ----
write.csv(final_table, "H:/Analysis/SIMS_BD_Predictions_with_Uncertainty.csv", row.names = FALSE)

