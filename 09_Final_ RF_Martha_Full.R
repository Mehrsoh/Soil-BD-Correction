# Variable importance of our final Random Forest Model on Full MARTHA Dataset
# Author: Mehrmanzar Sohrab
# Email: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load necessary libraries ----

library(dplyr) # For data manipulation (select, mutate, etc.)
library(caret) # For machine learning model training and tuning
library(ranger)  # For fast Random Forest implementation
library(hydroGOF) # For calculating performance metrics (e.g., NSE)

# 2. Set working directory ----
setwd("H:/Analysis")

# 3. Load MARTHA dataset ----
martha <- readRDS("MARTHA_with_EnvCov.rds")

# 4. Data cleaning ----
martha_clean <- martha %>%
  select(-CaCO3, -SOM, -Profile_ID) %>%
  na.omit() %>%
  mutate(
    Geology_02 = as.factor(Geology_02),
    Soil_type  = as.factor(Soil_type)
  )

# 5. Set tuning grid ----
t.grid <- expand.grid(mtry=c(1,2,3,4, seq(5,55,by=5), 58), # Number of variables to try at each split
                      splitrule="variance",  # Splitting criterion for regression
                      min.node.size=c(5)) # Minimum samples per terminal node


# 6. Train final Random Forest model using all data ----

set.seed(2025)
final_rf_full <- train(
  x=martha_clean[,c(1,3:59)], # Predictor variables (exclude target BD column)
  y=martha_clean$BD,   # Target variable (Bulk Density)
  method = "ranger", # Random Forest algorithm from ranger
  tuneGrid = t.grid, # Parameter grid for tuning
  trControl = trainControl(method = "cv"),  # Cross-validation for tuning
  importance = "impurity",  # Calculate variable importance
  quantreg = TRUE,        # Enable prediction intervals (uncertainty)
  num.trees = 200, # Number of trees
  num.threads = detectCores()-4
)
message("Final model trained on full dataset!")

# 7. Save final model for further analysis ----
saveRDS(final_rf_full, "H:/Analysis/Final RF Model/Final_RF_FullModel.rds")

# 8. Predict BD on the full dataset ----
martha_clean$BD_pred <- predict(final_rf_full, newdata = martha_clean)

# 9. Calculate validation metrics ----
sim <- martha_clean$BD_pred # Predicted values
obs <- martha_clean$BD # Predicted values

# Create a data frame with validation metrics
validation_metrics <- data.frame(
  ME   = mean(sim - obs, na.rm = TRUE), # Mean Error (bias)
  MAE  = mean(abs(sim - obs), na.rm = TRUE),  # Mean Absolute Error
  RMSE = sqrt(mean((sim - obs)^2, na.rm = TRUE)), # Root Mean Square Error
  NSE  = hydroGOF::NSE(sim = sim, obs = obs)   # Nash–Sutcliffe Efficiency
)

# 10. Print metrics rounded to 4 decimals ----
print(round(validation_metrics, 4))

# 11. Save validation metrics ----
saveRDS(validation_metrics, "H:/Analysis/Final RF Model/RF_FullModel_Validation.rds")

