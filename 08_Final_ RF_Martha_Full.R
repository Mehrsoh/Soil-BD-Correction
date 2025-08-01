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
  keep.inbag=TRUE, # Keep in-bag samples for later uncertainty quantification
  quantreg = TRUE,        # Enable prediction intervals (uncertainty)
  num.trees = 200, # Number of trees
  num.threads = detectCores()-4
)
message("Final model trained on full dataset!")

# 7. Save final model for further analysis ----
saveRDS(final_rf_full, "H:/Analysis/Final RF Model/Final_RF_FullModel.rds")



