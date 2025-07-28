# Testing PTFs with Advanced statistical techniques

# Developing a PTF for predicting BD based on the MARTHA dataset using Cubist Regression Models with cross validation

# Author:Mehrmanzar Sohrab
# E-mail:sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load necessary libraries ----
library(dplyr)        # For data manipulation
library(tidyr)        # For reshaping data 
library(caret)        # For machine learning, cross-validation, and model tuning
library(hydroGOF)     # For calculating NSE 

# 2. Set working directory ----
setwd("H:/Analysis")        

# 3. Load the MARTHA dataset ----
martha <- readRDS("MARTHA_with_EnvCov.rds")

# 4. Data Cleaning ----
martha_clean <- martha %>%
  select(-CaCO3, -SOM, -Profile_ID) %>%  # Remove unnecessary variables
  na.omit() %>%                          # Remove rows with missing values
  mutate(
    Geology_02 = as.factor(Geology_02), # Convert categorical variables to factors
    Soil_type  = as.factor(Soil_type)
  )

# 5. Create repeated 10-fold cross-validation ----
set.seed(1234)
folds <- createMultiFolds(martha_clean$BD, k = 10, times = 5)  
n <- length(folds)

# 6. Initialize validation results ----
validation <- data.frame(Fold = 1:n,
                         ME = rep(NA, n),
                         MAE = rep(NA, n),
                         RMSE = rep(NA, n),
                         NSE = rep(NA, n))

# 7. Cross-validation loop with Cubist ----
for (i in 1:n) {
  
  # 7.1. Split training and testing sets ====
  
  train.set <- martha_clean[folds[[i]], ]     # Training set
  test.set  <- martha_clean[-folds[[i]], ]   # Test set
  
  # 7.2. Define tuning grid ====
  # committees: number of rule-based models combined
  # neighbors: how many nearby training samples are used for adjustment
  t.grid <- expand.grid(committees = 1:5, 
                        neighbors = 0:3)
  
  # 7.3. Train Cubist model ====
  set.seed(2024)
  cubist.model <- train(
    BD ~ .,                        # Predict BD using all other variables
    data = train.set,
    method = "cubist",
    tuneGrid = t.grid,            # Try different combinations of committees and neighbors
    trControl = trainControl(method = "cv", number = 10)  # 10-fold CV for tuning
  )
  
  # 7.4. Save final model (from last fold) ====
  if (i == n) {
    final_model <- cubist.model
    saveRDS(final_model, file = "H:/Analysis/final_Cubist_model.rds")
  }  }
  
  # 7.5. Predict on test set ====
  
  test.set$BD_pred <- predict(cubist.model, newdata = test.set)
  
  sim <- test.set$BD_pred
  obs <- test.set$BD
  
  # 7.6. Calculate validation metrics ====
  
  validation$ME[i]   <- mean(sim - obs, na.rm = TRUE)                  # Mean Error
  validation$MAE[i]  <- mean(abs(sim - obs), na.rm = TRUE)             # Mean Absolute Error
  validation$RMSE[i] <- sqrt(mean((sim - obs)^2, na.rm = TRUE))        # Root Mean Square Error
  validation$NSE[i]  <- NSE(sim = sim, obs = obs)                      # Nash-Sutcliffe Efficiency

  message(paste0(round(100 * (i / n)), "% Done!"))   # Progress tracking

# 8. Model checking ----  
plot(final_model)                    # Visualizes model performance across tuning grid
summary(final_model$finalModel)     # Details on the rules and performance of final Cubist model

# 9. Summarize validation metrics ----
print(round(colMeans(validation[, -1], na.rm = TRUE), 4))  # Mean values of metrics
print(round(apply(validation[, -1], 2, sd, na.rm = TRUE), 4))  # Standard deviations

# 10. Save validation results ----
saveRDS(validation, file = "H:/Analysis/Validation_Cubist.rds")        
