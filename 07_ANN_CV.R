# Testing PTFs with advanced techniques

# Developing a PTF for predicting BD based on the MARTHA dataset using Artificial Neural Network (ANN) 

# Author:Mehrmanzar Sohrab
# E-mail:sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load necessary libraries ----
library(tidyr)       # Data reshaping
library(dplyr)        # For data manipulation
library(caret)       # Model training and cross-validation
library(hydroGOF)    # Evaluation metrics (NSE)
library(nnet)        # Neural networks

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

# 6. set a data frame for validation results ----
validation <- data.frame(Fold = 1:n,
                         ME = NA, MAE = NA,
                         RMSE = NA, NSE = NA)

# 7. Cross-validation loop with ANN ----
for (i in 1:n) {

  # 7.1 Split into training and testing sets ====
  
  train.set <- martha_clean[folds[[i]], ]
  test.set  <- martha_clean[-folds[[i]], ]
  
# 7.2.Train ANN model with cross-validation and hyperparameter tuning ====
  
  set.seed(2024)
  
  ann.model <- train(BD ~ .,                        # Use all predictors to model BD
                     data = train.set,              # Training dataset for current fold
                     method = "nnet",               # Use neural network method
                     tuneGrid = expand.grid(        # Grid of tuning parameters:
                       size = c(5, 10, 20, 30, 40, 50),   # Number of hidden neurons
                       decay = c(0.01, 0.1, 0.5)          # Regularization parameter
                     ),
                     trControl = trainControl(method = "cv", number = 10), # 10-fold CV
                     linout = TRUE,                 # Linear output for regression
                     trace = FALSE,                 # Don't show training details
                     maxit = 1000)                  # Maximum iterations for training
  
  
# 7.3.  Save final model for inspection ====
  if (i == n) {
    final_model <- ann.model
    saveRDS(final_model, file = "H:/Analysis/final_ANN_model.rds")
  }
# 7.4. Predict and calculate validation metrics ====
  pred <- predict(ann.model, newdata = test.set)
  obs  <- test.set$BD
  
  validation$ME[i]   <- mean(pred - obs)
  validation$MAE[i]  <- mean(abs(pred - obs))
  validation$RMSE[i] <- sqrt(mean((pred - obs)^2))
  validation$NSE[i]  <- NSE(pred, obs)

  message(paste0(round(100 * (i / n)), "% Done"))
}

# 8. Save results ====
saveRDS(validation, "H:/Analysis/Validation_ANN_Tuned.rds")

# 8.1. Mean and SD of validation metrics across folds ====
print(round(colMeans(validation[, -1], na.rm = TRUE), 4))
print(round(apply(validation[, -1], 2, sd, na.rm = TRUE), 4))

