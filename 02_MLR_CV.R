# Testing simple PTFs with classical statistical techniques

# Developing a PTF for predicting BD based on the MARTHA dataset using stepwise multiple linear regression (MLR) and cross validation

# Author:Mehrmanzar Sohrab
# E-mail:sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load only necessary libraries ----
library(dplyr)       # For data manipulation
library(tidyr)       # For reshaping data 
library(caret)       # For creating cross-validation folds
library(hydroGOF)    # For NSE metric

# 2. Set working directory ----
setwd("H:/Analysis")          

# 3. Load the MARTHA data set ----
martha <- readRDS("MARTHA_with_EnvCov.rds")

# 4. Data Cleaning and Preparation ----
martha_clean <- martha %>%
  select(-CaCO3, -SOM, -Profile_ID) %>%  # Remove unused columns
  na.omit() %>%                          # Drop rows with any missing values
  mutate(
    Geology_02 = as.factor(Geology_02),
    Soil_type  = as.factor(Soil_type)
  )

# 5. Create folds for reproducible cross-validation ----
set.seed(1234)
folds <- createMultiFolds(martha_clean$BD, k = 10, times = 5)

# 6. Run Cross-Validation with MLR and Stepwise Selection ----

n <- length(folds)  # number of folds

validation <- data.frame(Fold = 1:n, # prepare our data frame to compute and store metrics during each fold.
                         ME = rep(NA, n),
                         MAE = rep(NA, n),
                         RMSE = rep(NA, n),
                         NSE = rep(NA, n))
for (i in 1:n) {
  train.set <- martha_clean[folds[[i]], ]
  test.set  <- martha_clean[-folds[[i]], ]
  
  model <- step(lm(BD ~ ., data = train.set), trace = 0) #Run stepwise lm model: all variables except BD
  
  # 7. Validation----
  
  # 7.1 predict using the test set ====
  test.set$BD_pred <- predict(model, newdata = test.set)
  
  # 7.2 # Calculate error measures ====
  
  validation$ME[i]   <- mean(test.set$BD_pred - test.set$BD, na.rm = TRUE)
  validation$MAE[i]  <- mean(abs(test.set$BD_pred - test.set$BD), na.rm = TRUE)
  validation$RMSE[i] <- sqrt(mean((test.set$BD_pred - test.set$BD)^2, na.rm = TRUE))
  validation$NSE[i]  <- hydroGOF::NSE(sim = test.set$BD_pred, obs = test.set$BD)
  
  rm(model, test.set, train.set) # remove unnecessary components
  message(paste0(round(100 * (i / n)), "% Done!")) # keep track of the computation
}


# 8. Check the validation metrics ----
mean(validation$ME); sd(validation$ME) # the mean error and its std.deviation
mean(validation$MAE); sd(validation$MAE) # the mean absolute error and its std.deviation
mean(validation$RMSE); sd(validation$RMSE) # the RMSE and its std.deviation
mean(validation$NSE); sd(validation$NSE) # the model efficiency coefficient (Nash-Sutcliff) and its std.deviation

# 9. Save validation results ----
saveRDS(validation, file = "H:/Analysis/Validation_MLR.rds")  



