
# Developing a PTF considering environmental covariates (-soil types) + SOC + PH_H2O for predicting BD based on the MARTHA dataset using Random Forest Model (RF) with cross validation 

# Author:Mehrmanzar Sohrab
# E-mail:sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load necessary libraries ----
library(dplyr)
library(caret)
library(hydroGOF)
library(ggplot2)
library(tidyr)
library(doParallel)

# 2. Set working directory ----
setwd("H:/Analysis")           

# 3. Load the MARTHA dataset ----
martha <- readRDS("MARTHA_with_EnvCov.rds")

# 4. Data Cleaning ----
martha_clean <- martha %>%
  select(-CaCO3, -SOM, -Profile_ID) %>%
  na.omit() %>%
  mutate(
    Geology_02 = as.factor(Geology_02),
    Soil_type  = as.factor(Soil_type)
  )

# 5. Create repeated 10-fold cross-validation ----
set.seed(1234)
folds <- createMultiFolds(martha_clean$BD, k = 10, times = 5)
n <- length(folds)

# 6. set a data frame for validation results ----
validation <- data.frame(Fold = 1:n,
                         ME = rep(NA, n),
                         MAE = rep(NA, n),
                         RMSE = rep(NA, n),
                         NSE = rep(NA, n))

# 7. Start RF modeling with repeated 10-fold cross-validation ----
for (i in 1:n) {
  # 7.1. Split data into training and test sets ====
  
  train.set <- martha_clean[folds[[i]], ]
  test.set  <- martha_clean[-folds[[i]], ]
  

  # 7.2. Define tuning grid for hyperparameter search ====
  
  t.grid <- expand.grid(mtry=c(1, seq(5,50,by=5), 54),  # mtry= number of independant variables
                        splitrule="variance",
                        min.node.size=c(5))
  
  # 7.3. Train Random Forest model using caret ====
  set.seed(2024)
  rf.model <- train(
    x=train.set[,c(1,3,7:21,23:59)], # names(martha_clean[,c(1,3,7:21,23:59)])
    y=train.set$BD,
    method = "ranger",
    tuneGrid=t.grid,
    trControl = trainControl(method = "cv", number = 10),
    importance="impurity",
    num.trees=200,
    num.threads=c(detectCores()-2)
  )
  
  # 7.4. Predict BD using test set ====
  test.set$BD_pred <- predict(rf.model, newdata = test.set)
  
  # 7.5. Calculate and save validation metrics ====
  sim <- test.set$BD_pred
  obs <- test.set$BD
  
  validation$ME[i]   <- mean(sim - obs, na.rm = TRUE)
  validation$MAE[i]  <- mean(abs(sim - obs), na.rm = TRUE)
  validation$RMSE[i] <- sqrt(mean((sim - obs)^2, na.rm = TRUE))
  validation$NSE[i]  <- NSE(sim = sim, obs = obs)
  
  # 7.6. Save the final model only in the last fold ====
  if (i == n) {
    final_model <- rf.model
    saveRDS(final_model, file = "H:/Analysis/Scenarios/Final_RF_Mode1_scenario 3.rds")
  }
  
  message(paste0(round(100 * (i / n)), "% Done!"))  # track the process
}

# 8. Model evaluation and export results ----

# 8.1. Visualize final model performance ====

plot(final_model)
summary(final_model$finalModel)

# 8.2. Summarize validation metrics ====
print(round(colMeans(validation[, -1], na.rm = TRUE), 4))
print(round(apply(validation[, -1], 2, sd, na.rm = TRUE), 4))

# 8.3. Save validation results ====
saveRDS(validation, file = "H:/Analysis/Scenarios/Validation_scenario 3.rds")

