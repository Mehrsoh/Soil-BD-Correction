# Testing PTFs with advanced techniques

# Developing a PTF for predicting BD based on the MARTHA dataset using Random Forest Model (RF) with cross validation 

# Author:Mehrmanzar Sohrab
# E-mail:sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load necessary libraries ----
library(dplyr)
library(caret)
library(hydroGOF)
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

# 6. Initialize validation results ----
validation <- data.frame(Fold = 1:n,
                         ME = rep(NA, n),
                         MAE = rep(NA, n),
                         RMSE = rep(NA, n),
                         NSE = rep(NA, n))

# 7. Start cross-validation loop with RF ----
for (i in 1:n) {
  
# 7.1. Split training and test sets ====
  
  train.set <- martha_clean[folds[[i]], ]
  test.set  <- martha_clean[-folds[[i]], ]
  
  # 7.2. Define tuning grid for RF ====
  
  t.grid <- expand.grid(mtry=c(1, seq(5,55,by=5), 58),
                        splitrule="variance",
                        min.node.size=c(5))
  
  # 7.3. Train RF model with tuning ====
  set.seed(2024)
  rf.model <- train(
    BD ~ ., 
    data = train.set,
    method = "ranger",
    tuneGrid=t.grid,
    trControl = trainControl(method = "cv", number = 10),# Inner 10-fold CV
    importance="impurity", # variable importance 
    num.trees=200,
    num.threads=c(detectCores()-2)
  )
  
  # 7.4. Save final model on last fold ====
  if (i == n) {
    final_model <- rf.model
    saveRDS(final_model, file = "H:/Analysis/final_RF_model.rds")
  }
  
  # 7.5. Predict BD on test set ====
  
  test.set$BD_pred <- predict(rf.model, newdata = test.set)
  
  # 7.6. Calculate validation metrics ====
  
  sim <- test.set$BD_pred
  obs <- test.set$BD
  
  validation$ME[i]   <- mean(sim - obs, na.rm = TRUE)
  validation$MAE[i]  <- mean(abs(sim - obs), na.rm = TRUE)
  validation$RMSE[i] <- sqrt(mean((sim - obs)^2, na.rm = TRUE))
  validation$NSE[i]  <- NSE(sim = sim, obs = obs)
  
  message(paste0(round(100 * (i / n)), "% Done!")) #track the process
}

# 8. Check model performance ---- optional
plot(final_model)  # Plot tuning results
summary(final_model$finalModel)  # Model summary

# 9. Summarize results ----
print(round(colMeans(validation[, -1], na.rm = TRUE), 4)) # Mean of metrics
print(round(apply(validation[, -1], 2, sd, na.rm = TRUE), 4)) # SD of metrics

# 10. Save results ----
saveRDS(validation, file = "H:/Analysis/Validation_RF.rds")


