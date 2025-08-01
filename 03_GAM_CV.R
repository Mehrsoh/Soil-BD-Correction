# Testing simple PTFs with classical statistical techniques

# Developing a PTF for predicting BD based on the MARTHA dataset using Generalized Additive Model (GAM) with cross validation

# Author: Mehrmanzar Sohrab
# E-mail: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load necessary libraries ----
library(dplyr)       # Data manipulation
library(caret)       # Cross-validation folds
library(hydroGOF)    # For NSE 
library(mgcv)        # For GAM

# 2. Set working directory ----
setwd("H:/Analysis")          

# 3. Load the MARTHA dataset ----
martha <- readRDS("MARTHA_with_EnvCov.rds")

# 4. Data Cleaning ----
martha_clean <- martha %>%
  select(-CaCO3, -SOM, -Profile_ID) %>%
  na.omit() %>%
  mutate(
    Geology_02 = as.factor(Geology_02), # Convert categorical variables to factors
    Soil_type  = as.factor(Soil_type)
  )

# 5. Create repeated 10-fold cross-validation ----
set.seed(1234)
folds <- createMultiFolds(martha_clean$BD, k = 10, times = 5)

# 6. Create Dataframe for validation results ----

n <- length(folds)
validation <- data.frame(Fold = 1:n, # Preallocate a dataframe to store validation results
                         ME = rep(NA, n),
                         MAE = rep(NA, n),
                         RMSE = rep(NA, n),
                         NSE = rep(NA, n))

# 7. Cross-validation loop with GAM ----

# 7.1. Split training and test sets ====

for (i in 1:n) {
  
  train.set <- martha_clean[folds[[i]], ]
  test.set  <- martha_clean[-folds[[i]], ]
  
# 7.2.  Identify variables ====
  independent_vars <- setdiff(names(train.set), "BD") # Exclude target variable
  numeric_vars <- independent_vars[sapply(train.set[independent_vars], is.numeric)]  # Select numeric predictors
  factor_vars  <- independent_vars[sapply(train.set[independent_vars], is.factor)] # Select categorical predictors
  
# 7.3. Build GAM formula ====
  smooth_terms <- paste0("s(", numeric_vars, ")") # Apply smoothing to numeric variables
  full_formula_gam <- as.formula(paste("BD ~", paste(c(smooth_terms, factor_vars), collapse = " + ")))
  
# 7.4.  Fit GAM model ====
  model <- gam(full_formula_gam, data = train.set, family = gaussian(), select = TRUE) # Fit GAM with automatic term selection
  
# 7.5. Predict on test set ====
  test.set$BD_pred <- predict(model, newdata = test.set) # Predict BD for test set using trained model
  
# 7.6. Convert to numeric ====
  sim <- as.numeric(test.set$BD_pred) # Predicted values
  obs <- as.numeric(test.set$BD)  # Observed values
  
# 7.7. Validation metrics ====
  validation$ME[i]   <- mean(sim - obs, na.rm = TRUE)
  validation$MAE[i]  <- mean(abs(sim - obs), na.rm = TRUE)
  validation$RMSE[i] <- sqrt(mean((sim - obs)^2, na.rm = TRUE))
  validation$NSE[i]  <- NSE(sim = sim, obs = obs)

  message(paste0(round(100 * (i / n)), "% Done!")) # Show progress message
}

# 8. Check validation results ----

# 8.1. Desplay the results ====

print(round(colMeans(validation[, -1], na.rm = TRUE), 4)) # Print average of each validation metric
print(round(apply(validation[, -1], 2, sd, na.rm = TRUE), 4)) # Print standard deviation for each metric

plot(final_model) #visualize smooth terms of the final GAM (optional)

# 8.2. Save validation results ====
saveRDS(validation, file = "H:/Analysis/Validation_GAM.rds")  


