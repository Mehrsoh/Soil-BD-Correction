# Variable importance of our final Random Forest Model on Full MARTHA Dataset
# Author: Mehrmanzar Sohrab
# Email: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load required libraries ----
library(dplyr)    # For arranging and handling the importance table
library(caret)    # For extracting variable importance from the RF model
library(ggplot2)  # For plotting the importance as a bar chart

# 2. Load the saved Random Forest model ----

rf_model <- readRDS("H:/Analysis/Final RF Model/Final_RF_FullModel.rds")

# 3. Extract variable importance ----
var_imp <- varImp(rf_model)$importance # Extracts importance scores for each predictor
var_imp$Variable <- rownames(var_imp)   # Converts row names into a new column "Variable"
var_imp <- var_imp[order(-var_imp$Overall), ]  # Sorts variables from most to least important

# 3.1. Plot variable importance ====
varimp_plot <- ggplot(var_imp, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(
    title = "Variable Importance Final RF on Martha dataset",
    x = "Variables",
    y = "Importance"
  ) +
  theme_minimal()

# 3.2. Show the plot in the RStudio plot window ====
print(varimp_plot)

# 3.3. Save the variable importance table as a CSV file ====
write.csv(var_imp, file.path("H:/Analysis/Final RF Model", "Variable_Importance_Table.csv"),
          row.names = FALSE)

