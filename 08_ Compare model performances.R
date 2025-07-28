
# Comparison of the Mean, Median and Standard Deviation of Validation Metrics Among Pedotransfer Function (PTF) Models.

# Author:Mehrmanzar Sohrab
# E-mail:sohrab.mehrmanzar@atk.hun-ren.hu


# 1. Load necessary libraries ----
library(dplyr)  # data manipulation
library(ggplot2) # plotting
library(tidyr) # reshaping
library(readr) # save table


# 2. Set working directory ----
setwd("H:/Analysis")         

# 3. Recall and label validation tables ----
ANN    <- readRDS("2.c.advanced PTFs/Validation_ANN.rds")      %>% mutate(Model = "ANN")   
RF     <- readRDS("2.c.advanced PTFs/Validation_RF_final2.rds")       %>% mutate(Model = "RF")
Cubist <- readRDS("2.c.advanced PTFs/Validation_Cubist.rds")   %>% mutate(Model = "Cubist")
GAM    <- readRDS("2.b.simple PTFs/Validation_GAM.rds")        %>% mutate(Model = "GAM")
MLR    <- readRDS("2.b.simple PTFs/Validation_MLR.rds")        %>% mutate(Model = "MLR")

# 4. Combine all and reshape ----
all <- bind_rows(ANN, RF, Cubist, GAM, MLR) %>%  
  pivot_longer(cols = c(ME, MAE, RMSE, NSE),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Model = factor(Model, levels = c("ANN", "RF", "Cubist", "GAM", "MLR"))) 

# 5. Calculate median and SD ----
# Group by model and metric to calculate median and standard deviation

stats <- all %>%
  group_by(Model, Metric) %>%
  summarise(Median = median(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE), .groups = "drop")

# 6. Create boxplots to compare distribution of metrics across models with facets ----
p <- ggplot(all, aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(colour = "black") +
  facet_wrap(~ Metric, scales = "free", ncol = 2) +
  labs(title = "",
       y = "Metric Value", x = "Model") +
  theme_minimal(base_size = 16) +   
  theme(
    plot.title       = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x     = element_text(size = 16, face = "bold"),
    axis.title.y     = element_text(size = 16, face = "bold"),
    axis.text.x      = element_text(size = 16),
    axis.text.y      = element_text(size = 16),
    strip.text       = element_text(size = 16, face = "bold"),  # facet labels
    legend.position  = "none"
  )

# 6.1.Display the generated boxplot ====
p

# 7) Calculate Mean and SD for each Model-Metric combination ----
stats_table <- all %>%
  group_by(Model, Metric) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Metric, Model)

# 7.1. View the table in console ====
print(stats_table)

# 7.2.Save to CSV  ====
write_csv(stats_table, "Validation_Metrics_Summary.csv")


