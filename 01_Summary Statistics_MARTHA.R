# Summary statistics data analysis of BD in MARTHA dataset 
# Compute summary statistics (min, max, mean, median, standard deviation, skewness) for MARTA for different soil depths (e.g., 0-30, 30-60, 60-100, and 100-200 cm) and Prepare histograms 

# Author: Mehrmanzar Sohrab
# E-mail: sohrab.mehrmanzar@atk.hun-ren.hu

# 1. Load necessary libraries ----
library(dplyr)
library(ggplot2)
library(e1071)   # For skewness

# 2. Set working directory ----
setwd("H:/Analysis")

# 3. Load MARTHA dataset ----
martha <- readRDS("MARTHA_with_EnvCov.rds")

# 4. Preparing MARTHA dataset ----
martha_cleaned <- martha %>%
  filter(!is.na(Average_depth), !is.na(BD)) %>%
  mutate(depth_interval = case_when(
    Average_depth >= 0   & Average_depth < 30   ~ "0-30 cm",
    Average_depth >= 30  & Average_depth < 60   ~ "30-60 cm",
    Average_depth >= 60  & Average_depth < 100  ~ "60-100 cm",
    Average_depth >= 100 & Average_depth <= 200 ~ "100-200 cm",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(depth_interval)) %>%
  mutate(
    depth_interval = factor(depth_interval, levels = c("0-30 cm", "30-60 cm", "60-100 cm", "100-200 cm"))
  )

# 5. Compute summary statistics by depth ----
martha_stats <- martha_cleaned %>%
  group_by(depth_interval) %>%
  summarise(
    count = n(),
    min = min(BD, na.rm = TRUE),
    max = max(BD, na.rm = TRUE),
    mean = mean(BD, na.rm = TRUE),
    median = median(BD, na.rm = TRUE),
    sd = sd(BD, na.rm = TRUE),
    skewness = skewness(BD, na.rm = TRUE),
    .groups = "drop"
  )

# 5.1. View and save summary statistics ====
print(martha_stats)
write.csv(martha_stats, "H:/Analysis/BD_summary_by_depth_MARTHA.csv", row.names = FALSE)

# 6. Mean BD Profile by Depth ----
ggplot(martha_stats, aes(x = mean, y = depth_interval)) +
  geom_line(group = 1, color = "#1f77b4", size = 1.5) +
  geom_point(color = "#1f77b4", size = 3) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Mean BD Profile by Depth (MARTHA)",
    x = "Mean Bulk Density (g/cm³)",
    y = "Depth Interval"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# 7. Histogram of Relative Frequency by Depth ----
ggplot(martha_cleaned, aes(x = BD, fill = depth_interval)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.1,
    position = "identity",
    alpha = 0.5,
    color = NA
  ) +
  facet_wrap(~ depth_interval, scales = "free_y") +
  labs(
    title = "Relative Frequency Histogram of BD by Depth Interval (MARTHA)",
    x = "Bulk Density (g/cm³)",
    y = "Relative Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# 8. Violin Plots of BD by Depth ----
ggplot(martha_cleaned, aes(x = depth_interval, y = BD, fill = depth_interval)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +
  labs(
    title = "Violin Plots of Bulk Density by Depth Interval (MARTHA)",
    x = "Depth Interval",
    y = "Bulk Density (g/cm³)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


