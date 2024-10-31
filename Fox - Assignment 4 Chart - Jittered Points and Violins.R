# Salary Distribution by State and Role:
# Jittered points plus violins to highlight shapes

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Prepare data
df_salaries <- read_csv("https://github.com/AmandaSFox/DATA608/raw/refs/heads/main/salaries.csv")
df_state <- df_salaries %>%
  filter(state != "US")

# Chart
df_state %>%
  ggplot(aes(x = role, y = average)) +
  geom_jitter(aes(color = role), width = 0.2, height = 0, alpha = 0.6, size = 2) +  # Jitter points to reduce overlap
  scale_color_manual(values = c("steelblue", "peachpuff", "lightblue", "cornflowerblue", "lightcoral")) +
  geom_violin(fill = "gray70", color = NA, alpha = 0.2) +  
  labs(title = "2024 Average Salary Distribution by State",
       x = "",
       y = "") +
  scale_y_continuous(labels = dollar, breaks = pretty_breaks()) +
  theme_minimal() +
  theme(legend.position = "none")
