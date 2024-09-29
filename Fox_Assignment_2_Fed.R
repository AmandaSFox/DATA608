# load libraries
library(tidyverse)
#library(httr)
#library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

#-----------------------------
# Import datasets 
#-----------------------------

# Inflation rate: Year over year % change calculated from CPI (baseline 1982-84)

inflation_data <- read_csv("https://raw.githubusercontent.com/AmandaSFox/DATA608/refs/heads/main/inflation.csv?token=GHSAT0AAAAAACYG5Y6CXN26IOQR53HGX5VIZXZ2IGQ")
inflation_long <- inflation_data %>% 
    pivot_longer(cols = Jan:Dec,
                 names_to = "month_name",
                 values_to = "inflation_rate") 
inflation_long <- inflation_long %>% 
    mutate(year_month = ym(paste(Year,month_name))) %>% 
    arrange(year_month)
inflation_long

# Unemployment rate 

unempl_data <- read_csv("https://raw.githubusercontent.com/AmandaSFox/DATA608/refs/heads/main/unempl.csv?token=GHSAT0AAAAAACYG5Y6D2A4V5SIFT2E7DHICZXZ2IXA")
unempl_long <- unempl_data %>% 
  pivot_longer(cols = Jan:Dec,
               names_to = "month_name",
               values_to = "unempl_rate") 
unempl_long <- unempl_long %>% 
  mutate(year_month = ym(paste(Year,month_name))) %>% 
  arrange(year_month)
unempl_long

# Fed funds CHANGES in rate:

fed_data <- read_csv("https://raw.githubusercontent.com/AmandaSFox/DATA608/refs/heads/main/fedchange.csv?token=GHSAT0AAAAAACYG5Y6C7DAPBDPCQQMY326OZXZ2HNQ")
str(fed_data)
fed_data <- fed_data %>% 
  mutate(year_month = observation_date,
         rate_pct = FEDFUNDS_CHG_20240903/100) %>% 
  arrange(year_month) %>% 
  mutate(significant = abs(FEDFUNDS_CHG_20240903) > 0.25,  
         direction = sign(FEDFUNDS_CHG_20240903),  
         consecutive = direction == lag(direction) & !is.na(direction),
         highlight_change = significant | consecutive)

# Three economic events

dotcom_start <- as.Date("2000-03-01")
dotcom_end <- as.Date("2002-10-01")
gfc_start <- as.Date("2007-12-01")
gfc_end <- as.Date("2009-06-01")
covid_start <- as.Date("2020-03-01")
covid_end <- as.Date("2022-06-01")

# Thresholds for plots

low_inflation <- .03
moderate_inflation <- .10
low_unempl <- .04
moderate_unempl <- .08

#-----------------------------
# Inflation rate plot
#-----------------------------

inflation_plot <- inflation_long %>% 
    ggplot(aes(x = year_month, y = inflation_rate, group = 1)) +
    # ribbons for dotcom, financial crisis, and covid time periods
      geom_rect(aes(xmin = dotcom_start, xmax = dotcom_end, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.2) +
      geom_rect(aes(xmin = gfc_start, xmax = gfc_end, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.2) +
      geom_rect(aes(xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.2) +
      geom_line(color = "forestgreen", size = .6) +
    # moderate and high inflation thresholds
      geom_hline(yintercept = low_inflation, color = "yellow2", linetype = "dashed", size = 1) +
      geom_hline(yintercept = moderate_inflation, color = "tomato", linetype = "dashed", size = 1) +
    # points on line out of bounds - NOT USED
      # geom_point(data = subset(inflation_long, inflation_rate > low_inflation & inflation_rate <= moderate_inflation),
      #             aes(color = "Moderate Inflation"), size = .5, shape = 16, fill = "yellow") +  
      # geom_point(data = subset(inflation_long, inflation_rate > moderate_inflation), 
      #             aes(color = "High Inflation"), size = .5, shape = 21, fill = "red") +  
      # scale_color_manual(values = c("Moderate Inflation" = "yellow", "High Inflation" = "red")) +
      scale_x_date(limits = c(as.Date("1999-01-01"), as.Date("2023-12-31")), 
                   breaks = seq(as.Date("1999-01-01"), as.Date("2023-12-31"), by = "12 months"),
                   #date_breaks = "12 months", 
                   date_labels = "%b %Y")  +
      scale_y_continuous(limits = c(0, 0.10), 
                         breaks = seq(0, 0.11, by = 0.01),
                         labels = label_percent(),
                         ) +
      labs(title = NULL, #"Inflation Trend: 12-Month Rolling Change in CPI",
           x = NULL,
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

inflation_plot

#-----------------------------
# Fed Funds Trend + Significant Changes/Runs
#-----------------------------

fed_plot <- fed_data %>% 
  ggplot(aes(x = year_month, y = rate_pct, group = 1)) +
  # ribbons for dotcom, financial crisis, and covid time periods
  geom_rect(aes(xmin = dotcom_start, xmax = dotcom_end, ymin = -Inf, ymax = Inf), 
            fill = "lightgray", alpha = 0.2) +
  geom_rect(aes(xmin = gfc_start, xmax = gfc_end, ymin = -Inf, ymax = Inf), 
            fill = "lightgray", alpha = 0.2) +
  geom_rect(aes(xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf), 
            fill = "lightgray", alpha = 0.2) +
  geom_line(color = "blue", size = .5) +
  scale_x_date(limits = c(as.Date("1999-01-01"), as.Date("2023-12-31")), 
                breaks = seq(as.Date("1999-01-01"), as.Date("2023-12-31"), by = "12 months"),
                date_labels = "%b %Y")  +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  labs(title = "Fed Funds Rate Changes by Month",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_plot

#------------------
# Significant rate INCREASES for inflation plot 
#------------------

fed_data_sig_incr <- fed_data %>% 
  filter(significant == TRUE,
         direction == 1)

fed_plot2 <- fed_data_sig_incr %>% 
  ggplot(aes(x = observation_date)) +
  geom_vline(aes(xintercept = as.numeric(observation_date)), linetype = "dashed", color = "blue", size = 0.5, alpha = 0.7) +
  scale_x_date(limits = c(as.Date("1999-01-01"), as.Date("2023-12-31")), 
               breaks = seq(as.Date("1999-01-01"), as.Date("2023-12-31"), by = "12 months"),
               date_labels = "%b %Y") +
  labs(title = "Significant Increases in Federal Funds Rate (>0.25%)",
       x = "Date",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_plot2

infl_fed_plot <- inflation_plot +
  geom_vline(data = fed_data %>% 
               filter(significant == TRUE, direction == 1),
             aes(xintercept = observation_date), 
             linetype = "dashed", color = "blue", alpha = 0.5)

infl_fed_plot

#-----------------------------
# Unemployment rate plot
#-----------------------------

unempl_plot <- unempl_long %>% 
  ggplot(aes(x = year_month, y = unempl_rate, group = 1)) +
  # ribbons for dotcom, financial crisis, and covid time periods
  geom_rect(aes(xmin = dotcom_start, xmax = dotcom_end, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.2) +
  geom_rect(aes(xmin = gfc_start, xmax = gfc_end, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.2) +
  geom_rect(aes(xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.2) +
  geom_line(color = "gray40", size = .6) +
  # moderate and high thresholds
  geom_hline(yintercept = low_unempl, color = "yellow2", linetype = "dashed", size = 1) +
  geom_hline(yintercept = moderate_unempl, color = "tomato", linetype = "dashed", size = 1) +
  scale_x_date(limits = c(as.Date("1999-01-01"), as.Date("2023-12-31")), 
               breaks = seq(as.Date("1999-01-01"), as.Date("2023-12-31"), by = "12 months"),
               date_labels = "%b %Y")  +
  scale_y_continuous(limits = c(0, 0.18), 
                     breaks = seq(0, 0.18, by = 0.02),
                     labels = label_percent()) +
  labs(title = NULL, 
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

unempl_plot


#---------------
# Significant rate DECREASES for unemployment plot
#---------------

fed_data_sig_decr <- fed_data %>% 
  filter(significant == TRUE,
         direction == -1)

fed_plot3 <- fed_data_sig_decr %>% 
  ggplot(aes(x = observation_date)) +
  geom_vline(aes(xintercept = as.numeric(observation_date)), linetype = "dashed", color = "darkgreen", size = 0.5, alpha = 0.7) +
  scale_x_date(limits = c(as.Date("1999-01-01"), as.Date("2023-12-31")), 
               breaks = seq(as.Date("1999-01-01"), as.Date("2023-12-31"), by = "12 months"),
               date_labels = "%b %Y") +
  labs(title = "Significant Decreases in Federal Funds Rate (>0.25%)",
       x = "Date",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_plot3

unempl_fed_plot <- unempl_plot +
  geom_vline(data = fed_data %>% 
               filter(significant == TRUE, direction == -1),
             aes(xintercept = observation_date), 
             linetype = "dashed", color = "gray50", alpha = 0.5)

unempl_fed_plot

#---------------
# summaries for conclusion
#---------------

# Summarize inflation table
inflation_summary <- inflation_long %>%
  mutate(inflation_category = case_when(
    inflation_rate <= low_inflation ~ "Low Inflation",
    inflation_rate > low_inflation & inflation_rate <= moderate_inflation ~ "Moderate Inflation",
    inflation_rate > moderate_inflation ~ "High Inflation"
  )) %>%
  group_by(inflation_category) %>%
  summarise(month_count = n()) %>% 
  mutate(percentage_of_total = (month_count / sum(month_count)) * 100)

inflation_summary

# Summarize unemployment table
unempl_summary <- unempl_long %>%
  mutate(unempl_category = case_when(
    unempl_rate <= low_unempl ~ "Full Employment",
    unempl_rate > low_unempl & unempl_rate <= moderate_unempl ~ "Moderate Unemployment",
    unempl_rate > moderate_unempl ~ "High Unemployment"
  )) %>%
  group_by(unempl_category) %>%
  summarise(month_count = n()) %>% 
  mutate(percentage_of_total = (month_count / sum(month_count)) * 100)

unempl_summary

