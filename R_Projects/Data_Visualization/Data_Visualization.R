# Author: Darshana Anandan
# Guided Project: Analyzing Forest Fire Data
# Data associated with the paper titled "A Data Mining Approach to Predict Forest Fires using Meteorological Data"
## The study aims to develop modelling techniques to predict the occurrence of forest fires in Portugal.

# ==============================================================================
# Clean the environment

rm(list = ls())
setwd("C:/Users/Darshana/Documents/Data Samples")

# ==============================================================================
# Import libraries

# install.packages("readr")
# install.packages("tidyverse")

# ==============================================================================
# Understanding the Data

library(readr)
library(tidyverse)
forest_fires <- read_csv("forestfires.csv")
str(forest_fires)
summary(forest_fires)

# ==============================================================================
# Data Processing

forest_fires %>% pull(month) %>% unique
forest_fires %>% pull(day) %>% unique

## Defining the order for months and days
month_order <- c(
  "jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"
)
day_order <- c("sun","mon","tue","wed","thu","fri","sat")

## Creating categorical variables for month and day and ordering them
forest_fires <- forest_fires %>%
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = day_order)
  )

# ==============================================================================
# When do most forest fires occur?

fires_month <- forest_fires %>%
  group_by(month) %>%
  summarize(month_total = n())

fires_day <- forest_fires %>%
  group_by(day) %>%
  summarize(day_total = n())

## Plotting the frequency of forest fires by month and day

fires_month %>%
  ggplot(aes(x = month, y = month_total, fill = month))+
  geom_col()+
  labs(
    title = "Frequency of Forest Fires by Month",
    x = "Month",
    y = "Frequency") +
  theme(
    panel.background = element_rect(fill = "white"))

fires_day %>%
  ggplot(aes(x = day, y = day_total, fill = day))+
  geom_col()+
  labs(
    title = "Frequency of Forest Fires by Day",
    x = "Day",
    y = "Frequency") +
  theme(
    panel.background = element_rect(fill = "white"))

# ==============================================================================
# Understanding the temporal patterns of variables that relate to forest fires

fires_long <- forest_fires %>%
pivot_longer(
  cols = c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain"),
  names_to = "Vars_Fires",
  values_to = "Values"
)
fires_long %>%
  ggplot(aes(x = month, y = Values)) +
  geom_boxplot() +
  facet_wrap(vars(Vars_Fires), scale = "free_y") +
  labs(
    title = "Temporal relationship",
    x = "Month",
    y = "Values") +
    theme(
      panel.background = element_rect(fill = "white"))

# ==============================================================================
# Examining forest fire severity
## Using the 'area' variable which describes the number of hectares of forest
## that burned during the fire as a proxy for the intensity of the fire.

fires_long %>%
  ggplot(aes(x = Values, y = area)) +
  geom_point() +
  facet_wrap(vars(Vars_Fires), scale = "free_x") +
  labs(
    title = "Forest Fire Intensity",
    x = "Variables",
    y = "Area") +
  theme(
    panel.background = element_rect(fill = "white"))

# ==============================================================================
# Outlier Problems
## Identifying the outliers
range(fires_long$area, na.rm=TRUE)
fires_long %>%
  ggplot(aes(x = area))+
  geom_histogram()

fires_long_low <- fires_long %>%
  filter(area < 300, area > 0)
fires_long_low %>%
  ggplot(aes(x = Values, y = area)) +
  geom_point() +
  facet_wrap(vars(Vars_Fires), scale = "free_x") +
  labs(
    title = "Forest Fire (Area < 300)",
    x = "Variables",
    y = "Area") +
  theme(
    panel.background = element_rect(fill = "white"))

# ==============================================================================

  

