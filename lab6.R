# Load necessary libraries
library(tidyverse)

# Load data
data <- read.csv("nhl_data.csv")

# Convert necessary columns to factors
data <- data %>%
  mutate(
    Period = as.factor(Period),
    Time_Block = cut(Seconds_Elapsed, breaks = c(0, 400, 800, 1200),
                     labels = c("Early", "Mid", "Late")),
    Shot_Taken = ifelse(Event == "SHOT", 1, 0),
    Point_Differential = Home_Score - Away_Score
  )
#Problem 1
# Shot rate model 
shot_model_time <- glm(
  Shot_Taken ~ Time_Block + Point_Differential,
  data = data,
  family = binomial()
)
summary(shot_model_time)

#Problem 2
# Model incorporating spatial location (xC, yC)
data <- data %>% mutate(Shot_Location = ifelse(!is.na(xC) & !is.na(yC), 1, 0))
shot_model_spatial <- glm(
  Shot_Taken ~ xC + yC,
  data = data,
  family = binomial()
)
summary(shot_model_spatial)


# split playing field into 6 regions
data <- data %>% mutate(
  Ice_Region = case_when(
    xC < -50 & yC > 0 ~ "Left_Wing",
    xC < -50 & yC < 0 ~ "Left_Defense",
    xC > 50 & yC > 0 ~ "Right_Wing",
    xC > 50 & yC < 0 ~ "Right_Defense",
    abs(xC) < 50 & yC > 0 ~ "Center_Offense",
    abs(xC) < 50 & yC < 0 ~ "Center_Defense",
    TRUE ~ "Neutral"
  )
)
shot_model_region <- glm(
  Shot_Taken ~ Ice_Region,
  data = data,
  family = binomial()
)
summary(shot_model_region)

#Problem 3
# Estimating shot success rate (on goal vs miss)
data <- data %>%
  mutate(Shot_Success = ifelse(Event == "GOAL" | Event == "SHOT", 1, 0))

success_model <- glm(
  Shot_Success ~ xC + yC,
  data = data,
  family = binomial()
)
summary(success_model)
