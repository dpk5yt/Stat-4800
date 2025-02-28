
library(tidyverse)

# Load data
data <- readxl::read_xlsx("lab 6 sample.xlsx")

# Convert necessary columns to factors
data <- data %>%
  mutate(
    Period = as.factor(Period),
    Time_Block = cut(Seconds_Elapsed, breaks = c(0, 400, 800, 1200),
                     labels = c("Early", "Mid", "Late")),
    Shot_Taken = ifelse(Event == "SHOT", 1, 0),
    Point_Differential = Home_Score - Away_Score
  )

# Shot rate model based on time and point diff
shot_model_time <- glm(
  Shot_Taken ~ Time_Block + Point_Differential,
  data = data,
  family = poisson()
)
summary(shot_model_time)

# with spatial location
data <- data %>% mutate(Shot_Location = ifelse(!is.na(xC) & !is.na(yC), 1, 0))
shot_model_spatial <- glm(
  Shot_Taken ~ xC + yC,
  data = data,
  family = poisson()
)
summary(shot_model_spatial)

# splitting ice into 6 regions
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
  family = poisson()
)
summary(shot_model_region)

# Estimating shot success rate
data <- data %>%
  mutate(Shot_Success = ifelse(Event == "GOAL" | Event == "SHOT", 1, 0))

success_model <- glm(
  Shot_Success ~ xC + yC,
  data = data,
  family = poisson()
)
summary(success_model)
