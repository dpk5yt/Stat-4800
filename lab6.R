
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
#model 1
# Shot rate model based on time and point diff
shot_model_time <- glm(
  Shot_Taken ~ Time_Block + Point_Differential,
  data = data,
  family = poisson()
)
summary(shot_model_time)

#model 2
# with spatial location
data <- data %>% mutate(Shot_Location = ifelse(!is.na(xC) & !is.na(yC), 1, 0))
shot_model_spatial <- glm(
  Shot_Taken ~ xC + yC,
  data = data,
  family = poisson()
)
summary(shot_model_spatial)

# splitting ice into 6 regions-w=wing,d=defense,0=0ffense
data <- data %>% mutate(
  Ice_Region = case_when(
    xC < -50 & yC > 0 ~ "Left_W",
    xC < -50 & yC < 0 ~ "Left_D",
    xC > 50 & yC > 0 ~ "Right_W",
    xC > 50 & yC < 0 ~ "Right_D",
    abs(xC) < 50 & yC > 0 ~ "Center_O",
    abs(xC) < 50 & yC < 0 ~ "Center_D",
    TRUE ~ "Neutral"
  )
)
shot_model_region <- glm(
  Shot_Taken ~ Ice_Region,
  data = data,
  family = poisson()
)
summary(shot_model_region)

#model 3
# Estimating shot success rate
data <- data %>%
  mutate(Shot_Success = ifelse(Event == "GOAL" | Event == "SHOT", 1, 0))

success_model <- glm(
  Shot_Success ~ xC + yC,
  data = data,
  family = poisson()
)
summary(success_model)
 