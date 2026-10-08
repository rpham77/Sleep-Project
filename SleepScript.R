
library(dplyr)
library(ggplot2)
library(car)
sleep_data <- read.csv(file.choose())
# Check the data
names(sleep_data)
glimpse(sleep_data)

# Convert categorical variables to factors
sleep_data <- sleep_data %>%
  mutate(
    occupation = as.factor(occupation),
    mental_health_condition = as.factor(mental_health_condition),
    sleep_disorder_risk = as.factor(sleep_disorder_risk)
  )

#sleep quality
sleep_data %>%
  summarise(
    n = sum(!is.na(sleep_quality_score)),
    mean_sleep_quality = mean(sleep_quality_score, na.rm = TRUE),
    median_sleep_quality = median(sleep_quality_score, na.rm = TRUE),
    sd_sleep_quality = sd(sleep_quality_score, na.rm = TRUE),
    min_sleep_quality = min(sleep_quality_score, na.rm = TRUE),
    max_sleep_quality = max(sleep_quality_score, na.rm = TRUE)
  )

#Occupation vs. Sleep Quality
sleep_data %>%
  group_by(occupation) %>%
  summarise(
    n = n(),
    mean_sleep_quality = mean(sleep_quality_score, na.rm = TRUE),
    median_sleep_quality = median(sleep_quality_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_sleep_quality))

ggplot(sleep_data, aes(x = occupation, y = sleep_quality_score)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Sleep Quality by Occupation",
    x = "Occupation",
    y = "Sleep Quality Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Wake Episodes vs Sleep Quality
ggplot(sleep_data, aes(
  x = wake_episodes_per_night,
  y = sleep_quality_score
)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(
    title = "Wake Episodes per Night vs. Sleep Quality",
    x = "Wake Episodes per Night",
    y = "Sleep Quality Score"
  ) +
  theme_minimal()

#Stress Score vs Sleep Quality
ggplot(sleep_data, aes(
  x = stress_score,
  y = sleep_quality_score
)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(
    title = "Stress Score vs. Sleep Quality",
    x = "Stress Score",
    y = "Sleep Quality Score"
  ) +
  theme_minimal()


#Mental Health Condition vs Sleep Quality
sleep_data %>%
  group_by(mental_health_condition) %>%
  summarise(
    n = n(),
    mean_sleep_quality = mean(sleep_quality_score, na.rm = TRUE),
    median_sleep_quality = median(sleep_quality_score, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(sleep_data, aes(
  x = mental_health_condition,
  y = sleep_quality_score
)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Sleep Quality by Mental Health Condition",
    x = "Mental Health Condition",
    y = "Sleep Quality Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Cognitive Performance vs. Sleep Quality
ggplot(sleep_data, aes(
  x = cognitive_performance_score,
  y = sleep_quality_score
)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(
    title = "Cognitive Performance vs. Sleep Quality",
    x = "Cognitive Performance Score",
    y = "Sleep Quality Score"
  ) +
  theme_minimal()

#Sleep Disorder Risk vs Sleep Quality
sleep_data %>%
  group_by(sleep_disorder_risk) %>%
  summarise(
    n = n(),
    mean_sleep_quality = mean(sleep_quality_score, na.rm = TRUE),
    median_sleep_quality = median(sleep_quality_score, na.rm = TRUE),
    .groups = "drop"
  )
#sleep disorder risk vs sleep quality
ggplot(sleep_data, aes(
  x = sleep_disorder_risk,
  y = sleep_quality_score
)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Sleep Quality by Sleep Disorder Risk",
    x = "Sleep Disorder Risk",
    y = "Sleep Quality Score"
  ) +
  theme_minimal()

#Numerical Variable Correlations
numeric_data <- sleep_data %>%
  select(
    sleep_quality_score,
    wake_episodes_per_night,
    stress_score,
    cognitive_performance_score
  )

cor(numeric_data, use = "pairwise.complete.obs")

#Multiple Linear Regression
sleep_model <- lm(
  sleep_quality_score ~
    occupation +
    wake_episodes_per_night +
    stress_score +
    mental_health_condition +
    cognitive_performance_score +
    sleep_disorder_risk,
  data = sleep_data,
  na.action = na.exclude
)

summary(sleep_model)


#R-Squared vs Adjusted R-squared
# R-squared
summary(sleep_model)$r.squared

# Adjusted R-squared
summary(sleep_model)$adj.r.squared

# RMSE
actual <- model.response(model.frame(sleep_model))
predicted <- predict(sleep_model)

sqrt(mean((actual - predicted)^2, na.rm = TRUE))

#Multicollinearity
vif(sleep_model)

#Actual vs. Predicted Sleep Quality
prediction_data <- data.frame(
  actual = actual,
  predicted = predicted
)

ggplot(prediction_data, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = "Actual vs. Predicted Sleep Quality",
    x = "Actual Sleep Quality Score",
    y = "Predicted Sleep Quality Score"
  ) +
  theme_minimal()

# Model 1: Stress only
model1 <- lm(
  sleep_quality_score ~ stress_score,
  data = sleep_data,
  na.action = na.exclude
)

# Model 2: Stress + Mental Health Condition
model2 <- lm(
  sleep_quality_score ~ stress_score + mental_health_condition,
  data = sleep_data,
  na.action = na.exclude
)

# Model 3: Add Wake Episodes, Cognitive Performance, and Sleep Disorder Risk
model3 <- lm(
  sleep_quality_score ~
    stress_score +
    mental_health_condition +
    wake_episodes_per_night +
    cognitive_performance_score +
    sleep_disorder_risk,
  data = sleep_data,
  na.action = na.exclude
)

# Model 4: Add Occupation
model4 <- lm(
  sleep_quality_score ~
    stress_score +
    mental_health_condition +
    wake_episodes_per_night +
    cognitive_performance_score +
    sleep_disorder_risk +
    occupation,
  data = sleep_data,
  na.action = na.exclude
)


# Model Summaries
summary(model1)
summary(model2)
summary(model3)
summary(model4)


# Compare Candidate Models Using AIC
AIC(model1, model2, model3, model4)


# Compare Adjusted R-Squared
data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  Adjusted_R_Squared = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared,
    summary(model3)$adj.r.squared,
    summary(model4)$adj.r.squared
  )
)


# Diagnostic Plots for Model 4
plot(model4, which = 1)
plot(model4, which = 2)
