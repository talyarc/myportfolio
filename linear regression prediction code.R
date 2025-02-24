boston_data <- read.csv("C:/Users/talya/Downloads/mba project/950 mba r/final/Boston.csv", header = TRUE, sep = ",")
head(boston_data)
# Fit the multiple linear regression model, using lowercase column names
lm_model <- lm(value ~ crim + zn + indus + chas + nox + rm + age + dis + tax + ptratio, data = boston_data)

# View the summary of the model to check R-squared and other statistics
summary(lm_model)

# Fit the regression models for each variable against all other predictors
slrfit1 <- lm(crim ~ zn + indus + chas + nox + rm + age + dis + tax + ptratio, data = boston_data)
summary(slrfit1)

slrfit2 <- lm(zn ~ crim + indus + chas + nox + rm + age + dis + tax + ptratio, data = boston_data)
summary(slrfit2)

slrfit3 <- lm(indus ~ crim + zn + chas + nox + rm + age + dis + tax + ptratio, data = boston_data)
summary(slrfit3)

lm_model <- lm(value ~ crim + zn + indus + chas + nox + rm + age + dis + tax + ptratio, data = boston_data)
summary(lm_model)

# Create a data frame with the new property characteristics
new_property <- data.frame(
  crim = 0.005,
  zn = 12,
  indus = 2.1,
  chas = 0,
  nox = 0.5,
  rm = 6,
  age = 38,
  dis = 2,
  tax = 296,
  ptratio = 15.3
)

# Make the prediction based on the model
predicted_value <- predict(lm_model, new_property)
predicted_value


# Perform Shapiro-Wilk normality test on the residuals
shapiro_test <- shapiro.test(residuals(lm_model))
shapiro_test


)

predicted_value <- predict(lm_model, new_property)
predicted_value


