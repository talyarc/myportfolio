# Load the data
marketdata <- read.table("C:/Users/talya/Downloads/mba project/950 mba r/final/marketshare.csv", header = TRUE, sep = ",")

# View the first few rows to ensure the data is loaded correctly
head(marketdata)

# Fit the linear regression model (Market Share as a function of Product Quality)
model <- lm(Marketshare ~ Productquality, data = marketdata)
n <- nrow(marketdata)
df <- n - 2

# Critical value for a one-tailed test with alpha = 0.01 and degrees of freedom df
critical_value <- qt(1 - 0.01, df)

# Output the critical value
critical_value

p_value <- summary(model)$coefficients[2, 4]
p_value

# Fit the linear regression model
lm_fit <- lm(Marketshare ~ Productquality, data = marketdata)

# Predict the market share when Productquality is 55
market_share_estimate <- predict(lm_fit, newdata = data.frame(Productquality = 55))

# Show the predicted market share
market_share_estimate

# Fit the linear regression model
lm_fit <- lm(Marketshare ~ Productquality, data = marketdata)

# Obtain a 99% prediction interval for Marketshare when Productquality is 55
prediction_interval <- predict(lm_fit, newdata = data.frame(Productquality = 55), 
                               interval = "prediction", level = 0.99)

# Show the prediction interval
prediction_interval

prediction_interval <- predict(lm_fit, newdata = data.frame(Productquality = 55), 
                               interval = "prediction", level = 0.99)
# Calculate the correlation
correlation <- cor(marketdata$Marketshare, marketdata$Productquality)
correlation


