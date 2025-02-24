historicaldata<-read.table("C:/Users/talya/Downloads/mba project/950 mba r/set 7/demand.txt", header = TRUE)
attach(historicaldata)                           
head(historicaldata)                           
correlation_matrix <- cor(historicaldata)
print(correlation_matrix)
# Creating a subset of the dataset (combining the variables of interest)
subset <- cbind(historicaldata$demand, historicaldata$market, historicaldata$price, historicaldata$brand)

# Generate a scatter plot matrix for all variables in the subset
pairs(subset)

# Calculate and print the correlation matrix (excluding any NAs)
cor(na.omit(subset))

# Simple Linear Regression with 'market' as independent variable and 'demand' as dependent variable
slrfit1 <- lm(demand ~ market, data = data.frame(na.omit(historicaldata)))
summary(slrfit1)

# Simple Linear Regression with 'price' as independent variable and 'demand' as dependent variable
slrfit2 <- lm(demand ~ price, data = data.frame(na.omit(historicaldata)))
summary(slrfit2)

# Simple Linear Regression with 'brand' as independent variable and 'demand' as dependent variable
slrfit3 <- lm(demand ~ brand, data = data.frame(na.omit(historicaldata)))
summary(slrfit3)

# Fit the multiple linear regression model
mlrfit <- lm(demand ~ market + price + brand, data = data.frame(na.omit(historicaldata)))

# Display the summary of the model to view the results
summary(mlrfit)

predict(mlrfit, data.frame(market = 0.50, price = 100, brand = 0.9), interval = "prediction", level = 0.95)

predict(mlrfit, data.frame(market = 0.50, price = 100, brand = 0.9), interval = "prediction", level = 0.99)

# Run regression for market as the dependent variable
slrfit1 <- lm(market ~ price + brand, data = data.frame(na.omit(historicaldata)))
summary(slrfit1)

# Run regression for price as the dependent variable
slrfit2 <- lm(price ~ market + brand, data = data.frame(na.omit(historicaldata)))
summary(slrfit2)

# Run regression for brand as the dependent variable
slrfit3 <- lm(brand ~ market + price, data = data.frame(na.omit(historicaldata)))
summary(slrfit3)

# Run the multiple linear regression model using market, price, and brand as independent variables
mlrfit <- lm(demand ~ market + price + brand, data = data.frame(na.omit(historicaldata)))

# Extract the residuals from the model
sres <- rstandard(mlrfit)

# Perform the Shapiro-Wilk normality test on the residuals
shapiro.test(sres)

# Run the regression model
mlrfit <- lm(demand ~ market + price + brand, data = data.frame(na.omit(historicaldata)))

# Calculate the standardized residuals
sres <- rstandard(mlrfit)

# Square the residuals
sqres <- sres^2

# Square the independent variables
sqmarket <- market^2
sqprice <- price^2
sqbrand <- brand^2

# Run the regression model to check for constant variance (White's test)
white <- lm(sqres ~ market + price + brand + sqmarket + sqprice + sqbrand + market:price + market:brand + price:brand)

# Summary of the model
summary(white)


# Test Statistic Calculation for White's Test

# Length of the residuals
n = length(sqres)

# R-squared value from the White's test model
rsq = summary(white)$r.squared

# Compute the Chi-squared test statistic
chitest = n * rsq

# Print the test statistic
chitest

# Degrees of freedom (df) = 9
df = 9

# Critical Chi-Squared value at the 95% confidence level
critical_chi_squared = qchisq(0.95, df)

# Print the critical chi-squared value
critical_chi_squared

par(mfrow=c(1,4))
plot(mlrfit)





