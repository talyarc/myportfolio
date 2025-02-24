# Create the contingency table for the survey data
data <- matrix(c(40, 35, 25, 35, 25, 10, 5, 10, 15), 
               nrow = 3, 
               byrow = TRUE, 
               dimnames = list("Quality of Management" = c("Excellent", "Good", "Fair"), 
                               "Reputation of Company" = c("Excellent", "Good", "Fair")))

# View the table
data
chisq.test(data)

# Calculate the critical value for alpha = 0.01 and df = 4
critical_value <- qchisq(0.99, df = 4)
critical_value

observed <- matrix(c(40, 25, 5, 35, 35, 10, 25, 10, 15), nrow = 3, byrow = TRUE, dimnames = list("Quality of Management" = c("Excellent", "Good", "Fair"), 
                                                                                                 "Reputation of Company" = c("Excellent", "Good", "Fair")))
> test <- chisq.test(observed)
> test_statistic <- test$statistic
> p_value <- test$p.value
> critical_value <- qchisq(1 - 0.01, df = 4)
> list(
  +   test_statistic = test_statistic,
  +   p_value = p_value,
  +   critical_value = critical_value
  
