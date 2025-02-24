#Reading the data 
SalesAD<-read.table("C:/Users/tob5/Box Sync/Documents/ADMN 950/Set 6/SalesAd.csv", header=TRUE, sep=",")
attach(SalesAD)

#Scatter Plot 
plot(AdBudget, Sales, col="steelblue")

#Running a regression model 
slrfit<-lm(Sales~AdBudget)
summary(slrfit)

#Computing the t-critical at alpha=0.05
qt(0.975, 298)

#Plotting the fit 
plot(AdBudget, Sales, col="lightgray")
abline(slrfit, col="red")

#Obtaining the confidence intervals for the coefficients 
confint(slrfit, level=0.90)
confint(slrfit)
confint(slrfit, level=0.99)

#Predicting 
predict(slrfit, data.frame(AdBudget=c(0,1,2)), interval="confidence")
predict(slrfit, data.frame(AdBudget=c(0,1,2)), interval="prediction")

predict(slrfit, data.frame(AdBudget=c(10,100)), interval="confidence")
predict(slrfit, data.frame(AdBudget=c(10,100)), interval="prediction")


#Plotting the confidence and prediction intervals
predint =  predict(slrfit,interval="prediction")
confint =  predict(slrfit,interval="confidence")
predlower = predint[,2]
predupper = predint[,3]
conflower = confint[,2]
confupper = confint[,3]

plot(AdBudget, Sales, ylim=c(10,40), col="lightgray")
abline(slrfit, col="red",lwd=2)
lines(AdBudget,predlower,lwd=2,col="blue")
lines(AdBudget,predupper,lwd=2,col="blue")
lines(AdBudget,conflower,lwd=1,col="green")
lines(AdBudget,confupper,lwd=1,col="green")











