stocks<-read.table("C:/Users/talya/Downloads/mba project/950 mba r/set 3/stockdata.csv", header=TRUE, sep=",")
attach(stocks)
head(stocks)
mean(s1)
mean(s2)
sd(s1)
sd(s2)



#P(s1>.12)
1-pnorm(.12,mean(s1), sd(s1))

#P(s2>.12)
1-pnorm(.12,mean(s2), sd(s2))

#P(s1<0)
pnorm(0,mean(s1), sd(s1))

#P(s2<0)
pnorm(0,mean(s2), sd(s2))




qqnorm(s1, pch = 1, frame = FALSE)
qqline(s1, col = "steelblue", lwd = 2)

qqnorm(s2, pch = 1, frame = FALSE)
qqline(s2, col = "steelblue", lwd = 2)

#Total change example 
#P(T<0)
pnorm(0,0,sqrt(30))

#P(T>5)
1-pnorm(5,0,sqrt(30))
