#Reading the data 
shop<-read.table("C:/Users/tob5/Box Sync/Documents/ADMN 950/Set 5/shopping.csv", header=TRUE, sep=",")
attach(shop)

#Obtaining the boxplot
boxplot(Trips~ID)

#ANOVA 
anova<-aov(Trips~as.factor(ID))
summary(anova)

#F-critical at alpha=0.01
qf(.99,3 ,412)
----------------------------------------------------------------------------------------------------------------------

#Test of proportions 
ads<-read.table("C:/Users/tob5/Box Sync/Documents/ADMN 950/Set 5/tvads.csv", header=TRUE, sep=",")
attach(ads)

#Testing if the sample size is large enough 
n1=100
n2=150
p1bar=mean(MrRobot,na.rm=TRUE)
p2bar=mean(Firefly,na.rm=TRUE)

(n1>5/min(p1bar, (1-p1bar)))
(n2>5/min(p2bar, (1-p2bar)))

#Running the test of proportions
data <- c( sum(MrRobot ,na.rm=TRUE), sum(Firefly,na.rm=TRUE) )
size<-c(100, 150)
prop.test(data, size, alternative="greater", correct=FALSE)
----------------------------------------------------------------------------------------------------------------------

#Running the test of proportions for 3 or more populations
data <- c( 15, 14, 45, 38 )
size<-c(150, 100, 250, 200)
prop.test(data, size, correct=FALSE)

#Critical Chi-squared at 0.10 significance level 
qchisq(0.90, 3)
----------------------------------------------------------------------------------------------------------------------

#Test of independence between 2 categorical variables 
data=matrix(c(55,45,50,40,30,40,45,55,55,85,45,20,45,55,35),nrow=3,ncol=5)
colnames(data)<-c("light","wheat","regular","IPA","dark")
rownames(data)<-c("East","Mid","West")

chisq.test(data)

#Critical Chi-squared at 0.01 significance level 
qchisq(0.99, 8)






