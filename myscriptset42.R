Retail2 <- read.csv("C:/Users/talya/Downloads/mba project/950 mba r/set 4/assignment 4/Retail2.csv", header=TRUE, sep=",")
attach(Retail2)
head(Retail2)

t.test(After,Before,alternative="greater",paired=TRUE)

qt(.05,14)



Meeting<-read.table("C:/Users/talya/Downloads/mba project/950 mba r/set 4/assignment 4/Meeting.csv", header=TRUE, sep=",")
attach(Meeting)
head(Meeting)

var.test(Onsite,Hotel, alternative="two.sided")

t.test(Onsite, Hotel, var.equal = TRUE)
