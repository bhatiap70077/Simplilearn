h_c<-read.csv("C:/Users/bhati/Desktop/SimpliLearn/Data Science with R/projects to solve/Healthcare/Healthcare/HospitalCosts.csv")

summary(h_c)

hist(h_c$AGE, main = "Histogram for age frequency", xlab = "Age Group", ylab = "Frequency of Patients", prob= TRUE, col = "red")
lines(density(h_c$AGE))
summary(as.factor(h_c$AGE))
x <- aggregate(TOTCHG~AGE,FUN = sum,data = h_c)
x
max(x)


which.max(summary(as.factor(h_c$APRDRG)))

diagnosiscost <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = h_c)
diagnosiscost
diagnosiscost[which.max(diagnosiscost$TOTCHG),]



summary(as.factor(h_c$RACE))
head(h_c)
h_c<-na.omit(h_c)
h_c$RACE<-as.factor(h_c$RACE)
mod<- aov(TOTCHG ~ RACE, data = h_c)
mod
summary(mod)
summary(h_c$RACE)



model1 <- lm(TOTCHG ~ AGE + FEMALE, data = h_c)
h_c$FEMALE<-as.factor(h_c$FEMALE)
model1 <- lm(TOTCHG ~ AGE + FEMALE, data = h_c)
summary(model1)
summary(h_c$FEMALE)
head(h_c)



h_c$RACE<-as.factor(h_c$RACE)
model2 <- lm(TOTCHG ~ AGE + FEMALE + RACE, data = h_c)
summary(model2)


model3 <- lm(TOTCHG ~ ., data = h_c)
summary(model3)




