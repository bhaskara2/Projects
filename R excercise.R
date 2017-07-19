##Chapter7 & Excercises

NLData=load("C:/Users/Bhaskara/Desktop/SS/Chapter 7/7.R.RData")
str(NLData)
View(NLData)
summary(NLData)
plot(x,y)
fit=glm(y ~ x, data=NLData, family=binomial)
summary(fit)


##ISLR Question 6
require(ISLR)
attach(Wage)


err.fit=rep(NA,10)

for(i in 1:10)
{
  polfit <- glm(wage ~ poly(age, i), data = wage)
  err.fit[i]=cv.glm(wage,polyfit,k=10)$delta[1]
  
}

##Summary of Non-Linear Models : https://rpubs.com/ryankelly/GAMs

library(ISLR)
library(boot)
set.seed(1)
deltas <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  deltas[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(1:10,deltas,type="b",col="red")



##ISLR Questoin 7
##marit1 & Job class are factor variables.
summary(Wage)
library(gam)
fit0 <- gam(wage ~ s(age, 5) + education, data = Wage)
fit1 <- gam(wage ~ s(age, 5) + education + jobclass, data = Wage)
fit2 <- gam(wage ~ s(age, 5) + education + maritl, data = Wage)
fit3 <- gam(wage ~ s(age, 5) + education + jobclass + maritl, data = Wage)
anova(fit0, fit1, fit2, fit3)


