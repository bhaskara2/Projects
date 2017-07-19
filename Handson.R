require(ISLR)
require(boot)

hn = load("C:/Users/Bhaskara/Desktop/SS/Chapter 5/5.R.RData")
summary(hn)#


fit=lm(y~X1+X2 ,data=Xy)
summary(fit)
matplot(Xy,type="l")


##Lets write a simple function to use formula (5.2)
loocv=function(fit)
{
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

##ISLR Sampling 5 hands on
##cross
library (ISLR)
set.sedd(1)
train=sample (392,196,replace = T)
lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train )
attach(Auto)
mean((mpg-predict(lm.fit,Auto)) [-train]^2)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)

##LOOCV
attach(Auto)
glm.fit = glm(mpg~horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta #gives the base error and biased error

#let's compare the loocv error estimate for diferent degrees
loocvfn = function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

loocvfn(glm.fit)

cv.error = rep(0,5)
degree = c(1:5);degree
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error[d] = loocvfn(glm.fit)
  
}

plot(degree,cv.error,type="b")

#k=10 folds
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


#bootstrap
#ISLR book exercises
x <- 1:100000
y=1 - ((1 - 1/x)^x) #when n=10000, the probability that jth observation is in bootstrap sample.
plot(x, y) #

##Applied exercises

attach(Default)
set.seed(1)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)
#Divide the data in to 2 parts of equal size.
library(dplyr)
train<-sample_frac(Default, 0.5)
sid<-as.numeric(rownames(train)) # every data frame has rownames(), which has the number of rows in data frame & returns character
test<-Default[-sid,]
##use training data to fit the model
tfit.glm <- glm(default ~ income + balance, data = train, family = "binomial")
summary(tfit.glm)
##validate the model
probs <- predict(fit.glm, newdata = test, type = "response")
pred.glm <- rep("No", length(probs)) #create an empty vector
pred.glm[probs > 0.5] <- "Yes" #whenever the probabilities in the probs vector greater than 0.5, an yes is stored to "pred.glm".
#mis classification rate.
mean(pred.glm != Default[test,]$default)

##use Bootstrap to calculate the calculate SD of coefficient estimates.
library(boot)
set.seed(1)

bglm.fit=glm(default~income+balance,data=Default,family="binomial")
summary(bglm.fit)

boot.fn=function(data,index){
  x=glm(default~income+balance,data=Default,family="binomial",subset=index)
  return(coef(x))
}
boot(Default,boot.fn,1000)#1000 bootstrap samples are generated, each time a different set of indices are passed to the "boot.fn" function.
#for each different indices, different estimates are caculated, and avearge of them is calculated. "Refer to notes", standard bootstrap formula.



