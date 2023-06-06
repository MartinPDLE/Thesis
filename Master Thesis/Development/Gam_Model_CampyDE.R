setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/Data")
library(surveillance)
library(dplyr)

data("campyDE")

campyDE <- campyDE%>%
  mutate(case1 = lag(case, n=1))
campyDE <- campyDE%>%
  mutate(case2 = lag(case, n=2))


library(mgcv)
form <- as.formula("case ~ s(case1,k=4)")

training <- campyDE[1:407,]

attach(training)
mod.train <- mgcv::gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)
concurvity(mod.train)

par(mfrow=c(1,1))
p <- fitted.values(mod.train)
plot(training$case, type="l")
points(predict(mod.train, type="response"),type="l", col="red")

sqrt(mean((training$case-p)^2,na.rm=T))
sqrt(mean((training$case-p)^2,na.rm=T))/sqrt(mean((training$case)^2))
detach(training)

f.total <- campyDE
### testing
attach(f.total)
preddata <- data.frame(case1)
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))

p <- f.total$predict
train <- f.total[1:407,]
pred <- f.total[408:522,]

par(mfrow=c(1,1))
plot(f.total$case, type="l")
points(train$p,type="l", col="red")
points(408:522,pred$p,type="l", col="blue")
abline(h=60, col = "gray60")

#for training data
sqrt(mean((train$case-train$p)^2,na.rm=T))/sqrt(mean((train$case)^2))
#for validation data 2011-2013
sqrt(mean((pred$case-pred$p)^2,na.rm=T))/sqrt(mean((pred$case)^2))

