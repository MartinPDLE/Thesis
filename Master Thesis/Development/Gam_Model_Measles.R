setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/Data")
library(surveillance)
library(dplyr)

Bauchi <- read.csv("Bauchi_Complete.csv")


library(mgcv)
form <- as.formula("Cases ~ s(Cases01,k=4)+s(temperature01,k=4)+s(mm17,k=4)+s(avg_hum17,k=4)")

training <- Bauchi[1:261,]

attach(training)
mod.train <- mgcv::gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)
concurvity(mod.train)

  par(mfrow=c(1,1))
p <- fitted.values(mod.train)
plot(training$Cases, type="l")
points(predict(mod.train, type="response"),type="l", col="red")

sqrt(mean((training$Cases-p)^2,na.rm=T))
sqrt(mean((training$Cases-p)^2,na.rm=T))/sqrt(mean((training$Cases)^2))
detach(training)

f.total <- Bauchi
### testing
attach(f.total)
preddata <- data.frame(temperature03,mm17)
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))

p <- f.total$predict
train <- f.total[1:261,]
pred <- f.total[262:313,]

par(mfrow=c(1,1))
plot(f.total$Cases, type="l")
points(train$p,type="l", col="red")
points(262:313,pred$p,type="l", col="blue")
abline(h=60, col = "gray60")

#for training data
sqrt(mean((train$Cases-train$p)^2,na.rm=T))/sqrt(mean((train$Cases)^2))
#for validation data 2011-2013
sqrt(mean((pred$Cases-pred$p)^2,na.rm=T))/sqrt(mean((pred$Cases)^2))

