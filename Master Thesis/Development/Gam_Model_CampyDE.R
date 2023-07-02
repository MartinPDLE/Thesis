setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/Data")
library(surveillance)
library(dplyr)

campyDE <- read.csv("campyDE.csv")

campyDE <- campyDE%>%
  mutate(case1 = lag(case, n=1))
campyDE <- campyDE%>%
  mutate(case2 = lag(case, n=2))
campyDE <- campyDE%>%
  mutate(case3 = lag(case, n=3))
campyDE <- campyDE%>%
  mutate(case4 = lag(case, n=4))
campyDE <- campyDE%>%
  mutate(case5 = lag(case, n=5))


library(mgcv)
form <- as.formula("case ~ s(case1)+s(l4.hum)")

training <- campyDE[1:418,]

attach(training)
mod.train <- mgcv::gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)
concurvity(mod.train)

par(mfrow=c(1,1))
p <- fitted.values(mod.train)
plot(training$case, type="l",ylab="Camplylobacteriosis Cases", xlab="week",
     main="Observed vs. Fitted Campylobacteriosis Cases (Meteorological Model)")
points(predict(mod.train, type="response"),type="l", col="red")

sqrt(mean((training$case-p)^2,na.rm=T))
sqrt(mean((training$case-p)^2,na.rm=T))/sqrt(mean((training$case)^2))
detach(training)

f.total <- campyDE
### testing
attach(f.total)
preddata <- data.frame(case,case1,l4.hum)
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))
pred <-predict(mod.train, type="response", newdata=as.data.frame(preddata),se.fit = T)
#####
preddata <- transform(preddata, fitted = pred$fit)
# 95% confidence interval
preddata <- transform(preddata, 
                      upper = fitted + (1.96 * pred$se.fit),
                      lower = fitted - (1.96 * pred$se.fit))

p <- f.total$predict
train <- f.total[1:418,]
pred <- f.total[419:522,]
preddata <- preddata[419:522,]

par(mfrow=c(1,1))
plot(f.total$case, type="l", ylab="Camplylobacteriosis Cases", xlab="week",
     main="Observed vs. Predicted Campylobacteriosis Cases")
points(train$p,type="l", col="red")
points(419:522,pred$p,type="l", col="blue")
points(419:522,preddata$upper, type="l", col="grey")
points(419:522,preddata$lower,type="l", col="grey")
abline(h=60, col = "gray60")
#### Plot Predictions
plot(419:522,pred$case, type="l",ylab="Camplylobacteriosis Cases", xlab="week", ylim=c(0,max(pred$predict)),
     main="Observed vs. Predicted Camplylobacteriosis Cases")
points(419:522,pred$predict,type="l", col="blue")
points(419:522,preddata$upper, type="l", col="grey")
points(419:522,preddata$lower,type="l", col="grey")

#for training data
sqrt(mean((train$case-train$p)^2,na.rm=T))/sqrt(mean((train$case)^2))
#for validation data 2011-2013
sqrt(mean((pred$case-pred$p)^2,na.rm=T))
sqrt(mean((pred$case-pred$p)^2,na.rm=T))/sqrt(mean((pred$case)^2))

preddata <- preddata%>%
  mutate(CI_cov = between(case,lower,upper))
summary(preddata$CI_cov)
30/105

