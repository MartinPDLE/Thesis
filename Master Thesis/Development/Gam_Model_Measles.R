setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/Data")
library(surveillance)
library(dplyr)
library(mgcv)

Bauchi <- read.csv("Bauchi_Complete.csv")


library(mgcv)
form <- as.formula("Cases ~ s(Cases01)+s(temperature01)+s(mm17)+s(avg_hum13)")

training <- Bauchi[1:261,]

attach(training)
mod.train <- mgcv::gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)
concurvity(mod.train)

par(mfrow=c(1,1))
p <- fitted.values(mod.train)
plot(training$Cases, type="l", ylab="Measles Cases", xlab="week",
     main="Observed vs. Fitted Measles Cases (Cases Only)")
points(predict(mod.train, type="response"),type="l", col="red")

sqrt(mean((training$Cases-p)^2,na.rm=T))
sqrt(mean((training$Cases-p)^2,na.rm=T))/sqrt(mean((training$Cases)^2))
detach(training)

f.total <- Bauchi
### testing
attach(f.total)
preddata <- data.frame(Cases,Cases01,temperature01,mm17,avg_hum13)
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))
pred <-predict(mod.train, type="response", newdata=as.data.frame(preddata),se.fit = T)
#####
preddata <- transform(preddata, fitted = pred$fit)
# 95% confidence interval
preddata <- transform(preddata, 
                      upper = fitted + (1.96 * pred$se.fit),
                      lower = fitted - (1.96 * pred$se.fit))

#tr <- subset(preddata,year<2011)
#pr <- subset(preddata,year>2010)
#######
p <- f.total$predict
train <- f.total[1:261,]
pred <- f.total[262:313,]
preddata <- preddata[262:313,]
#predcase <- pred%>%
#  select(Cases)
#preddat <- merge(preddata, predcase, by="row.names")

par(mfrow=c(1,1))
plot(f.total$Cases, type="l", ylab="Measles Cases", xlab="week",
     main="Observed vs. Predicted Measles Cases")
points(train$p,type="l", col="red")
points(262:313,preddata$fitted,type="l", col="blue")
points(262:313,preddata$upper, type="l", col="grey")
points(262:313,preddata$lower,type="l", col="grey")
abline(h=60, col = "gray60")

#for training data
sqrt(mean((train$Cases-train$p)^2,na.rm=T))/sqrt(mean((train$Cases)^2))
#for validation data 2011-2013
sqrt(mean((pred$Cases-pred$p)^2,na.rm=T))
sqrt(mean((pred$Cases-pred$p)^2,na.rm=T))/sqrt(mean((pred$Cases)^2))
#
preddata <- preddata%>%
  mutate(CI_cov2 = between(Cases01,lower,upper))
summary(preddata$CI_cov2)
38/52
