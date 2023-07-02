setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/Data")
Iquitos <- read.csv("../Data/Iquitos_total2.csv")

library(mgcv)
form <- as.formula("total_cases ~s(total_cases1)+s(relative_humidity3)+s(DTR3)+s(precipitation_amount3)")
#form <- as.formula("total_cases ~ s(total_cases,k=4)+s(total_cases1,k=4)+s(precipitation_amount5,k=4)+s(specific_humidity5,k=4)")

training <- Iquitos[1:364,]

attach(training)
mod.train <- mgcv::gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)
concurvity(mod.train)

par(mfrow=c(1,1))
p <- fitted.values(mod.train)
plot(training$total_cases, type="l",ylab="Dengue Cases", xlab="week",
     main="Observed vs. Fitted Dengue Cases (Meteorological Model)")
points(predict(mod.train, type="response"),type="l", col="red")

sqrt(mean((training$total_cases-p)^2,na.rm=T))
sqrt(mean((training$total_cases-p)^2,na.rm=T))/sqrt(mean((training$total_cases)^2))
detach(training)

f.total <- Iquitos
### testing
attach(f.total)
preddata <- data.frame(total_cases,specific_humidity3,total_cases1,precipitation_amount3,relative_humidity3,DTR3)
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))
pred <-predict(mod.train, type="response", newdata=as.data.frame(preddata),se.fit = T)
#####
preddata <- transform(preddata, fitted = pred$fit)
# 95% confidence interval
preddata <- transform(preddata, 
                      upper = fitted + (1.96 * pred$se.fit),
                      lower = fitted - (1.96 * pred$se.fit))

p <- f.total$predict
train <- f.total[1:364,]
pred <- f.total[365:468,]
preddata <- preddata[365:468,]

par(mfrow=c(1,1))
plot(f.total$total_cases, type="l",ylab="Dengue Cases", xlab="week",
     main="Observed vs. Predicted Dengue Cases")
points(train$predict,type="l", col="red")
points(365:468,pred$predict,type="l", col="blue")
points(365:468,preddata$upper, type="l", col="grey")
points(365:468,preddata$lower,type="l", col="grey")
abline(h=60, col = "gray60")
### Plot predictions
plot(365:468,pred$total_cases, type="l",ylab="Dengue Cases", xlab="week", ylim=c(0,max(pred$predict)),
     main="Observed vs. Predicted Dengue Cases")
points(365:468,pred$predict,type="l", col="blue")
points(365:468,preddata$upper, type="l", col="grey")
points(365:468,preddata$lower,type="l", col="grey")

#for training data
sqrt(mean((train$total_cases-train$predict)^2,na.rm=T))/sqrt(mean((train$total_cases)^2))
#for validation data 2011-2013
sqrt(mean((pred$total_cases-pred$predict)^2,na.rm=T))
sqrt(mean((pred$total_cases-pred$predict)^2,na.rm=T))/sqrt(mean((pred$total_cases)^2))

preddata <- preddata%>%
  mutate(CI_cov = between(total_cases,lower,upper))
summary(preddata$CI_cov)
25/104
