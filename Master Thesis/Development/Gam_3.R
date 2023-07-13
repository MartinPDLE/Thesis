setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")
Iquitos <- read.csv("../Data/Iquitos_total2.csv")

library(mgcv)
form <- as.formula("total_cases ~s(total_cases1)+s(total_cases2)+s(total_cases3)+
                   s(relative_humidity1)+s(relative_humidity2)+s(relative_humidity3)+
                   s(DTR1)+s(DTR2)+s(DTR3)+s(precipitation_amount1)+s(precipitation_amount2)+s(precipitation_amount3)")
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
preddata <- data.frame(total_cases,specific_humidity1,specific_humidity2,specific_humidity3,total_cases1,total_cases2,total_cases3,
                       precipitation_amount1,precipitation_amount2,precipitation_amount3,relative_humidity1,relative_humidity2,relative_humidity3,
                       DTR1,DTR2,DTR3)
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
plot(365:468,pred$total_cases, type="l",ylab="Dengue Cases", xlab="week",
     main="Observed vs. Predicted Dengue Cases",lwd=1.5,ylim = c(0,max(preddata$upper)))
polygon(x=c(365:468,rev(365:468)),y=c(preddata$upper,rev(preddata$lower)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 0.1))
points(365:468,pred$predict,type="l", col="white",lwd=1.5)
#points(365:468,preddata$lower,type="l", col="grey")

#for training data
sqrt(mean((train$total_cases-train$predict)^2,na.rm=T))/sqrt(mean((train$total_cases)^2))
#for validation data 2011-2013
sqrt(mean((pred$total_cases-pred$predict)^2,na.rm=T))
sqrt(mean((pred$total_cases-pred$predict)^2,na.rm=T))/sqrt(mean((pred$total_cases)^2))

preddata <- preddata%>%
  mutate(CI_cov = between(total_cases,lower,upper))
summary(preddata$CI_cov)
25/104
