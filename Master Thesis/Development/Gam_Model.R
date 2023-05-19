setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/Data")
Iquitos <- read.csv("Iquitos.csv")

library(mgcv)
form <- as.formula("total_cases ~ s(mm3,k=4)+s(total_cases2,k=4)")

training <- Iquitos[1:350,]

attach(training)
mod.train <- gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)

par(mfrow=c(1,1))
p <- fitted.values(mod.train)
plot(training$total_cases, type="l")
points(predict(mod.train, type="response"),type="l", col="red")

sqrt(mean((training$total_cases-p)^2,na.rm=T))
sqrt(mean((training$total_cases-p)^2,na.rm=T))/sqrt(mean((training$total_cases)^2))
detach(training)

f.total <- Iquitos
### testing
attach(f.total)
preddata <- data.frame(mm3,mm2,total_cases2)
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))

p <- f.total$predict
train <- f.total[1:350,]
pred <- f.total[351:468,]

par(mfrow=c(1,1))
plot(f.total$total_cases, type="l")
points(train$p,type="l", col="red")
points(351:468,pred$p,type="l", col="blue")
abline(h=60, col = "gray60")

#for training data
sqrt(mean((train$total_cases-train$p)^2,na.rm=T))/sqrt(mean((train$total_cases)^2))
#for validation data 2011-2013
sqrt(mean((pred$total_cases-pred$p)^2,na.rm=T))/sqrt(mean((pred$total_cases)^2))
