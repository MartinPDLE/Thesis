setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")
library(surveillance)
library(dplyr)

campyDE <- read.csv("campyDE_2.csv")
campyDE$Date <- as.Date(campyDE$date)

#set cutoff date
cutoff <- as.Date("2010-01-01")


library(mgcv)
form <- as.formula("case ~ s(case1)+s(l4.hum)")

training <- campyDE[campyDE$Date < cutoff,]

attach(training)
mod.train <- mgcv::gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)
concurvity(mod.train)

##Plot Fit
par(mfrow=c(1,1))
p <- fitted.values(mod.train)
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/GAM Fits/Final/Campy Met.png', width=2000, height=1400, res=300,pointsize = 10)
plot(training$Date,training$case1, type="l",ylab="Campylobacteriosis Cases", xlab="time",
     main="Meteorological Model",lwd=1.5,cex.lab=1.2)
points(training$Date,predict(mod.train, type="response"),type="l", col="red",lwd=1.5)
abline(v=seq(as.Date("2002-01-01"), by="+1 year", length.out=9),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2002-01-01"), by="+1 year", length.out=9),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("fitted values","obs.values"),
       col = c(
         "red",
         "black"), pch = c(15, 15), bty = "n",cex = 1.2)
dev.off()

#RMSE and SRMSE for training data 
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
train <- f.total[f.total$Date <= cutoff,]
pred <- f.total[f.total$Date >= cutoff,]
preddata <- preddata[f.total$Date >= cutoff,]


#### Plot Predictions
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/Gam Campy.png', width=2000, height=1400, res=300,pointsize = 10)
plot(pred$date,pred$case, type="l",ylab="Camplylobacteriosis Cases", xlab="time",lwd=1.5,ylim = c(0,max(preddata$upper)), cex.lab=1.2)
polygon(x=c(pred$date,rev(pred$date)),y=c(preddata$upper,rev(preddata$lower)),col = rgb(0, 0, 1, alpha = 0.1), border = rgb(0, 0, 1, alpha = 0.3))
points(pred$date,pred$predict,type="l", col="blue",lwd=1.5)
abline(v=seq(as.Date("2010-01-01"), by="+6 month", length.out=5),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2010-01-01"), by="+6 month", length.out=5),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("95% PI", "prediction","obs.values"),
       col = c( rgb(0, 0, 1, alpha = 0.1),
                "blue",
                "black"), pch = c(15, 15, 15), bty = "n",cex = 1.2)
dev.off()


#RMSE and SRMSE for validation data 2011-2013
sqrt(mean((pred$case-pred$p)^2,na.rm=T))
sqrt(mean((pred$case-pred$p)^2,na.rm=T))/sqrt(mean((pred$case)^2))

#PI coverage
preddata <- preddata%>%
  mutate(CI_cov = between(case,lower,upper))
summary(preddata$CI_cov)


