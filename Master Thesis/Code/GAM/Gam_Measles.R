setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")
library(surveillance)
library(dplyr)
library(mgcv)

Bauchi <- read.csv("Bauchi_Complete.csv")
Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)
Bauchi$Date <- as.Date(Bauchi$Date)

#set cutoff date
cutoff_l <- as.Date("2013-01-01")
cutoff <- as.Date("2017-01-01")

library(mgcv)
form <- as.formula("Cases ~ s(Cases01)+s(temperature01) + s(mm17) + s(avg_hum13)")

training <- Bauchi[Bauchi$Date >= cutoff_l & Bauchi$Date < cutoff,]

attach(training)
mod.train <- mgcv::gam(form, family=quasipoisson, na.action=na.exclude, data=training)
summary(mod.train)
concurvity(mod.train)

par(mfrow=c(1,1))
p <- fitted.values(mod.train)
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/GAM Fits/Final/Measles Case.png', width=2000, height=1400, res=300,pointsize = 10)
plot(training$Date,training$Cases, type="l",ylab="Measles Cases", xlab="time",
     main="Cases-only Model",lwd=1.5,cex.lab=1.2)
points(training$Date,predict(mod.train, type="response"),type="l", col="red",lwd=1.5)
abline(v=seq(as.Date("2013-01-01"), by="+1 year", length.out=5),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2013-01-01"), by="+1 year", length.out=5),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("fitted values","obs.values"),
       col = c(
         "red",
         "black"), pch = c(15, 15), bty = "n",cex = 1.2)
dev.off()

#RMSE and SRMSE for training data 
sqrt(mean((training$Cases-p)^2,na.rm=T))
sqrt(mean((training$Cases-p)^2,na.rm=T))/sqrt(mean((training$Cases)^2))
detach(training)

f.total <- Bauchi[Bauchi$Date >= cutoff_l,]
### testing
attach(f.total)
preddata <- data.frame(Cases,Cases01,temperature01,mm17,avg_hum13)
start_time <- Sys.time()
f.total$predict<-predict(mod.train, type="response", newdata=as.data.frame(preddata))
end_time <- Sys.time()
end_time - start_time
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

train <- f.total[Bauchi$Date < cutoff,]
pred <- f.total[f.total$Date >= cutoff,]
preddata <- preddata[f.total$Date >= cutoff,]


### Plot predictions
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/Gam Measles.png', width=2000, height=1400, res=300,pointsize = 10)
plot(pred$Date,pred$Cases, type="l",ylab="Measles Cases", xlab="time",lwd=1.5,ylim = c(0,max(preddata$upper)), cex.lab=1.2)
polygon(x=c(pred$Date,rev(pred$Date)),y=c(preddata$upper,rev(preddata$lower)),col = rgb(0, 0, 1, alpha = 0.1), border = rgb(0, 0, 1, alpha = 0.3))
points(pred$Date,pred$predict,type="l",col="blue", lwd=1.5)
abline(v=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("95% PI", "prediction","obs.values"),
       col = c( rgb(0, 0, 1, alpha = 0.1),
                "blue",
                "black"), pch = c(15, 15, 15), bty = "n",cex = 1.2)
dev.off()




#RMSE and SRMSE for validation data 
sqrt(mean((pred$Cases-pred$p)^2,na.rm=T))
sqrt(mean((pred$Cases-pred$p)^2,na.rm=T))/sqrt(mean((pred$Cases)^2))

#PI Coverage
preddata <- preddata%>%
  mutate(CI_cov2 = between(Cases01,lower,upper))
summary(preddata$CI_cov2)

#PI NAW
1/(max(pred$Cases)-min(pred$Cases))*mean(preddata$upper-preddata$lower)

