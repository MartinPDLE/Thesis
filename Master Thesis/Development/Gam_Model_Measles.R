setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")
library(surveillance)
library(dplyr)
library(mgcv)

Bauchi <- read.csv("Bauchi_Complete.csv")
Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)
Bauchi$Date <- as.Date(Bauchi$Date)


library(mgcv)
form <- as.formula("Cases ~ s(Cases01) + s(temperature01) + s(mm17) + s(avg_hum13)")

training <- Bauchi[53:261,]

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

f.total <- Bauchi[53:313,]
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
train <- f.total[1:209,]
pred <- f.total[210:261,]
preddata <- preddata[210:261,]
timepoints <- 210:261

#predcase <- pred%>%
#  select(Cases)
#preddat <- merge(preddata, predcase, by="row.names")

par(mfrow=c(1,1))
plot(f.total$Cases, type="l", ylab="Measles Cases", xlab="week",
     main="Observed vs. Predicted Measles Cases")
points(train$p,type="l", col="red")
points(timepoints,preddata$fitted,type="l", col="blue")
points(timepoints,preddata$upper, type="l", col="grey")
points(timepoints,preddata$lower,type="l", col="grey")
abline(h=60, col = "gray60")
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
#points(preddata$lower,type="l", col="grey")
###


#for training data
sqrt(mean((train$Cases-train$p)^2,na.rm=T))/sqrt(mean((train$Cases)^2))
#for validation data 2011-2013
sqrt(mean((pred$Cases-pred$p)^2,na.rm=T))
sqrt(mean((pred$Cases-pred$p)^2,na.rm=T))/sqrt(mean((pred$Cases)^2))
#
preddata <- preddata%>%
  mutate(CI_cov2 = between(Cases01,lower,upper))
summary(preddata$CI_cov2)
39/52
