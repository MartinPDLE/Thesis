setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")
Iquitos <- read.csv("Iquitos_total2.csv")

predI <- Iquitos[365:468,]
predI$Date <- mdy(predI$week_start_date)

png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/Naive Dengue.png', width=2000, height=1400, res=300,pointsize = 10)
plot(predI$Date, predI$total_cases, type="l",ylab="Dengue Cases", xlab="time", cex.lab=1.2)
points(predI$Date, predI$total_cases1,type="l", col="blue",lwd=1.5)
abline(v=seq(as.Date("2007-07-01"), by="+6 month", length.out=5),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2007-07-01"), by="+6 month", length.out=5),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("prediction","obs.values"),
       col = c(
         "blue",
         "black"), pch = c( 15, 15), bty = "n",cex = 1.2)
dev.off()


sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))
sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))/sqrt(mean((predI$total_cases)^2))


campyDE <- read.csv("campyDE.csv")
campyDE <- campyDE%>%
  mutate(case1 = lag(case, n=1))

predC <- campyDE[419:522,]
predC$date <- as.Date(predC$date)

png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/Naive Campy.png', width=2000, height=1400, res=300,pointsize = 10)
plot(x=predC$date,y=predC$case, type="l",ylab="Campylobacteriosis Cases", xlab="time", cex.lab=1.2)
points(predC$date,predC$case1,type="l", col="blue",lwd=1.5)
abline(v=seq(as.Date("2010-01-01"), by="+6 month", length.out=5),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2010-01-01"), by="+6 month", length.out=5),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("prediction","obs.values"),
       col = c(
                "blue",
                "black"), pch = c( 15, 15), bty = "n",cex = 1.2)
dev.off()

sqrt(mean((predC$case-predC$case1)^2,na.rm=T))
sqrt(mean((predC$case-predC$case1)^2,na.rm=T))/sqrt(mean((predC$case)^2))

Bauchi <- read.csv("Bauchi_Complete.csv")
Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)

predB <- Bauchi[262:313,]

png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/Naive Measles.png', width=2000, height=1400, res=300,pointsize = 10)
plot(predB$Date,predB$Cases, type="l",ylab="Measles Cases", xlab="time", cex.lab=1.2)
points(predB$Date,predB$Cases01,type="l", col="blue",lwd=1.5)
abline(v=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("prediction","obs.values"),
       col = c(
         "blue",
         "black"), pch = c( 15, 15), bty = "n",cex = 1.2)
dev.off()


sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))
sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))/sqrt(mean((predB$Cases)^2))
