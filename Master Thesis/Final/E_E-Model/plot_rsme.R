setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/EE-Model/dengue")
forecasts_dengue_geom <- read.csv("forecasts/forecasts_dengue_geom_Iq4.csv")
forecasts_campy_geom <- read.csv("forecasts/forecasts_dengue_geom_ca3.csv")
forecasts_measles_geom <- read.csv("forecasts/forecasts_dengue_geom_mb.csv")

Iquitos<-read.csv("../../Data/Iquitos_total2.csv")
Iquitos <- Iquitos[365:467,]
Iquitos <- Iquitos%>%
  select(week_start_date)
Iquitos <- Iquitos%>%
  mutate(prediction_time=row_number()+364)
#####
Bauchi<-read.csv("../../Data/Measles_Bauchi.csv")
Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)
Bauchi$Date <- as.Date(Bauchi$Date)
Bauchi <- Bauchi%>%
  filter(Date >= "2017-01-01")
Bauchi <- Bauchi%>%
  select(Date)
Bauchi <- Bauchi%>%
  mutate(prediction_time=row_number()+207)
######
data("campyDE")
campyDE$date <- as.Date(campyDE$date)
campyDE <- campyDE%>%
  filter(date >= "2010-01-04")
campyDE <- campyDE%>%
  select(date)
campyDE <- campyDE%>%
  mutate(prediction_time=row_number()+417)

par(mfrow=c(1,1))
p1d <- forecasts_dengue_geom[forecasts_dengue_geom$prediction_horizon==1,]
p1c <- forecasts_campy_geom[forecasts_campy_geom$prediction_horizon==1,]
p1m <- forecasts_measles_geom[forecasts_measles_geom$prediction_horizon==1,]

p2d <- p1d[1:112,]
p2c <- p1c[1:114,]
p2m <- p1m[1:60,]
p3d <- p1d[10:112,]
p3c <- p1c[11:114,]
p3m <- p1m[9:60,]

p3d <- left_join(p3d,Iquitos,by="prediction_time")
p3d$week_start_date <- mdy(p3d$week_start_date)

p3c <- left_join(p3c,campyDE,by="prediction_time")

p3m <- left_join(p3m,Bauchi,by="prediction_time")

#plot(1:522,campyDE$case, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week",main= "Predicted vs Actual Campylobacteriosis Cases")
#plot(1:521,p1$obs, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week")
#plot(1:468,Iquitos$total_cases, type="l", ylab ="Dengue Cases", xlab= "Week", main= "Predicted vs Actual Dengue Cases")
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/EE Measles_8_t.png', width=2000, height=1400, res=300,pointsize = 10)
plot(p3m$Date,p3m$obs, type="l", ylab ="Measles Cases", xlab= "time",main="EE 8-week Prediction Horizon",lwd=1.5, ylim = c(0,max(p3m$ub95)), cex.lab=1.2)
polygon(x=c(p3m$Date,rev(p3m$Date)),y=c(p3m$ub95,rev(p3m$lb95)),col = rgb(0, 0, 1, alpha = 0.1), border = rgb(0, 0, 1, alpha = 0.3))
#polygon(x=c(p3m$Date,rev(p3m$Date)),y=c(p3m$ub50,rev(p3m$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
points(p3m$Date,p3m$pred_mean, type = "l", col="blue", lwd=1.5)
abline(v=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("95% PI", "pred.mean","obs.values"),
       col = c( rgb(0, 0, 1, alpha = 0.1),
                "blue",
                "black"), pch = c(15, 15, 15), bty = "n",cex = 1.2)
dev.off()
#####
#plot((4*52):(5*52-1),p3m$obs, type="l", ylab ="Measles Cases", xlab= "Week",main= "Predicted vs Measles Cases",lwd=1.5, ylim = c(0,max(p3m$ub95)))
#polygon(x=c((4*52):(5*52-1),rev((4*52):(5*52-1))),y=c(p3m$ub95,rev(p3m$lb95)),col = rgb(1, 0, 0, alpha = 0.3), border = rgb(1, 0, 0, alpha = 1))
#polygon(x=c((4*52):(5*52-1),rev((4*52):(5*52-1))),y=c(p3m$ub50,rev(p3m$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
#points((4*52):(5*52-1),p3m$pred_mean, type = "l", col="white", lwd=2)
####

####
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/EE Campy_8_t.png', width=2000, height=1400, res=300,pointsize = 10)
plot(p3c$date,p3c$obs, type="l", ylab ="Campylobacteriosis Cases", xlab= "time",main="EE 8-week Prediction Horizon",
     lwd=1.5,ylim = c(0,max(p3c$ub95)),cex.lab=1.2)
#plot(p3c$date,p3c$obs, ylab ="Campylobacteriosis Cases", xlab= "time [years]",
#     pch=16,cex=0.5,ylim = c(0,max(p3c$ub95)))
polygon(x=c(p3c$date,rev(p3c$date)),y=c(p3c$ub95,rev(p3c$lb95)),col = rgb(0, 0, 1, alpha = 0.1), border = rgb(0, 0, 1, alpha = 0.3))
#polygon(x=c(p3c$date,rev(p3c$date)),y=c(p3c$ub50,rev(p3c$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
points(p3c$date,p3c$pred_mean, type = "l", col="blue", lwd=1.5)
abline(v=seq(as.Date("2010-01-01"), by="+6 month", length.out=5),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2010-01-01"), by="+6 month", length.out=5),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("95% PI", "pred.mean","obs.values"),
       col = c( rgb(0, 0, 1, alpha = 0.1),
                "blue",
                "black"), pch = c(15, 15, 15), bty = "n",cex = 1.2)
dev.off()

#plot((8*52):(10*52-1),p3c$obs, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week",main= "Predicted vs Campylobacteriosis Cases",
#     lwd=1.5,ylim = c(0,max(p3c$ub95)))
#polygon(x=c((8*52):(10*52-1),rev((8*52):(10*52-1))),y=c(p3c$ub95,rev(p3c$lb95)),col = rgb(1, 0, 0, alpha = 0.3), border = rgb(1, 0, 0, alpha = 1))
#polygon(x=c((8*52):(10*52-1),rev((8*52):(10*52-1))),y=c(p3c$ub50,rev(p3c$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
#points((8*52):(10*52-1),p3c$pred_mean, type = "l", col="white", lwd=1.5)
####
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds/Final/EE Dengue_8_t.png', width=2000, height=1400, res=300,pointsize = 10)
plot(p3d$week_start_date,p3d$obs,type="l", ylab ="Dengue Cases", xlab= "time",main="EE 8-week Prediction Horizon",lwd=1.5,ylim = c(0,max(p3d$ub95)),cex.lab=1.2)
polygon(x=c(p3d$week_start_date,rev(p3d$week_start_date)),y=c(p3d$ub95,rev(p3d$lb95)),col = rgb(0, 0, 1, alpha = 0.1), border = rgb(0, 0, 1, alpha = 0.3))
#polygon(x=c(p3d$week_start_date,rev(p3d$week_start_date)),y=c(p3d$ub50,rev(p3d$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
points(p3d$week_start_date,p3d$pred_mean, type = "l", col="blue", lwd=1.5)
abline(v=seq(as.Date("2007-07-01"), by="+6 month", length.out=5),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2007-07-01"), by="+6 month", length.out=5),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("95% PI", "pred.mean","obs.values"),
       col = c( rgb(0, 0, 1, alpha = 0.1),
                "blue",
                "black"), pch = c(15, 15, 15), bty = "n",cex = 1.2)
dev.off()
####
#plot((7*52):(9*52-1),p3d$obs,type="l", ylab ="Dengue Cases", xlab= "Week",main= "Predicted vs Dengue Cases",lwd=1.5,ylim = c(0,max(p3d$ub95)))
#polygon(x=c((7*52):(9*52-1),rev((7*52):(9*52-1))),y=c(p3d$ub95,rev(p3d$lb95)),col = rgb(1, 0, 0, alpha = 0.3), border = rgb(1, 0, 0, alpha = 0.3))
#polygon(x=c((7*52):(9*52-1),rev((7*52):(9*52-1))),y=c(p3d$ub50,rev(p3d$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
#points((7*52):(9*52-1),p3d$pred_mean, type = "l", col="white", lwd=1.5)

getECE(p3d$obs,p3d$pred_mean,n_bins=10)
getECE(p3c$obs,p3c$pred_mean,n_bins=10)
getECE(p3m$obs,p3m$pred_mean,n_bins=10)

sqrt(mean(p3d$pred_var))/sqrt(mean(p3d$obs))
sqrt(mean(p3c$pred_var))/sqrt(mean(p3c$obs))
sqrt(mean(p3m$pred_var))/sqrt(mean(p3m$obs))

CalibratR::getRMSE(p3d$obs,p3d$pred_mean)
rmse(p3d$obs,p3d$pred_mean)

sqrt(mean((p3d$obs-p3d$pred_mean)^2,na.rm=T))
sqrt(mean((p3d$obs-p3d$pred_mean)^2,na.rm=T))/sqrt(mean((p3d$obs)^2))
p3d <- p3d%>%
  mutate(CI_cov = between(p3d$obs,p3d$lb95,p3d$ub95))
summary(p3d$CI_cov)
length(p3d$CI_cov[p3d$CI_cov==T])/(length(p3d$CI_cov[p3d$CI_cov==T])+length(p3d$CI_cov[p3d$CI_cov==F]))
###
sqrt(mean((p3c$obs-p3c$pred_mean)^2,na.rm=T))
sqrt(mean((p3c$obs-p3c$pred_mean)^2,na.rm=T))/sqrt(mean((p3c$obs)^2))
p3c <- p3c%>%
  mutate(CI_cov = between(p3c$obs,p3c$lb95,p3c$ub95))
summary(p3c$CI_cov)
length(p3c$CI_cov[p3c$CI_cov==T])/(length(p3c$CI_cov[p3c$CI_cov==T])+length(p3c$CI_cov[p3c$CI_cov==F]))
###
sqrt(mean((p3m$obs-p3m$pred_mean)^2,na.rm=T))
sqrt(mean((p3m$obs-p3m$pred_mean)^2,na.rm=T))/sqrt(mean((p3m$obs)^2))
p3m <- p3m%>%
  mutate(CI_cov = between(p3m$obs,p3m$lb95,p3m$ub95))
summary(p3m$CI_cov)
length(p3m$CI_cov[p3m$CI_cov==T])/(length(p3m$CI_cov[p3m$CI_cov==T])+length(p3m$CI_cov[p3m$CI_cov==F]))
