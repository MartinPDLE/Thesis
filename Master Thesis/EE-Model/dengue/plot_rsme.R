setwd("C:/Users/Martin Perez de Lema/Thesis/Thesis/Master Thesis/EE-Model/dengue")
forecasts_dengue_geom <- read.csv("forecasts/forecasts_dengue_geom_Iq4.csv")
forecasts_campy_geom <- read.csv("forecasts/forecasts_dengue_geom_ca2.csv")
forecasts_measles_geom <- read.csv("forecasts/forecasts_dengue_geom_mb7.csv")


par(mfrow=c(1,1))

p1d <- forecasts_dengue_geom[forecasts_dengue_geom$prediction_horizon==1,]
p1c <- forecasts_campy_geom[forecasts_campy_geom$prediction_horizon==1,]
p1m <- forecasts_measles_geom[forecasts_measles_geom$prediction_horizon==1,]

p2d <- p1d[1:112,]
p2c <- p1c[1:112,]
p2m <- p1m[1:60,]
p3d <- p1d[9:112,]
p3c <- p1c[9:112,]
p3m <- p1m[9:60,]

#plot(1:522,campyDE$case, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week",main= "Predicted vs Actual Campylobacteriosis Cases")
#plot(1:521,p1$obs, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week")
#plot(1:468,Iquitos$total_cases, type="l", ylab ="Dengue Cases", xlab= "Week", main= "Predicted vs Actual Dengue Cases")
plot((5*52):(6*52-1),p3m$obs, type="l", ylab ="Measles Cases", xlab= "Week",main= "Predicted vs Measles Cases",lwd=1.5, ylim = c(0,max(p3m$ub95)))
polygon(x=c((5*52):(6*52-1),rev((5*52):(6*52-1))),y=c(p3m$ub95,rev(p3m$lb95)),col = rgb(1, 0, 0, alpha = 0.3), border = rgb(1, 0, 0, alpha = 1))
polygon(x=c((5*52):(6*52-1),rev((5*52):(6*52-1))),y=c(p3m$ub50,rev(p3m$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
points((5*52):(6*52-1),p3m$pred_mean, type = "l", col="white", lwd=2)
legend("topleft",
       legend = c("95% PI", "50% PI", "pred.mean"),
       col = c( rgb(1, 0, 0, alpha = 0.3),
                rgb(1, 0, 0, alpha = 0.5),
                "white"),
       pch = c(15, 15, 0), bty = "n")
####
plot((8*52):(10*52-1),p3c$obs, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week",main= "Predicted vs Campylobacteriosis Cases",
     lwd=1.5,ylim = c(0,max(p3c$ub95)))
polygon(x=c((8*52):(10*52-1),rev((8*52):(10*52-1))),y=c(p3c$ub95,rev(p3c$lb95)),col = rgb(1, 0, 0, alpha = 0.3), border = rgb(1, 0, 0, alpha = 1))
polygon(x=c((8*52):(10*52-1),rev((8*52):(10*52-1))),y=c(p3c$ub50,rev(p3c$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
points((8*52):(10*52-1),p3c$pred_mean, type = "l", col="white", lwd=1.5)
####
plot((7*52):(9*52-1),p3d$obs,type="l", ylab ="Dengue Cases", xlab= "Week",main= "Predicted vs Dengue Cases",lwd=1.5,ylim = c(0,max(p3d$ub95)))
polygon(x=c((7*52):(9*52-1),rev((7*52):(9*52-1))),y=c(p3d$ub95,rev(p3d$lb95)),col = rgb(1, 0, 0, alpha = 0.3), border = rgb(1, 0, 0, alpha = 0.3))
polygon(x=c((7*52):(9*52-1),rev((7*52):(9*52-1))),y=c(p3d$ub50,rev(p3d$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
points((7*52):(9*52-1),p3d$pred_mean, type = "l", col="white", lwd=1.5)


p3d <- p1d[9:112,]
p3c <- p1c[9:112,]
p3m <- p1m[9:60,]

sqrt(mean((p3d$obs-p3d$pred_mean)^2,na.rm=T))
sqrt(mean((p3d$obs-p3d$pred_mean)^2,na.rm=T))/sqrt(mean((p3d$pred_mean)^2))
p3d <- p3d%>%
  mutate(CI_cov = between(p3d$obs,p3d$lb95,p3d$ub95))
summary(p3d$CI_cov)
length(p3d$CI_cov[p3d$CI_cov==T])/(length(p3d$CI_cov[p3d$CI_cov==T])+length(p3d$CI_cov[p3d$CI_cov==F]))
###
sqrt(mean((p3c$obs-p3c$pred_mean)^2,na.rm=T))
sqrt(mean((p3c$obs-p3c$pred_mean)^2,na.rm=T))/sqrt(mean((p3c$pred_mean)^2))
p3c <- p3c%>%
  mutate(CI_cov = between(p3c$obs,p3c$lb95,p3c$ub95))
summary(p3c$CI_cov)
length(p3c$CI_cov[p3c$CI_cov==T])/(length(p3c$CI_cov[p3c$CI_cov==T])+length(p3c$CI_cov[p3c$CI_cov==F]))
###
sqrt(mean((p3m$obs-p3m$pred_mean)^2,na.rm=T))
sqrt(mean((p3m$obs-p3m$pred_mean)^2,na.rm=T))/sqrt(mean((p3m$pred_mean)^2))
p3m <- p3m%>%
  mutate(CI_cov = between(p3m$obs,p3m$lb95,p3m$ub95))
summary(p3m$CI_cov)
length(p3m$CI_cov[p3m$CI_cov==T])/(length(p3m$CI_cov[p3m$CI_cov==T])+length(p3m$CI_cov[p3m$CI_cov==F]))
