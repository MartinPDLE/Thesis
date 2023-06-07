setwd("C:/Users/Martin Perez de Lema/Thesis/Thesis/Master Thesis/Endemic-Epidemic-Model/dengue")
forecasts_dengue_geom <- read.csv("forecasts/forecasts_dengue_geom_iq_3.csv")

par(mfrow=c(1,1))

p1 <- forecasts_dengue_geom[forecasts_dengue_geom$prediction_horizon==1,]

p2 <- p1[2:104,]
#p2 <- p1[1:104,]

#plot(1:522,campyDE$case, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week",main= "Predicted vs Actual Campylobacteriosis Cases")
#plot(1:521,p1$obs, type="l", ylab ="Campylobacteriosis Cases", xlab= "Week")
#plot(1:468,Iquitos$total_cases, type="l", ylab ="Dengue Cases", xlab= "Week", main= "Predicted vs Actual Dengue Cases")
plot(365:467,p2$obs, type="l", ylab ="Dengue Cases", xlab= "Week",main= "Predicted vs Dengue Cases")
polygon(x=c(365:467,rev(365:467)),y=c(p2$ub95,rev(p2$lb95)),col = rgb(0, 1, 1, alpha = 0.5), border = rgb(0, 1, 1, alpha = 1))
polygon(x=c(365:467,rev(365:467)),y=c(p2$ub50,rev(p2$lb50)),col = rgb(0, 1, 1, alpha = 0.8), border = rgb(0, 1, 1, alpha = 1))
points(365:467,p2$pred_mean, type = "l", col="white", lwd=2)
#points(365:467,p2$pred_mean, type = "l", col="blue")
#points(365:467,p2$pred_mean, type = "l", col="blue")
#points(10:363,fits_dengue[["geom"]][["fitted.values"]], type="l", col="red")
#points(408:521,p1$lb95, type = "l", col="red")
#points(408:521,p1$ub95, type = "l", col="red")


#pred<- Iquitos$total_cases[364:467]
pred <- campyDE$case[408:521]
sqrt(mean((p2$obs-p2$pred_mean)^2,na.rm=T))/sqrt(mean((p2$pred_mean)^2))

