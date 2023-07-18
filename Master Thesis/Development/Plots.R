setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")

z=data.frame(
  x=seq(as.Date("2001-01-01"), by="+1 year", length.out=9),
  y=1:9
)
par(mfrow=c(1,1))
#######
Iquitos <- read.csv("Iquitos_total2.csv")
Iquitos$date <- mdy(Iquitos$week_start_date)
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Dengue Iquitos.png', width=2000, height=1200, res=300,pointsize = 10)
plot(Iquitos$date,Iquitos$total_cases,ylab="No. infected", xlab="time [years]",type="l", lwd=1.1, cex.lab=1.1)
abline(v=seq(as.Date("2001-01-01"), by="+1 year", length.out=9),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2001-01-01"), by="+1 year", length.out=9),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
dev.off()

sum(Iquitos$total_cases)
summary(Iquitos$air_temperature)
par(mfrow=c(1,1))
Iquitos$date <- mdy(Iquitos$week_start_date)
plot(y=Iquitos$DTR, x=Iquitos$date,type="l", ylab="Average Weekly Daily Temperature Range (°C)", xlab="Time [years]")
plot(y=Iquitos$air_temperature, x=Iquitos$date, type="l", ylab="Average Weekly Temperature (°C)", xlab="Time [years]")
plot(y=Iquitos$precipitation_amount, x=Iquitos$date,type="l", ylab="Cummulative Weekly Rainfall (mm)", xlab="Time [years]")
plot(y=Iquitos$relative_humidity, x=Iquitos$date,type="l", ylab="Average Weekly Relative Humidity (%)", xlab="Time [years]")
plot(y=Iquitos$specific_humidity, x=Iquitos$date,type="l", ylab="Average Weekly Absolute Humidity (g/kg)", xlab="Time [years]")

##########
plot(campyDE$case,type="l",)
points(campyDE$hum*100,type="l",col="red")

data(campyDE)
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Campy Ger.png', width=2000, height=1200, res=300,pointsize = 10)
plot(campyDE$date,campyDE$case,ylab="No. infected", xlab="time [years]",type="l", lwd=1.1, cex.lab=1.1)
abline(v=seq(as.Date("2002-01-01"), by="+1 year", length.out=11),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2002-01-01"), by="+1 year", length.out=11),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
dev.off()

plot(y=campyDE$hum, x=campyDE$date,type="l", ylab="Average Weekly Absolute Humidity (g/kg)", xlab="Time [years]")
############
Bauchi <- read.csv("Bauchi_Complete.csv")

Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)
Bauchi$Date <- as.Date(Bauchi$Date)
Bauchi <-Bauchi[Bauchi$year > 2012, , drop = FALSE]

png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Measles Bauchi.png', width=2000, height=1200, res=300,pointsize = 10)
plot(Bauchi$Date,Bauchi$Cases,ylab="No. infected", xlab="time [years]",type="l", lwd=1.1, cex.lab=1.1)
abline(v=seq(as.Date("2013-01-01"), by="+1 year", length.out=6),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2013-01-01"), by="+1 year", length.out=6),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
dev.off()


plot(Bauchi$Date,Bauchi$Cases,type="l",main= "Time Series for Dengue Cases in the State of Bauchi, Nigeria",ylab="No. infected", xlab="time [years]")


plot(Bauchi$Date,Bauchi$mm,type="l", ylab="Cummulative Weekly Rainfall (mm)", xlab="Time [years]", cex.lab=1.25, cex.axis=1.25)
plot(Bauchi$Date,Bauchi$temperature,type="l",ylab="Average Daily Temperature (°C)",xlab="Time [years]")
plot(Bauchi$Date,Bauchi$avg_hum,type="l",ylab="Average Weekly Relative Humidity (%)",xlab="Time [years]")

plot(Bauchi$Date,Bauchi$temperature,type="l",col="blue")
