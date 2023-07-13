setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")

San_Juan_test$date <- mdy(San_Juan_test$week_start_date)

plot(y=San_Juan_test$total_cases, x=San_Juan_test$date, ylim=c(0,130),main= "Time Series for Dengue Cases in Iquitos, Peru", type="l",
     ylab="No. infected", xlab="time [years]")
abline(v = 419, col="red", lty =10)
#######
Iquitos <- read.csv("Iquitos_total2.csv")

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
plot(y=campyDE$hum, x=campyDE$date,type="l", ylab="Average Weekly Absolute Humidity (g/kg)", xlab="Time [years]")
############
Bauchi <- read.csv("Bauchi_Complete.csv")

Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)
Bauchi$Date <- as.Date(Bauchi$Date)
Bauchi <-Bauchi[Bauchi$year > 2012, , drop = FALSE]

plot(Bauchi$Date,Bauchi$Cases,type="l",main= "Time Series for Dengue Cases in the State of Bauchi, Nigeria",ylab="No. infected", xlab="time [years]")
plot(Bauchi$Date,Bauchi$mm,type="l", ylab="Cummulative Weekly Rainfall (mm)", xlab="Time [years]", cex.lab=1.25, cex.axis=1.25)
plot(Bauchi$Date,Bauchi$temperature,type="l",ylab="Average Daily Temperature (°C)",xlab="Time [years]")
plot(Bauchi$Date,Bauchi$avg_hum,type="l",ylab="Average Weekly Relative Humidity (%)",xlab="Time [years]")

plot(Bauchi$Date,Bauchi$temperature,type="l",col="blue")
