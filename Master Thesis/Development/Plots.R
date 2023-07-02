San_Juan_test$date <- mdy(San_Juan_test$week_start_date)

plot(y=San_Juan_test$total_cases, x=San_Juan_test$date, ylim=c(0,130),main= "Time Series for Dengue Cases in Iquitos, Peru", type="l",
     ylab="No. infected", xlab="time [years]")
abline(v = 419, col="red", lty =10)
#######
sum(Iquitos$total_cases)
summary(Iquitos$air_temperature)
par(mfrow=c(2,2))
Iquitos$date <- mdy(Iquitos$week_start_date)
plot(y=Iquitos$DTR, x=Iquitos$date,type="l")
plot(y=Iquitos$air_temperature, x=Iquitos$date,type="l")
plot(y=Iquitos$precipitation_amount, x=Iquitos$date,type="l")
plot(y=Iquitos$relative_humidity, x=Iquitos$date,type="l")

plot(campyDE$case,type="l",)
points(campyDE$hum*100,type="l",col="red")


Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)
Bauchi$Date <- as.Date(Bauchi$Date)
Bauchi <-Bauchi[Bauchi$year > 2012, , drop = FALSE]

plot(Bauchi$Date,Bauchi$Cases,type="l",main= "Time Series for Dengue Cases in the State of Bauchi, Nigeria",ylab="No. infected", xlab="time [years]")
points(Bauchi$Date,Bauchi$mm,type="l",col="red")
points(Bauchi$temperature,type="l",col="blue")
plot(Bauchi$temperature,type="l",col="blue")
