San_Juan_test$date <- mdy(San_Juan_test$week_start_date)

plot(y=San_Juan_test$total_cases, x=San_Juan_test$date, ylim=c(0,130),main= "Time Series for Dengue Cases in Iquitos, Peru", type="l",
     ylab="No. infected", xlab="time [years]")
abline(v = 419, col="red", lty =10)
