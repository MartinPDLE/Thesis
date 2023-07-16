setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")
Iquitos <- read.csv("Iquitos_total2.csv")

predI <- Iquitos[365:468,]
predI$Date <- mdy(predI$week_start_date)

plot(predI$Date, predI$total_cases, type="l",ylab="Dengue Cases", xlab="time",
     main="Observed vs. Predicted Dengue Cases")
points(predI$Date, predI$total_cases1,type="l", col="blue")

sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))
sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))/sqrt(mean((predI$total_cases)^2))


campyDE <- read.csv("campyDE.csv")
campyDE <- campyDE%>%
  mutate(case1 = lag(case, n=1))

predC <- campyDE[419:522,]
predC$date <- as.Date(predC$date)

plot(x=predC$date,y=predC$case, type="l",ylab="Campylobacteriosis Cases", xlab="time [year]",
     main="Observed vs. Predicted Campylobacteriosis Cases")
points(predC$date,predC$case1,type="l", col="blue")

sqrt(mean((predC$case-predC$case1)^2,na.rm=T))
sqrt(mean((predC$case-predC$case1)^2,na.rm=T))/sqrt(mean((predC$case)^2))

Bauchi <- read.csv("Bauchi_Complete.csv")
Bauchi$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( Bauchi$week - 1 )+lubridate::years(Bauchi$year - 2012)
Bauchi$Date <- ymd(Bauchi$Date)

predB <- Bauchi[262:313,]

plot(predB$Date,predB$Cases, type="l",ylab="Measles Cases", xlab="time",
     main="Observed vs. Predicted Measles Cases")
points(predB$Date,predB$Cases01,type="l", col="blue")

sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))
sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))/sqrt(mean((predB$Cases)^2))
