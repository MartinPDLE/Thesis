Iquitos <- read.csv("Iquitos_total2.csv")

predI <- Iquitos[365:468,]

plot(predI$total_cases, type="l",ylab="Dengue Cases", xlab="week",
     main="Observed vs. Predicted Dengue Cases")
points(predI$total_cases1,type="l", col="blue")

sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))
sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))/sqrt(mean((predI$total_cases1)^2))


campyDE <- read.csv("campyDE.csv")
campyDE <- campyDE%>%
  mutate(case1 = lag(case, n=1))

predC <- campyDE[419:522,]

plot(predC$case, type="l",ylab="Campylobacteriosis Cases", xlab="week",
     main="Observed vs. Predicted Campylobacteriosis Cases")
points(predC$case1,type="l", col="blue")

sqrt(mean((predC$case-predC$case1)^2,na.rm=T))
sqrt(mean((predC$case-predC$case1)^2,na.rm=T))/sqrt(mean((predC$case1)^2))

Bauchi <- read.csv("Bauchi_Complete.csv")

predB <- Bauchi[262:313,]

plot(predC$case, type="l",ylab="Measles Cases", xlab="week",
     main="Observed vs. Predicted Measles Cases")
points(predC$case1,type="l", col="blue")

sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))
sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))/sqrt(mean((predB$Cases01)^2))
