Iquitos <- read.csv("Iquitos_total2.csv")

predI <- Iquitos[365:468,]

sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))
sqrt(mean((predI$total_cases-predI$total_cases1)^2,na.rm=T))/sqrt(mean((predI$total_cases1)^2))


campyDE <- read.csv("campyDE.csv")
campyDE <- campyDE%>%
  mutate(case1 = lag(case, n=1))

predC <- campyDE[419:522,]

sqrt(mean((predC$case-predC$case1)^2,na.rm=T))
sqrt(mean((predC$case-predC$case1)^2,na.rm=T))/sqrt(mean((predC$case1)^2))

Bauchi <- read.csv("Bauchi_Complete.csv")

predB <- Bauchi[262:313,]

sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))
sqrt(mean((predB$Cases-predB$Cases01)^2,na.rm=T))/sqrt(mean((predB$Cases01)^2))
