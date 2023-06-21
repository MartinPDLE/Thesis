library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tidyselect)
library(WriteXLS)
library(lubridate)
library(stringr)

setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Data")

{
  Cholera <- read_excel("Nigeria_Case.xls", sheet = "Cholera1", col_names = T, skip = 1)

Cholera <- Cholera%>%
  filter(State %in% c("Sokoto","Bauchi","Akwa Ibom"))%>%
  select(-c(SNO,GeoZones,NoOfLGAs,StatePop2016,QtrCas00,QtrLab00,QtrDcd00))

Cholera_S <- Cholera%>%
  filter(State=="Sokoto")%>%
  select(-c(State,SumCas00))
Cholera_S <- Cholera_S%>%
  select(vars_select(names(Cholera_S), starts_with('SumCas', ignore.case = TRUE)))
Cholera_S <- t(Cholera_S)
Cholera_S <- as.data.frame(Cholera_S)
Cholera_S <- Cholera_S[, 6:1]
Cholera_S <- data.frame(SumCas01=unlist(Cholera_S, use.names = FALSE))
plot(Cholera_S$SumCas01, type="l")

Cholera_A <- Cholera%>%
  filter(State=="Akwa Ibom")%>%
  select(-c(State,SumCas00))
Cholera_A <- Cholera_A%>%
  select(vars_select(names(Cholera_A), starts_with('SumCas', ignore.case = TRUE)))
Cholera_A <- t(Cholera_A)
Cholera_A <- as.data.frame(Cholera_A)
Cholera_A <- Cholera_A[, 6:1]
Cholera_A <- data.frame(SumCas01=unlist(Cholera_A, use.names = FALSE))
plot(Cholera_A$SumCas01, type="l")

Cholera_B <- Cholera%>%
  filter(State=="Bauchi")%>%
  select(-c(State,SumCas00))
Cholera_B <- Cholera_B%>%
  select(vars_select(names(Cholera_B), starts_with('SumCas', ignore.case = TRUE)))
Cholera_B <- t(Cholera_B)
Cholera_B <- as.data.frame(Cholera_B)
Cholera_B <- Cholera_B[, 6:1]
Cholera_B <- data.frame(SumCas01=unlist(Cholera_B, use.names = FALSE))
plot(Cholera_B$SumCas01, type="l")

rm(Cholera)
}

{
  Measles <- read_excel("Nigeria_Case.xls", sheet = "Measles1", col_names = T, skip = 1)
  
  Measles <- Measles%>%
    filter(State %in% c("Sokoto","Bauchi","Akwa Ibom"))%>%
    select(-c(SNO,GeoZones,NoOfLGAs,StatePop2016,QtrCas00,QtrLab00,QtrDcd00))
  
  Measles_S <- Measles%>%
    filter(State=="Sokoto")%>%
    select(-c(State,SumCas00))
  Measles_S <- Measles_S%>%
    select(vars_select(names(Measles_S), starts_with('SumCas', ignore.case = TRUE)))
  Measles_S <- t(Measles_S)
  Measles_S <- as.data.frame(Measles_S)
  Measles_S <- Measles_S[, 6:1]
  Measles_S <- data.frame(SumCas01=unlist(Measles_S, use.names = FALSE))
  plot(Measles_S$SumCas01, type="l", ylab = "Measles Cases", xlab = "Week",
       main = "Measles Cases in the State of Sokoto")
  
  Measles_A <- Measles%>%
    filter(State=="Akwa Ibom")%>%
    select(-c(State,SumCas00))
  Measles_A <- Measles_A%>%
    select(vars_select(names(Measles_A), starts_with('SumCas', ignore.case = TRUE)))
  Measles_A <- t(Measles_A)
  Measles_A <- as.data.frame(Measles_A)
  Measles_A <- Measles_A[, 6:1]
  Measles_A <- data.frame(SumCas01=unlist(Measles_A, use.names = FALSE))
  plot(Measles_A$SumCas01, type="l")
  
  Measles_B <- Measles%>%
    filter(State=="Bauchi")%>%
    select(-c(State,SumCas00))
  Measles_B <- Measles_B%>%
    select(vars_select(names(Measles_B), starts_with('SumCas', ignore.case = TRUE)))
  Measles_B <- t(Measles_B)
  Measles_B <- as.data.frame(Measles_B)
  Measles_B <- Measles_B[, 6:1]
  Measles_B <- data.frame(SumCas01=unlist(Measles_B, use.names = FALSE))
  plot(y=Measles_B$SumCas01,x= type="l", ylab = "Measles Cases", xlab = "Week",
       main = "Measles Cases in the State of Bauchi")
  
  rm(Measles)
}

{
  Lassa <- read_excel("Nigeria_Case.xls", sheet = "Lassa1", col_names = T, skip = 1)
  
  Lassa <- Lassa%>%
    filter(State %in% c("Sokoto","Bauchi","Akwa Ibom"))%>%
    select(-c(SNO,GeoZones,NoOfLGAs,StatePop2016,QtrCas00,QtrLab00,QtrDcd00))
  
  Lassa_S <- Lassa%>%
    filter(State=="Sokoto")%>%
    select(-c(State,SumCas00))
  Lassa_S <- Lassa_S%>%
    select(vars_select(names(Lassa_S), starts_with('SumCas', ignore.case = TRUE)))
  Lassa_S <- t(Lassa_S)
  Lassa_S <- as.data.frame(Lassa_S)
  Lassa_S <- Lassa_S[, 6:1]
  Lassa_S <- data.frame(SumCas01=unlist(Lassa_S, use.names = FALSE))
  plot(Lassa_S$SumCas01, type="l")
  
  Lassa_A <- Lassa%>%
    filter(State=="Akwa Ibom")%>%
    select(-c(State,SumCas00))
  Lassa_A <- Lassa_A%>%
    select(vars_select(names(Lassa_A), starts_with('SumCas', ignore.case = TRUE)))
  Lassa_A <- t(Lassa_A)
  Lassa_A <- as.data.frame(Lassa_A)
  Lassa_A <- Lassa_A[, 6:1]
  Lassa_A <- data.frame(SumCas01=unlist(Lassa_A, use.names = FALSE))
  plot(Lassa_A$SumCas01, type="l")
  
  Lassa_B <- Lassa%>%
    filter(State=="Bauchi")%>%
    select(-c(State,SumCas00))
  Lassa_B <- Lassa_B%>%
    select(vars_select(names(Lassa_B), starts_with('SumCas', ignore.case = TRUE)))
  Lassa_B <- t(Lassa_B)
  Lassa_B <- as.data.frame(Lassa_B)
  Lassa_B <- Lassa_B[, 6:1]
  Lassa_B <- data.frame(SumCas01=unlist(Lassa_B, use.names = FALSE))
  plot(Lassa_B$SumCas01, type="l")
  
  rm(Lassa)
}

{
  CSM <- read_excel("Nigeria_Case.xls", sheet = "CSM1", col_names = T, skip = 1)
  
  CSM <- CSM%>%
    filter(State %in% c("Sokoto","Bauchi","Akwa Ibom"))%>%
    select(-c(SNO,GeoZones,NoOfLGAs,StatePop2016,QtrCas00,QtrLab00,QtrDcd00))
  
  CSM_S <- CSM%>%
    filter(State=="Sokoto")%>%
    select(-c(State,SumCas00))
  CSM_S <- CSM_S%>%
    select(vars_select(names(CSM_S), starts_with('SumCas', ignore.case = TRUE)))
  CSM_S <- t(CSM_S)
  CSM_S <- as.data.frame(CSM_S)
  CSM_S <- CSM_S[, 6:1]
  CSM_S <- data.frame(SumCas01=unlist(CSM_S, use.names = FALSE))
  plot(CSM_S$SumCas01, type="l",ylab = "CSM Cases", xlab = "Week",
       main = "CSM Cases in the State of Sokoto")
  
  CSM_A <- CSM%>%
    filter(State=="Akwa Ibom")%>%
    select(-c(State,SumCas00))
  CSM_A <- CSM_A%>%
    select(vars_select(names(CSM_A), starts_with('SumCas', ignore.case = TRUE)))
  CSM_A <- t(CSM_A)
  CSM_A <- as.data.frame(CSM_A)
  CSM_A <- CSM_A[, 6:1]
  CSM_A <- data.frame(SumCas01=unlist(CSM_A, use.names = FALSE))
  plot(CSM_A$SumCas01, type="l")
  
  CSM_B <- CSM%>%
    filter(State=="Bauchi")%>%
    select(-c(State,SumCas00))
  CSM_B <- CSM_B%>%
    select(vars_select(names(CSM_B), starts_with('SumCas', ignore.case = TRUE)))
  CSM_B <- t(CSM_B)
  CSM_B <- as.data.frame(CSM_B)
  CSM_B <- CSM_B[, 6:1]
  CSM_B <- data.frame(SumCas01=unlist(CSM_B, use.names = FALSE))
  plot(CSM_B$SumCas01, type="l")
  
  rm(CSM)
}

###
Measles_t <- Measles_B%>%
  mutate(week = rep(1:53,times=6))
Measles_t <- Measles_t%>%
  mutate(year = rep(2012:2017,each=53))
Measles_t <- Measles_t%>%
  rename("Cases"="SumCas01")
write_csv(Measles_t,"Measles_Bauchi.csv")

#####
Measles <- read.csv("Measles_Bauchi.csv")
Measles$week <- sprintf("%02d", Measles$week)
Measles <- Measles%>%
  mutate(index = paste(year,week,sep="-"))%>%
  arrange(index)%>%
  select(-c(year,week))
#Delete non existing week 53s
Measles <- Measles[-c(53,106,159,265,318),]

hum <- read_excel("Bauchi_Weather.xlsx", sheet = "Rel Hum")
#delete 31-12-17 as it belongs in 1st week of 2018
hum <- hum[-2192,]
hum <- hum%>%
  mutate(date = make_date(year= year, month = mn, day =dy))
hum <- hum%>%
  mutate(week = epiweek(date))
hum$week <- sprintf("%02d", hum$week)
hum <- hum%>%
  group_by(week,year)%>%
  summarize(avg_hum = mean(RH)
  )%>%
  mutate(index = paste(year,week,sep="-"))%>%
  arrange(index)
hum <- hum%>%
  group_by(index)%>%
  select(-c(year,week))

temp <- read_excel("Bauchi_Weather.xlsx", sheet = "Temp")
#delete 31-12-17 as it belongs in 1st week of 2018
temp <- temp[-2192,]
temp <- temp%>%
  mutate(date = make_date(year= year, month = Month, day =day))
temp <- temp%>%
  mutate(week = epiweek(date))%>%
  mutate(grp = row_number())
temp$week <- sprintf("%02d", temp$week)
temp$Temp <- 1/2*(temp$`Max Temp`+temp$`Min Temp`)
temp <- temp%>%
  group_by(week,year)%>%
  summarize(avg_temp = mean(Temp)
  )%>%
mutate(index = paste(year,week,sep="-"))%>%
  arrange(index)
temp <- temp%>%
  group_by(index)%>%
  select(-c(year,week))
  

rain <- read_excel("Bauchi_Weather.xlsx", sheet = "Rain")
#delete 31-12-17 as it belongs in 1st week of 2018
rain <- rain[-2192,]
rain <- rain%>%
  mutate(date = make_date(year= year, month = month, day =day))
rain <- rain%>%
  mutate(week = epiweek(date))
rain$week <- sprintf("%02d", rain$week)
rain <- rain%>%
  group_by(week,year)%>%
  summarize(Sum_mm = sum(mm)
  )%>%
  mutate(index = paste(year,week,sep="-"))%>%
  arrange(index)

weather <- merge(rain,temp, by="index")
weather <- merge(weather,hum, by="index")
#delete duplicate week 53
weather <- weather[-157,]
weather <- weather%>%
  rename(mm = Sum_mm, temperature = avg_temp)
Bauchi <- merge(Measles,weather, by="index")

##### Add Lags 
test <- Bauchi%>%
  select(Cases)
addlag <- function(df, i) {
  varname <- paste(x, i , sep="")
  df$varname <- with(df, lag(x, n=i))
  df
}
#test <- Bauchi
#variables <- names(Bauchi[,c(2,5,6)])
#for (x in variables) {
#  for (v in 1:25) {
#    test <- addlag(test,v)
#  }
#}
lag_matrix<-c()
for (i in 1:20 ){
  temp <- Bauchi$Cases
  #lag_matrix<- c(lag_matrix,lag(Cases, n=i))
  lag_matrix <- rbind(lag_matrix,lag(Cases, n=i))
}
lag_matrix <- t(lag_matrix)
{
Bauchi <- Bauchi%>%
  mutate(Cases01 = lag(Cases, n=1))
Bauchi <- Bauchi%>%
  mutate(Cases02 = lag(Cases, n=2))
Bauchi <- Bauchi%>%
  mutate(Cases03 = lag(Cases, n=3))
Bauchi <- Bauchi%>%
  mutate(Cases04 = lag(Cases, n=4))
Bauchi <- Bauchi%>%
  mutate(Cases05 = lag(Cases, n=5))
Bauchi <- Bauchi%>%
  mutate(Cases06 = lag(Cases, n=6))
Bauchi <- Bauchi%>%
  mutate(Cases07 = lag(Cases, n=7))
Bauchi <- Bauchi%>%
  mutate(Cases08 = lag(Cases, n=8))
Bauchi <- Bauchi%>%
  mutate(Cases09 = lag(Cases, n=9))
Bauchi <- Bauchi%>%
  mutate(Cases10 = lag(Cases, n=10))
Bauchi <- Bauchi%>%
  mutate(Cases11 = lag(Cases, n=11))
Bauchi <- Bauchi%>%
  mutate(Cases12 = lag(Cases, n=12))
Bauchi <- Bauchi%>%
  mutate(Cases13 = lag(Cases, n=13))
Bauchi <- Bauchi%>%
  mutate(Cases14 = lag(Cases, n=14))
Bauchi <- Bauchi%>%
  mutate(Cases15 = lag(Cases, n=15))
Bauchi <- Bauchi%>%
  mutate(Cases16 = lag(Cases, n=16))
Bauchi <- Bauchi%>%
  mutate(Cases17 = lag(Cases, n=17))
Bauchi <- Bauchi%>%
  mutate(Cases18 = lag(Cases, n=18))
Bauchi <- Bauchi%>%
  mutate(Cases19 = lag(Cases, n=19))
Bauchi <- Bauchi%>%
  mutate(Cases20 = lag(Cases, n=20))
}
{
Bauchi <- Bauchi%>%
  mutate(temperature01 = lag(temperature, n=1))
Bauchi <- Bauchi%>%
  mutate(temperature02 = lag(temperature, n=2))
Bauchi <- Bauchi%>%
  mutate(temperature03 = lag(temperature, n=3))
Bauchi <- Bauchi%>%
  mutate(temperature04 = lag(temperature, n=4))
Bauchi <- Bauchi%>%
  mutate(temperature05 = lag(temperature, n=5))
Bauchi <- Bauchi%>%
  mutate(temperature06 = lag(temperature, n=6))
Bauchi <- Bauchi%>%
  mutate(temperature07 = lag(temperature, n=7))
Bauchi <- Bauchi%>%
  mutate(temperature08 = lag(temperature, n=8))
Bauchi <- Bauchi%>%
  mutate(temperature09 = lag(temperature, n=9))
Bauchi <- Bauchi%>%
  mutate(temperature10 = lag(temperature, n=10))
Bauchi <- Bauchi%>%
  mutate(temperature11 = lag(temperature, n=11))
Bauchi <- Bauchi%>%
  mutate(temperature12 = lag(temperature, n=12))
Bauchi <- Bauchi%>%
  mutate(temperature13 = lag(temperature, n=13))
Bauchi <- Bauchi%>%
  mutate(temperature14 = lag(temperature, n=14))
Bauchi <- Bauchi%>%
  mutate(temperature15 = lag(temperature, n=15))
Bauchi <- Bauchi%>%
  mutate(temperature16 = lag(temperature, n=16))
Bauchi <- Bauchi%>%
  mutate(temperature17 = lag(temperature, n=17))
Bauchi <- Bauchi%>%
  mutate(temperature18 = lag(temperature, n=18))
Bauchi <- Bauchi%>%
  mutate(temperature19 = lag(temperature, n=19))
Bauchi <- Bauchi%>%
  mutate(temperature20 = lag(temperature, n=20))
}
{
  Bauchi <- Bauchi%>%
    mutate(avg_hum01 = lag(avg_hum, n=1))
  Bauchi <- Bauchi%>%
    mutate(avg_hum02 = lag(avg_hum, n=2))
  Bauchi <- Bauchi%>%
    mutate(avg_hum03 = lag(avg_hum, n=3))
  Bauchi <- Bauchi%>%
    mutate(avg_hum04 = lag(avg_hum, n=4))
  Bauchi <- Bauchi%>%
    mutate(avg_hum05 = lag(avg_hum, n=5))
  Bauchi <- Bauchi%>%
    mutate(avg_hum06 = lag(avg_hum, n=6))
  Bauchi <- Bauchi%>%
    mutate(avg_hum07 = lag(avg_hum, n=7))
  Bauchi <- Bauchi%>%
    mutate(avg_hum08 = lag(avg_hum, n=8))
  Bauchi <- Bauchi%>%
    mutate(avg_hum09 = lag(avg_hum, n=9))
  Bauchi <- Bauchi%>%
    mutate(avg_hum10 = lag(avg_hum, n=10))
  Bauchi <- Bauchi%>%
    mutate(avg_hum11 = lag(avg_hum, n=11))
  Bauchi <- Bauchi%>%
    mutate(avg_hum12 = lag(avg_hum, n=12))
  Bauchi <- Bauchi%>%
    mutate(avg_hum13 = lag(avg_hum, n=13))
  Bauchi <- Bauchi%>%
    mutate(avg_hum14 = lag(avg_hum, n=14))
  Bauchi <- Bauchi%>%
    mutate(avg_hum15 = lag(avg_hum, n=15))
  Bauchi <- Bauchi%>%
    mutate(avg_hum16 = lag(avg_hum, n=16))
  Bauchi <- Bauchi%>%
    mutate(avg_hum17 = lag(avg_hum, n=17))
  Bauchi <- Bauchi%>%
    mutate(avg_hum18 = lag(avg_hum, n=18))
  Bauchi <- Bauchi%>%
    mutate(avg_hum19 = lag(avg_hum, n=19))
  Bauchi <- Bauchi%>%
    mutate(avg_hum20 = lag(avg_hum, n=20))
}
{
Bauchi <- Bauchi%>%
  mutate(mm01 = lag(mm, n=1))
Bauchi <- Bauchi%>%
  mutate(mm02 = lag(mm, n=2))
Bauchi <- Bauchi%>%
  mutate(mm03 = lag(mm, n=3))
Bauchi <- Bauchi%>%
  mutate(mm04 = lag(mm, n=4))
Bauchi <- Bauchi%>%
  mutate(mm05 = lag(mm, n=5))
Bauchi <- Bauchi%>%
  mutate(mm06 = lag(mm, n=6))
Bauchi <- Bauchi%>%
  mutate(mm07 = lag(mm, n=7))
Bauchi <- Bauchi%>%
  mutate(mm08 = lag(mm, n=8))
Bauchi <- Bauchi%>%
  mutate(mm09 = lag(mm, n=9))
Bauchi <- Bauchi%>%
  mutate(mm10 = lag(mm, n=10))
Bauchi <- Bauchi%>%
  mutate(mm11 = lag(mm, n=11))
Bauchi <- Bauchi%>%
  mutate(mm12 = lag(mm, n=12))
Bauchi <- Bauchi%>%
  mutate(mm13 = lag(mm, n=13))
Bauchi <- Bauchi%>%
  mutate(mm14 = lag(mm, n=14))
Bauchi <- Bauchi%>%
  mutate(mm15 = lag(mm, n=15))
Bauchi <- Bauchi%>%
  mutate(mm16 = lag(mm, n=16))
Bauchi <- Bauchi%>%
  mutate(mm17 = lag(mm, n=17))
Bauchi <- Bauchi%>%
  mutate(mm18 = lag(mm, n=18))
Bauchi <- Bauchi%>%
  mutate(mm19 = lag(mm, n=19))
Bauchi <- Bauchi%>%
  mutate(mm20 = lag(mm, n=20))
Bauchi <- Bauchi%>%
  mutate(mm21 = lag(mm, n=21))
Bauchi <- Bauchi%>%
  mutate(mm22 = lag(mm, n=22))
Bauchi <- Bauchi%>%
  mutate(mm23 = lag(mm, n=23))
Bauchi <- Bauchi%>%
  mutate(mm24 = lag(mm, n=24))
}


write.csv(Bauchi,"Bauchi_Complete.csv" )
Bauchi<-read.csv("Bauchi_Complete.csv")

plot(x=Bauchi$Sum_mm,y=Bauchi$Cases, abline())
plot(x=Bauchi$avg_temp,y=Bauchi$Cases)
plot(Bauchi$temperature, type="l")
plot(Bauchi$Cases,type="l")
plot(Bauchi$mm, type="l")
plot(Bauchi$avg_hum, type="l")
