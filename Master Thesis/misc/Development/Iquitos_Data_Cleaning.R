library(dlnm)
library(gam)
library(mgcv)
library(dplyr)
library(tidyverse)
library(utilities)
library(readxl)
library(Thermimage)

Iquitos <- read.csv("Iquitos_Training_Data.csv")

dat <- read_excel("Iquitos_Hum.xlsx", sheet = "ReanalysisHumidity", col_types = c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
dat<- dat[-1, ]

dat <- dat%>%
  filter(Year>=2000 & Year<=2010)

dat <- dat%>%
  select(-c(dew_point_temperature,))

dat <- dat%>%
  mutate(Date =make_date(Year,Month,Day))

dat$Date <- as.Date(dat$Date)

dat$DTR <- dat$maximum_air_temperature-dat$minimum_air_temperature

dat <- dat%>%
  select(-c(1,2,3,8,9))

dat <- dat[dat$Date >= "2000-07-01" & dat$Date <= "2009-07-01",]

dat <- dat[dat$Date != "2004-02-29" & dat$Date !="2008-02-29",]

dat <- dat[dat$Date != "2000-12-31" & dat$Date != "2001-12-31" & dat$Date != "2002-12-31" & dat$Date != "2003-12-31" &
           dat$Date != "2004-12-31" & dat$Date != "2005-12-31" & dat$Date != "2006-12-31" & dat$Date != "2007-12-31" &
           dat$Date != "2008-12-31" & dat$Date != "2009-07-01", ]

dat$air_temperature <- dat$air_temperature -273.15

dat <- dat %>% 
  mutate(grp = 1+ (row_number()-1) %/% 7) %>% 
  group_by(grp) %>% 
  summarise(across(everything(), mean, na.rm = TRUE)) %>% 
  select(-grp)

dat$Date <- dat$Date -3
dat$precipitation_amount <- dat$precipitation_amount*7

Iquitos <- Iquitos%>%
  mutate(id = row_number())
dat <- dat%>%
  mutate(id = row_number())

dat <- merge(Iquitos, dat, by = "id")

dat <-dat%>%
  select(-c(id,Date))

dat <- dat%>%
  select(-c(denv1_cases,denv2_cases,denv3_cases,denv4_cases,other_positive_cases))

dat <- dat%>%
  mutate(total_cases1 = lag(total_cases, n=1))
  dat <- dat%>%
    mutate(total_cases2 = lag(total_cases, n=2))
  dat <- dat%>%
    mutate(total_cases3 = lag(total_cases, n=3))
  dat <- dat%>%
    mutate(total_cases4 = lag(total_cases, n=4))
  dat <- dat%>%
    mutate(total_cases5 = lag(total_cases, n=5))
dat <- dat%>%
  mutate(air_temperature1 = lag(air_temperature, n=1))
dat <- dat%>%mutate(air_temperature2 = lag(air_temperature, n=2))
  dat <- dat%>%
    mutate(air_temperature3 = lag(air_temperature, n=3))
  dat <- dat%>%
    mutate(air_temperature4 = lag(air_temperature, n=4))
  dat <- dat%>%
    mutate(air_temperature5 = lag(air_temperature, n=5))
dat <- dat%>%
   mutate(relative_humidity1 = lag(relative_humidity, n=1))
  dat <- dat%>%
    mutate(relative_humidity2 = lag(relative_humidity, n=2))
  dat <- dat%>%
    mutate(relative_humidity3 = lag(relative_humidity, n=3))
  dat <- dat%>%
    mutate(relative_humidity4 = lag(relative_humidity, n=4))
  dat <- dat%>%
    mutate(relative_humidity5 = lag(relative_humidity, n=5))
dat <- dat%>%
    mutate(specific_humidity1 = lag(specific_humidity, n=1))
  dat <- dat%>%
    mutate(specific_humidity2 = lag(specific_humidity, n=2))
  dat <- dat%>%
    mutate(specific_humidity3 = lag(specific_humidity, n=3))
  dat <- dat%>%
    mutate(specific_humidity4 = lag(specific_humidity, n=4))
  dat <- dat%>%
    mutate(specific_humidity5 = lag(specific_humidity, n=5))
dat <- dat%>%
  mutate(precipitation_amount1 = lag(precipitation_amount, n=1))
dat <- dat%>%
  mutate(precipitation_amount2 = lag(precipitation_amount, n=2))
dat <- dat%>%
  mutate(precipitation_amount3 = lag(precipitation_amount, n=3))
dat <- dat%>%
  mutate(precipitation_amount4 = lag(precipitation_amount, n=4))
dat <- dat%>%
  mutate(precipitation_amount5 = lag(precipitation_amount, n=5))
dat <- dat%>%
  mutate(precipitation_amount6 = lag(precipitation_amount, n=6))
dat <- dat%>%
  mutate(precipitation_amount7 = lag(precipitation_amount, n=7))
dat <- dat%>%
  mutate(precipitation_amount8 = lag(precipitation_amount, n=8))
dat <- dat%>%
  mutate(precipitation_amount9 = lag(precipitation_amount, n=9))
dat <- dat%>%
  mutate(precipitation_amount10 = lag(precipitation_amount, n=10))
dat <- dat%>%
  mutate(precipitation_amount11 = lag(precipitation_amount, n=11))
dat <- dat%>%
  mutate(precipitation_amount12 = lag(precipitation_amount, n=12))
dat <- dat%>%
  mutate(DTR1 = lag(DTR, n=1))
dat <- dat%>%
  mutate(DTR2 = lag(DTR, n=2))
dat <- dat%>%
  mutate(DTR3 = lag(DTR, n=3))
dat <- dat%>%
  mutate(DTR4 = lag(DTR, n=4))
dat <- dat%>%
  mutate(DTR5 = lag(DTR, n=5))

write.csv(dat,"Iquitos_total2.csv")
