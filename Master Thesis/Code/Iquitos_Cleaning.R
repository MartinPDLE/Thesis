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

dat <- read.csv("Iquitos_total2.csv")
{
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
    mutate(total_cases6 = lag(total_cases, n=5))
  dat <- dat%>%
    mutate(total_cases7 = lag(total_cases, n=7))
  dat <- dat%>%
    mutate(total_cases8 = lag(total_cases, n=8))
  dat <- dat%>%
    mutate(total_cases9 = lag(total_cases, n=9))
  dat <- dat%>%
    mutate(total_cases10 = lag(total_cases, n=10))
  dat <- dat%>%
    mutate(total_cases11 = lag(total_cases, n=11))
  dat <- dat%>%
    mutate(total_cases12 = lag(total_cases, n=12))
  dat <- dat%>%
    mutate(total_cases13 = lag(total_cases, n=13))
  dat <- dat%>%
    mutate(total_cases14 = lag(total_cases, n=14))
  dat <- dat%>%
    mutate(total_cases15 = lag(total_cases, n=15))
  dat <- dat%>%
    mutate(total_cases16 = lag(total_cases, n=16))
  dat <- dat%>%
    mutate(total_cases17 = lag(total_cases, n=17))
  dat <- dat%>%
    mutate(total_cases18 = lag(total_cases, n=18))
  dat <- dat%>%
    mutate(total_cases19 = lag(total_cases, n=19))
  dat <- dat%>%
    mutate(total_cases20 = lag(total_cases, n=20))
  }
  {
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
      mutate(relative_humidity6 = lag(relative_humidity, n=5))
    dat <- dat%>%
      mutate(relative_humidity7 = lag(relative_humidity, n=7))
    dat <- dat%>%
      mutate(relative_humidity8 = lag(relative_humidity, n=8))
    dat <- dat%>%
      mutate(relative_humidity9 = lag(relative_humidity, n=9))
    dat <- dat%>%
      mutate(relative_humidity10 = lag(relative_humidity, n=10))
    dat <- dat%>%
      mutate(relative_humidity11 = lag(relative_humidity, n=11))
    dat <- dat%>%
      mutate(relative_humidity12 = lag(relative_humidity, n=12))
    dat <- dat%>%
      mutate(relative_humidity13 = lag(relative_humidity, n=13))
    dat <- dat%>%
      mutate(relative_humidity14 = lag(relative_humidity, n=14))
    dat <- dat%>%
      mutate(relative_humidity15 = lag(relative_humidity, n=15))
    dat <- dat%>%
      mutate(relative_humidity16 = lag(relative_humidity, n=16))
    dat <- dat%>%
      mutate(relative_humidity17 = lag(relative_humidity, n=17))
    dat <- dat%>%
      mutate(relative_humidity18 = lag(relative_humidity, n=18))
    dat <- dat%>%
      mutate(relative_humidity19 = lag(relative_humidity, n=19))
    dat <- dat%>%
      mutate(relative_humidity20 = lag(relative_humidity, n=20))
    }
{
    dat <- dat%>%
      mutate(air_temperature1 = lag(air_temperature, n=1))
    dat <- dat%>%
      mutate(air_temperature2 = lag(air_temperature, n=2))
    dat <- dat%>%
      mutate(air_temperature3 = lag(air_temperature, n=3))
    dat <- dat%>%
      mutate(air_temperature4 = lag(air_temperature, n=4))
    dat <- dat%>%
      mutate(air_temperature5 = lag(air_temperature, n=5))
    dat <- dat%>%
      mutate(air_temperature6 = lag(air_temperature, n=5))
    dat <- dat%>%
      mutate(air_temperature7 = lag(air_temperature, n=7))
    dat <- dat%>%
      mutate(air_temperature8 = lag(air_temperature, n=8))
    dat <- dat%>%
      mutate(air_temperature9 = lag(air_temperature, n=9))
    dat <- dat%>%
      mutate(air_temperature10 = lag(air_temperature, n=10))
    dat <- dat%>%
      mutate(air_temperature11 = lag(air_temperature, n=11))
    dat <- dat%>%
      mutate(air_temperature12 = lag(air_temperature, n=12))
    dat <- dat%>%
      mutate(air_temperature13 = lag(air_temperature, n=13))
    dat <- dat%>%
      mutate(air_temperature14 = lag(air_temperature, n=14))
    dat <- dat%>%
      mutate(air_temperature15 = lag(air_temperature, n=15))
    dat <- dat%>%
      mutate(air_temperature16 = lag(air_temperature, n=16))
    dat <- dat%>%
      mutate(air_temperature17 = lag(air_temperature, n=17))
    dat <- dat%>%
      mutate(air_temperature18 = lag(air_temperature, n=18))
    dat <- dat%>%
      mutate(air_temperature19 = lag(air_temperature, n=19))
    dat <- dat%>%
      mutate(air_temperature20 = lag(air_temperature, n=20))
}
{
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
  dat <- dat%>%
    mutate(DTR6 = lag(DTR, n=5))
  dat <- dat%>%
    mutate(DTR7 = lag(DTR, n=7))
  dat <- dat%>%
    mutate(DTR8 = lag(DTR, n=8))
  dat <- dat%>%
    mutate(DTR9 = lag(DTR, n=9))
  dat <- dat%>%
    mutate(DTR10 = lag(DTR, n=10))
  dat <- dat%>%
    mutate(DTR11 = lag(DTR, n=11))
  dat <- dat%>%
    mutate(DTR12 = lag(DTR, n=12))
  dat <- dat%>%
    mutate(DTR13 = lag(DTR, n=13))
  dat <- dat%>%
    mutate(DTR14 = lag(DTR, n=14))
  dat <- dat%>%
    mutate(DTR15 = lag(DTR, n=15))
  dat <- dat%>%
    mutate(DTR16 = lag(DTR, n=16))
  dat <- dat%>%
    mutate(DTR17 = lag(DTR, n=17))
  dat <- dat%>%
    mutate(DTR18 = lag(DTR, n=18))
  dat <- dat%>%
    mutate(DTR19 = lag(DTR, n=19))
  dat <- dat%>%
    mutate(DTR20 = lag(DTR, n=20))
}
{
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
  mutate(precipitation_amount13 = lag(precipitation_amount, n=13))
dat <- dat%>%
  mutate(precipitation_amount14 = lag(precipitation_amount, n=14))
dat <- dat%>%
  mutate(precipitation_amount15 = lag(precipitation_amount, n=15))
dat <- dat%>%
  mutate(precipitation_amount16 = lag(precipitation_amount, n=16))
dat <- dat%>%
  mutate(precipitation_amount17 = lag(precipitation_amount, n=17))
dat <- dat%>%
  mutate(precipitation_amount18 = lag(precipitation_amount, n=18))
dat <- dat%>%
  mutate(precipitation_amount19 = lag(precipitation_amount, n=19))
dat <- dat%>%
  mutate(precipitation_amount20 = lag(precipitation_amount, n=20))
}

write.csv(dat,"Iquitos_total2.csv")
