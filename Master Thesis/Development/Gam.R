library(dlnm)
library(gam)
library(mgcv)
library(dplyr)
library(tidyverse)
library(utilities)
library(readxl)

setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/Data")
Iquitos <- read.csv("Iquitos_Training_Data.csv")
#Iquitos$week_start_date <- as.Date(Iquitos$week_start_date, origin = "%m/%d/Y")
PrecipI <- read_excel("Iquitos_Precip.xlsx", col_names = c("Year","Month","Day","mm"), col_types = c("numeric","numeric","numeric","numeric"))
PrecipI <- PrecipI[-c(1,2),]

PrecipI <- PrecipI%>%
  filter(Year>=2000 & Year<=2010)

PrecipI <- PrecipI%>%
  mutate(Date =make_date(Year,Month,Day))

PrecipI$Date <- as.Date(PrecipI$Date)
  
PrecipI <- PrecipI%>%
  select(c(4,5))

PrecipI <- PrecipI[,c(2,1)]

PrecipI <- PrecipI[PrecipI$Date >= "2000-07-01" & PrecipI$Date <= "2009-06-25",]

PrecipI <- PrecipI[-c(1339,2797),]

PrecipI <- PrecipI%>%
  select(2)

PrecipI <- rowsum(PrecipI,rep(1:468, each=7))

PrecipI <- PrecipI%>%
  mutate(id = row_number())

Iquitos <- Iquitos%>%
  mutate(id = row_number())

Iquitos <- merge(Iquitos, PrecipI, by = "id")

Iquitos <-Iquitos%>%
  select(-id)

Iquitos <- Iquitos%>%
  select(-c(denv1_cases,denv2_cases,denv3_cases,denv4_cases,other_positive_cases))

Iquitos <- Iquitos%>%
  mutate(total_cases1 = lag(total_cases, n=1))

Iquitos <- Iquitos%>%
  mutate(total_cases2 = lag(total_cases, n=2))

Iquitos <- Iquitos%>%
  mutate(total_cases3 = lag(total_cases, n=3))

Iquitos <- Iquitos%>%
  mutate(total_cases4 = lag(total_cases, n=4))

Iquitos <- Iquitos%>%
  mutate(total_cases5 = lag(total_cases, n=5))

Iquitos <- Iquitos%>%
  mutate(mm1 = lag(mm, n=1))

Iquitos <- Iquitos%>%
  mutate(mm2 = lag(mm, n=2))

Iquitos <- Iquitos%>%
  mutate(mm3 = lag(mm, n=3))

Iquitos <- Iquitos%>%
  mutate(mm4 = lag(mm, n=4))

Iquitos <- Iquitos%>%
  mutate(mm5 = lag(mm, n=5))

write.csv(Iquitos,"Iquitos.csv")


