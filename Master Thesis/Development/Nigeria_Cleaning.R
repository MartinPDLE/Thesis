library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tidyselect)
library(WriteXLS)

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
  plot(Measles_B$SumCas01, type="l", ylab = "Measles Cases", xlab = "Week",
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
  plot(CSM_S$SumCas01, type="l")
  
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
