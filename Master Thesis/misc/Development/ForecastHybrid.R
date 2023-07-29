library(dlnm)
library(gam)
library(mgcv)
library(dplyr)
library(tidyverse)
library(utilities)
library(readxl)
library(forecastHybrid)
library(fable)
library(forecast)

setwd("C:/Users/Martin Perez de Lema/Documents/Doks/Uni/Epidemiologie/Master Thesis/Data")
Iquitos <- read.csv("Iquitos.csv")

dat <- Iquitos%>%
  select(total_cases,mm)

train_size <- floor(0.7* nrow(dat))
train_data <- dat[1:train_size,]
test_data <- dat[(train_size +1):nrow(dat), ]

diff_order <- ndiffs(train_data$total_cases)
sdiff_order <- nsdiffs(train_data$total_cases)
sarima_model <- auto.arima(train_data$total_cases, xreg = train_data$mm, d = diff_order, seasonal = TRUE)
sarima_model2 <- auto.arima(train_data$total_cases, d = diff_order, seasonal = TRUE)
sarima_model3 <- Arima(train_data$total_cases, order = c(1,1,1), seasonal = c(1,0,1))
sarima_model3%>% forecast(h=1) %>% autoplot()

summary(sarima_model)
summary(sarima_model2)
summary(sarima_model3)
autoplot()