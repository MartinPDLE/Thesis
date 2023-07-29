library(readr)
library(xts)
library(plyr)
library(dplyr)
library(lubridate)
library(fitdistrplus)
library(matrixStats)
library(tidyverse)
library(FinTS)
library(tseries)
library(vars)
library(covid19italy)
library(reshape2)
library(covid19us)
library(nnfor)
library(emssm)
suppressPackageStartupMessages(library(forecast)) 
suppressPackageStartupMessages(library(dlm))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(gridExtra))

setwd("C:/Users/Martin Perez de Lema/Documents/Doks/Uni/Epidemiologie/Master Thesis/Data")
Iquitos <- read.csv("Iquitos.csv")

Iquitos <- Iquitos%>%
  dplyr::select(total_cases)

Length_Time_Series <- nrow(Iquitos)
prediction_range<-12
Training_Length<-1:(Length_Time_Series-prediction_range)
Test_Length<-(Length_Time_Series-prediction_range+1):Length_Time_Series

data_all <- Iquitos[1:Length_Time_Series,]
data_all <-as.data.frame(data_all)
data_all_training <- Iquitos[Training_Length,]
data_all_training <- as.data.frame(data_all_training)
data_all_test <- Iquitos[Test_Length,]
data_all_test <- as.data.frame(data_all_test)


###ARIMA###
par(mfrow = c(2, ceiling(ncol(data_all)/2)))
i<-1
for (i in 1:ncol(data_all_training)) {
  ARIMA_Model<-auto.arima(ts(data_all_training[,i],frequency = 7))
  rmse_arima_week_out_of_sample<-rep(0,4)
  rmse_arima_week_out_of_sample[1]<-round(MLmetrics::RMSE(forecast(ARIMA_Model,h=7)$mean,
                                                          as.numeric(data_all_test[1:7,i])),1)
  week_index<-1
  for (week_index in 1:3) {
    ARIMA_Model_week_n<-Arima(ts(c(data_all_training[,i],data_all_test[1:7*week_index,i]),frequency = 7),
                              model = ARIMA_Model)
    rmse_arima_week_out_of_sample[week_index+1]<-MLmetrics::RMSE(forecast(ARIMA_Model_week_n,h=7)$mean,
                                                                 as.numeric(data_all_test[((7*week_index)+1):(7*(week_index+1)),i]))
  }
  results_univariate_forecasting_ARIMA[i,]<-c(paste0(ARIMA_Model, " parameters=",length(ARIMA_Model[["coef"]])),
                                              round(MLmetrics::RMSE(ARIMA_Model$fitted, data_all_training[,i]),1),
                                              round(MLmetrics::RMSE(forecast(ARIMA_Model,h=prediction_range)$mean,
                                                                    as.numeric(data_all_test[,i])),1), 
                                              round(mean(rmse_arima_week_out_of_sample),1))
  {plot(as.numeric(data_all_training[,i]), type = "l", xlim = c(1, Length_Time_Series),main=
          paste(colnames(data_all_training)[i],"Time-serie evaluation:",paste0(ARIMA_Model)),xlab="Days", ylab = "Daily cases")
    lines(as.numeric(ARIMA_Model$fitted), col = "blue",lty = 2)
    lines((max(Training_Length)+1):Length_Time_Series,as.numeric(data_all_test[,i]), col = "green")
    lines((max(Training_Length)+1):Length_Time_Series,as.numeric(forecast(ARIMA_Model,h=prediction_range)$mean), 
          col = "blue",lty = 3)}
  rm(ARIMA_Model_week_n,rmse_arima_week_out_of_sample)
}
results_univariate_forecasting_ARIMA