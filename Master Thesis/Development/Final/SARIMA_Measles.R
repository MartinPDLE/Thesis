library(lubridate)
library(forecast)
library(dplyr)
library(surveillance)
library(tidyr)
setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/article-disease-pred-with-kcde/inst/code/estimation")

        ## Load data
        data <- read.csv("../../../../Data/Bauchi_Complete.csv")
        data$Date<-lubridate::ymd( "2012-01-01" ) + lubridate::weeks( data$week - 1 )+lubridate::years(data$year - 2012)
        data$Date <- ymd(data$Date)
        data$Date <- as.Date(data$Date)
        
        Bauchi_test <- data
        xreg <- data%>%
          select(c(temperature,mm,year))
        xreg1 <- xreg[xreg$year <= 2016, , drop = FALSE]
        xreg1 <- as.matrix(xreg1)
        xmm <- xreg1[,2]
        
        data <- data[data$year <= 2016, , drop = FALSE]
        data$total_cases_plus_1 <- data$Cases + 1
        
        
        prediction_target_var <- "total_cases_plus_1"
        log_prediction_target <- log(data[, prediction_target_var])
        
        
        
        seasonally_differenced_log_prediction_target <-
          ts(log_prediction_target[seq(from = 53, to = length(log_prediction_target))] -
               log_prediction_target[seq(from = 1, to = length(log_prediction_target) - 52)],
             frequency = 52)
        
        seasonally_differenced_log_xmm <-
          ts(xmm[seq(from = 53, to = length(xmm))] -
               xmm[seq(from = 1, to = length(xmm) - 52)],
             frequency = 52)
        
        seasonally_differenced_log_sarima_fit <- auto.arima(seasonally_differenced_log_prediction_target)
          #Arima(seasonally_differenced_log_prediction_target, order = c(1,1,1), seasonal = list(order=c(1,0,0),period = 52), 
          #      method = "CSS", optim.method = "BFGS",
          #      include.mean = F)
        
        summary(seasonally_differenced_log_sarima_fit)

#Create Result Data Frame
predictions_df <- data.frame(ph=rep(seq_len(52), times = 1 * 52),
	last_obs_season=rep(c("2017"), each = 52, times = 52),
	last_obs_week=rep(seq_len(52) - 1, each = 52 * 1),
	model="sarima",
	stringsAsFactors=FALSE)
predictions_df$prediction_season <- predictions_df$last_obs_season
predictions_df$prediction_week <- predictions_df$last_obs_week + predictions_df$ph

inds_last_obs_season_prev_year <- which(predictions_df$last_obs_week == 0)
predictions_df$last_obs_season[inds_last_obs_season_prev_year] <- 
	sapply(predictions_df$last_obs_season[inds_last_obs_season_prev_year],
		function(next_season) {
			start_year <- as.integer(substr(next_season, 1, 4)) - 1L
			paste0( start_year + 1)
		}
	)
predictions_df$last_obs_week[inds_last_obs_season_prev_year] <- 52L

inds_prediction_season_next_year <- which(predictions_df$prediction_week > 52)
predictions_df$prediction_season[inds_prediction_season_next_year] <- 
	sapply(predictions_df$prediction_season[inds_prediction_season_next_year],
		function(next_season) {
			start_year <- as.integer(substr(next_season, 1, 4)) + 1L
			paste0( start_year + 1)
		}
	)
predictions_df$prediction_week[inds_prediction_season_next_year] <-
	predictions_df$prediction_week[inds_prediction_season_next_year] - 52L

predictions_df$log_score <- NA
predictions_df$prediction <- NA
predictions_df$AE <- NA
predictions_df$predictive_80pct_lb <- NA
predictions_df$predictive_80pct_ub <- NA
predictions_df$predictive_95pct_lb <- NA
predictions_df$predictive_95pct_ub <- NA
predictions_df$week_start_date <- Bauchi_test$week[1]

#Create Predictions
sarima_inds <- which(predictions_df$model == "sarima")
start_time <- Sys.time()
for(predictions_df_row_ind in sarima_inds) {
	ph <- as.numeric(predictions_df$ph[predictions_df_row_ind])
	last_obs_ind <- which(Bauchi_test$year == predictions_df$last_obs_season[predictions_df_row_ind] &
		Bauchi_test$week == predictions_df$last_obs_week[predictions_df_row_ind])
	
	predictions_df$week_start_date[predictions_df_row_ind] <- Bauchi_test$week[last_obs_ind + as.numeric(ph)]
	
	
#	new_data <- ts(log(Bauchi_test$total_cases[seq_len(last_obs_ind)] + 1), frequency = 52)
#	updated_sj_log_sarima_fit <- Arima(new_data, model = sj_log_sarima_fit)


	new_data <- log(Bauchi_test$Cases[seq_len(last_obs_ind)]+1)
	seasonal_diff_new_data <- ts(new_data[seq(from = 54, to = length(new_data))] -
	    new_data[seq(from = 1, to = length(new_data) - 53)], frequency = 53)
	updated_sj_log_sarima_fit <- Arima(seasonal_diff_new_data, model = seasonally_differenced_log_sarima_fit)
	
	
	
	#predict_result <- forecast(updated_sj_log_sarima_fit, h = ph)
	predict_result <- predict(updated_sj_log_sarima_fit, n.ahead = ph, se.fit = T)
	
#	predictive_log_mean <- as.numeric(predict_result$pred[ph])
#	predictions_df$prediction[predictions_df_row_ind] <- exp(predictive_log_mean) - 1
	
	predictive_log_mean <- as.numeric(predict_result$pred[ph]) + new_data[last_obs_ind + ph - 53]
	predictions_df$prediction[predictions_df_row_ind] <- exp(as.numeric(predict_result$pred[ph]) + new_data[last_obs_ind + ph - 53]) - 1


	predictions_df$AE[predictions_df_row_ind] <- abs(predictions_df$prediction[predictions_df_row_ind] - Bauchi_test$Cases[last_obs_ind + ph])
	predictions_df$log_score[predictions_df_row_ind] <- dlnorm(Bauchi_test$Cases[last_obs_ind + ph] + 1,
		meanlog = predictive_log_mean,
		sdlog = as.numeric(predict_result$se[ph]),
		log = TRUE)
	temp <- qlnorm(c(0.025, 0.25, 0.75, 0.975),
		meanlog = predictive_log_mean,
		sdlog = as.numeric(predict_result$se[ph]))
	predictions_df[predictions_df_row_ind, c("predictive_95pct_lb", "predictive_50pct_lb", "predictive_50pct_ub", "predictive_95pct_ub")] <-
		temp - 1
	print(predictions_df_row_ind)
}
end_time <- Sys.time()
end_time - start_time

#Save Results
sarima_predictions_df <- predictions_df
save(sarima_predictions_df, file = "../../results/dengue_sj/prediction-results/sarima-predictions_Meases_111_100.Rdata")
load("../../results/dengue_sj/prediction-results/sarima-predictions_Meases_Short_111_100.Rdata")
predictions_df <- sarima_predictions_df

predictions_df$week_start_date <- as.factor(predictions_df$week_start_date)

#Observed Values from Prediction Interval
pred <- Bauchi_test[262:313,]

#Get Results for set Prediction Horizon
w1 <- predictions_df$prediction[predictions_df$ph==1]
u95 <- predictions_df$predictive_95pct_ub[predictions_df$ph==1]
l95 <- predictions_df$predictive_95pct_lb[predictions_df$ph==1]
u50 <- predictions_df$predictive_80pct_ub[predictions_df$ph==1]
l50 <- predictions_df$predictive_80pct_lb[predictions_df$ph==1]

#Plot Predictions
png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds//Final/Sarima Measles_8_t.png', width=2000, height=1400, res=300,pointsize = 10)
plot(pred$Date,pred$Cases,type="l", ylab ="Measles Cases", xlab= "time",main="SARIMA 8-week Prediction Horizon",lwd=1.5,ylim = c(0,max(w1)), cex.lab=1.2)
polygon(x=c(pred$Date,rev(pred$Date)),y=c(u95,rev(l95)),col = rgb(0, 0, 1, alpha = 0.1), border = rgb(0, 0, 1, alpha = 0.3))
points(pred$Date,w1, type = "l", col="blue", lwd=1.5)
abline(v=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2017-01-01"), by="+2 month", length.out=7),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("95% PI", "prediction","obs.values"),
       col = c( rgb(0, 0, 1, alpha = 0.1),
                "blue",
                "black"), pch = c(15, 15, 15), bty = "n",cex = 1.2)
dev.off()

#RMSE and SRMSE for test data 
sqrt(mean((pred$Cases-w1)^2,na.rm=T))/sqrt(mean((pred$Cases)^2))
#PI Coverage
t<- data.frame(w1,u95,l95,pred$Cases)
t <- t%>%
  mutate(CI_cov = between(pred.Cases,l95,u95))
summary(t$CI_cov)


