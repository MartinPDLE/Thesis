library(lubridate)
library(forecast)
library(dplyr)
library(surveillance)
library(tidyr)

data(campyDE)

campyDE$date <- as.Date(campyDE$date)
        ## Load data
        data <- campyDE
        
        data$time <- ymd(data$date)
        data$time <- as.Date(data$time)
        
        data <-separate(data, "time", c("Year", "Month", "Day"), sep = "-")%>%
          select(-c(Month,Day))
        data$season_week <- row_number(data$date) %% 52
        data$season_week[data$season_week==0] <- 52
        
        San_Juan_test <- data
        data <- data[data$Year <= 2009, , drop = FALSE]
        
        prediction_target_var <- "case"
        log_prediction_target <- log(data[, prediction_target_var])
        
        #seasonally_differenced_log_prediction_target <-
        #  ts(log_prediction_target[seq(from = 53, to = length(log_prediction_target))] -
        #       log_prediction_target[seq(from = 1, to = length(log_prediction_target) - 52)],
        #     frequency = 52)
        
        sj_log_sarima_fit <-
          auto.arima(log_prediction_target)
        
        summary(seasonally_differenced_log_sarima_fit)
        
        
        ## Restrict to data from 1990/1991 through 2008/2009 seasons
        #train_seasons <- paste0(2000:2007, "/", 2001:2008)
        #data <- data[data$season %in% train_seasons, ]
        
        ## Form variable with total cases + 1 which can be logged
        #data$total_cases_plus_1 <- data$total_cases + 1
        
        ## convert dates
        #data$time <- mdy(data$week_start_date)
        
        #xreg <- San_Juan_test%>%
        #  select(c(air_temperature,relative_humidity,precipitation_amount))
       
        
        #xreg <- as.matrix(xreg)
        #xreg1 <- xreg[1:312,]
        ## Add time_index column.  This is used for calculating the periodic kernel.
        ## Here, this is calculated as the number of days since some origin date (1970-1-1 in this case).
        ## The origin is arbitrary.
    
    #saveRDS(seasonally_differenced_log_sarima_fit,
    #    file = file.path(
    #        "../../../inst/results",
    #        "campyDE",
     #       "estimation-results/sarima-fit_iq2.rds"))


predictions_df <- data.frame(ph=rep(seq_len(52), times = 2 * 52),
	last_obs_season=rep(c("2010", "2011"), each = 52, times = 52),
	last_obs_week=rep(seq_len(52) - 1, each = 52 * 2),
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
predictions_df$week_start_date <- San_Juan_test$week_start_date[1]



#sj_log_sarima_fit <- sj_log_sarima_fit_manual1  ## very bad
#sj_log_sarima_fit <- sj_log_sarima_fit_manual2  ## quite bad
#sj_log_sarima_fit <- sj_log_sarima_fit_manual3  ## very bad

sarima_inds <- which(predictions_df$model == "sarima")
#sarima_inds <- which(predictions_df$model == "sarima" & predictions_df$ph %in% c(1, 13, 26, 39, 52))
for(predictions_df_row_ind in sarima_inds) {
	ph <- as.numeric(predictions_df$ph[predictions_df_row_ind])
	last_obs_ind <- which(San_Juan_test$Year == predictions_df$last_obs_season[predictions_df_row_ind] &
		San_Juan_test$season_week == predictions_df$last_obs_week[predictions_df_row_ind])
	
	predictions_df$week_start_date[predictions_df_row_ind] <- San_Juan_test$week_start_date[last_obs_ind + as.numeric(ph)]
	
	
new_data <- ts(log(San_Juan_test$case[seq_len(last_obs_ind)] + 1), frequency = 52)
	updated_sj_log_sarima_fit <- Arima(new_data, model = sj_log_sarima_fit)


#	new_data <- log(San_Juan_test$case[seq_len(last_obs_ind)]+1)
#	seasonal_diff_new_data <- ts(new_data[seq(from = 53, to = length(new_data))] -
#	    new_data[seq(from = 1, to = length(new_data) - 52)], frequency = 52)
#	updated_sj_log_sarima_fit <- Arima(seasonal_diff_new_data, model = seasonally_differenced_log_sarima_fit)
	
	
	
	#predict_result <- forecast(updated_sj_log_sarima_fit, h = ph, xreg = xreg2)
	predict_result <- predict(updated_sj_log_sarima_fit, n.ahead = ph, se.fit = T)
	
predictive_log_mean <- as.numeric(predict_result$pred[ph])
	predictions_df$prediction[predictions_df_row_ind] <- exp(predictive_log_mean) - 1
	
	#predictive_log_mean <- as.numeric(predict_result$pred[ph]) + new_data[last_obs_ind + ph - 52]
	#predictions_df$prediction[predictions_df_row_ind] <- exp(as.numeric(predict_result$pred[ph]) + new_data[last_obs_ind + ph - 52]) - 1


	predictions_df$AE[predictions_df_row_ind] <- abs(predictions_df$prediction[predictions_df_row_ind] - San_Juan_test$case[last_obs_ind + ph])
	predictions_df$log_score[predictions_df_row_ind] <- dlnorm(San_Juan_test$case[last_obs_ind + ph] + 1,
		meanlog = predictive_log_mean,
		sdlog = as.numeric(predict_result$se[ph]),
		log = TRUE)
	temp <- qlnorm(c(0.05, 0.25, 0.75, 0.95),
		meanlog = predictive_log_mean,
		sdlog = as.numeric(predict_result$se[ph]))
	predictions_df[predictions_df_row_ind, c("predictive_95pct_lb", "predictive_80pct_lb", "predictive_80pct_ub", "predictive_95pct_ub")] <-
		temp - 1
	print(predictions_df_row_ind)
}

#predictions_df <- predictions_df[predictions_df$week_start_date %in% San_Juan_test$week_start_date[San_Juan_test$season %in% c("2010,2011")], ]

predictions_df$ph <- as.factor(predictions_df$ph)

sarima_predictions_df <- predictions_df
save(sarima_predictions_df, file = "../../results/dengue_sj/prediction-results/sarima-predictions_campy.Rdata")

predictions_df$week_start_date <- as.factor(predictions_df$week_start_date)

w1 <- predictions_df$prediction[predictions_df$ph==1]
u95 <- predictions_df$predictive_95pct_ub[predictions_df$ph==1]
l95 <- predictions_df$predictive_95pct_lb[predictions_df$ph==1]


library(ggplot2)
ggplot() +
	geom_line(aes(x = 419:522, y = case), data = pred) +
	geom_line(aes(x = 419:522, y = prediction, colour ="prediction"), data = predictions_df[predictions_df$ph %in% 1, , drop = FALSE]) +
  #geom_line(aes(x=419:522, y=u95),linetype="dashed")+
  #geom_line(aes(x=419:522, y=l95),linetype="dashed")+
	theme_bw()+
  ggtitle("Sarima Prediction of Campylobacteriosis Cases")+
  ylab("Campylobacteriosis Cases")+
  xlab("Week")

pred <- San_Juan_test[419:522,]
sqrt(mean((pred$case-w1)^2,na.rm=T))/sqrt(mean((w1)^2))

updated_sj_log_sarima_fit$var.coef <- matrix(1, nrow = 8, ncol = 8)
