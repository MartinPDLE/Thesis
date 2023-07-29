library(lubridate)
library(forecast)
library(dplyr)
setwd("~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/article-disease-pred-with-kcde/inst/code/estimation")

        data <- read.csv("../../../data-raw/Iquitos_total.csv")
        
        San_Juan_test <- read.csv("../../../data-raw/Iquitos_total.csv")
        
        ## Restrict to data from 1990/1991 through 2008/2009 seasons
        train_seasons <- paste0(2000:2007, "/", 2001:2008)
        data <- data[data$season %in% train_seasons, ]
        
        ## Form variable with total cases + 1 which can be logged
        data$total_cases_plus_1 <- data$total_cases + 1
        
        ## convert dates
        data$time <- mdy(data$week_start_date)
        
        #xreg <- San_Juan_test%>%
         # select(c(air_temperature,relative_humidity,precipitation_amount))
       
        
        #xreg <- as.matrix(xreg)
        #xreg1 <- xreg[1:312,]
        ## Add time_index column.  This is used for calculating the periodic kernel.
        ## Here, this is calculated as the number of days since some origin date (1970-1-1 in this case).
        ## The origin is arbitrary.
        data$time_index <- as.integer(data$time -  ymd(paste("1970", "01", "01", sep = "-")))
        
        prediction_target_var <- "total_cases_plus_1"
        
        log_prediction_target <- log(data[, prediction_target_var])
        
        
    
    
    
    seasonally_differenced_log_prediction_target <-
        ts(log_prediction_target[seq(from = 53, to = length(log_prediction_target))] -
                log_prediction_target[seq(from = 1, to = length(log_prediction_target) - 52)],
            frequency = 52)
    
    seasonally_differenced_log_sarima_fit <-Arima(seasonally_differenced_log_prediction_target, order = c(0,1,1), seasonal = list(order=c(1,0,0),period = 52), 
            method = "CSS", optim.method = "BFGS",
            include.mean = F)
        #auto.arima(seasonally_differenced_log_prediction_target, xreg = NULL)
    summary(seasonally_differenced_log_sarima_fit)
    #302 110

######
predictions_df <- data.frame(ph=rep(seq_len(52), times = 2 * 52),
	last_obs_season=rep(c("2007/2008", "2008/2009"), each = 52, times = 52),
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
			paste0(start_year, "/", start_year + 1)
		}
	)
predictions_df$last_obs_week[inds_last_obs_season_prev_year] <- 52L

inds_prediction_season_next_year <- which(predictions_df$prediction_week > 52)
predictions_df$prediction_season[inds_prediction_season_next_year] <- 
	sapply(predictions_df$prediction_season[inds_prediction_season_next_year],
		function(next_season) {
			start_year <- as.integer(substr(next_season, 1, 4)) + 1L
			paste0(start_year, "/", start_year + 1)
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
start_time <- Sys.time()
#sarima_inds <- which(predictions_df$model == "sarima" & predictions_df$ph %in% c(1, 13, 26, 39, 52))
for(predictions_df_row_ind in sarima_inds) {
	ph <- as.numeric(predictions_df$ph[predictions_df_row_ind])
	last_obs_ind <- which(San_Juan_test$season == predictions_df$last_obs_season[predictions_df_row_ind] &
		San_Juan_test$season_week == predictions_df$last_obs_week[predictions_df_row_ind])
	
	predictions_df$week_start_date[predictions_df_row_ind] <- San_Juan_test$week_start_date[last_obs_ind + as.numeric(ph)]
	
	
	#new_data <- ts(log(San_Juan_test$total_cases[seq_len(last_obs_ind)] + 1), frequency = 52)
  #updated_sj_log_sarima_fit <- Arima(new_data, model = sj_log_sarima_fit)


	new_data <- log(San_Juan_test$total_cases[seq_len(last_obs_ind)] + 1)
	seasonal_diff_new_data <- ts(new_data[seq(from = 53, to = length(new_data))] -
	    new_data[seq(from = 1, to = length(new_data) - 52)], frequency = 52)
	#xreg2 <- xreg[1:length(seasonal_diff_new_data),]
	updated_sj_log_sarima_fit <- Arima(seasonal_diff_new_data, model = seasonally_differenced_log_sarima_fit)
	
	
	
	predict_result <- predict(updated_sj_log_sarima_fit, n.ahead = ph, se.fit = T)
	#predict_result <- forecast(updated_sj_log_sarima_fit, h = ph)
	
#predictive_log_mean <- as.numeric(predict_result$pred[ph])
#predictions_df$prediction[predictions_df_row_ind] <- exp(predictive_log_mean) - 1
	
	predictive_log_mean <- as.numeric(predict_result$pred[ph]) + new_data[last_obs_ind + ph - 52]
	predictions_df$prediction[predictions_df_row_ind] <- exp(as.numeric(predict_result$pred[ph]) + new_data[last_obs_ind + ph - 52]) - 1


	predictions_df$AE[predictions_df_row_ind] <- abs(predictions_df$prediction[predictions_df_row_ind] - San_Juan_test$total_cases[last_obs_ind + ph])
	predictions_df$log_score[predictions_df_row_ind] <- dlnorm(San_Juan_test$total_cases[last_obs_ind + ph] + 1,
		meanlog = predictive_log_mean,
		sdlog = as.numeric(predict_result$se[ph]),
		log = TRUE)
	temp <- qlnorm(c(0.05, 0.25, 0.75, 0.95),
		meanlog = predictive_log_mean,
		sdlog = as.numeric(predict_result$se[ph]))
	#temp <- c(predict_result$lower[ph, 2],predict_result$lower[ph, 1],predict_result$upper[ph, 2 ],predict_result$upper[ph, 1 ])
	predictions_df[predictions_df_row_ind, c("predictive_95pct_lb", "predictive_80pct_lb", "predictive_80pct_ub", "predictive_95pct_ub")] <-
		temp - 1
	print(predictions_df_row_ind)
}
end_time <- Sys.time()
end_time - start_time

#predictions_df <- predictions_df[predictions_df$week_start_date %in% San_Juan_test$week_start_date[San_Juan_test$season %in% c("2007/2008", "2008/2009")], ]

predictions_df$ph <- as.factor(predictions_df$ph)

sarima_predictions_df <- predictions_df
save(sarima_predictions_df, file = "../../results/dengue_sj/prediction-results/sarima-predictions_iq_011_100.Rdata")
load("../../results/dengue_sj/prediction-results/sarima-predictions_iq_011_100.Rdata")
predictions_df <- sarima_predictions_df

predictions_df$week_start_date <- as.factor(predictions_df$week_start_date)
w1 <- predictions_df$prediction[predictions_df$ph==8]
pred <- San_Juan_test[365:468,]
pred$date <- mdy(pred$week_start_date)
u95 <- predictions_df$predictive_95pct_ub[predictions_df$ph==8]
l95 <- predictions_df$predictive_95pct_lb[predictions_df$ph==8]
u50 <- predictions_df$predictive_80pct_ub[predictions_df$ph==8]
l50 <- predictions_df$predictive_80pct_lb[predictions_df$ph==8]

png('~/Documents/Uni/Epidemiology/Master Thesis/Thesis/Master Thesis/Plots/Preds//Final/Sarima Dengue_8_t.png', width=2000, height=1400, res=300,pointsize = 10)
plot(x=pred$date,y=pred$total_cases,type="l", ylab ="Dengue Cases", xlab= "time",main="SARIMA 8-week Prediction Horizon",lwd=1.5,ylim = c(0,max(w1)), cex.lab=1.2)
polygon(x=c(pred$date,rev(pred$date)),y=c(u95,rev(l95)),col = rgb(0, 0, 1, alpha = 0.1), border = rgb(0, 0, 1, alpha = 0.3))
#polygon(x=c(p3d$week_start_date,rev(p3d$week_start_date)),y=c(p3d$ub50,rev(p3d$lb50)),col = rgb(1, 0, 0, alpha = 0.5), border = rgb(1, 0, 0, alpha = 1))
points(pred$date,w1, type = "l", col="blue", lwd=1.5)
abline(v=seq(as.Date("2007-07-01"), by="+6 month", length.out=5),col = "lightgray", lty = "dotted")
axis(1,at=seq(as.Date("2007-07-01"), by="+6 month", length.out=5),labels = F)
grid(NA, NULL, col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("95% PI", "prediction","obs.values"),
       col = c( rgb(0, 0, 1, alpha = 0.1),
                "blue",
                "black"), pch = c(15, 15, 15), bty = "n",cex = 1.2)
dev.off()
#points(pred$date,pred$case,type="l", col="black")
#points(pred$date,l95,type="l", col="grey")
#points(pred$date,u95,type="l", col="grey")



#library(ggplot2)
#ggplot() +
#	geom_line(aes(x = 1:468, y = total_cases, group=1), data = San_Juan_test) +
#	geom_line(aes(x = 365:468, y = w1, colour = ph), data = predictions_df[predictions_df$ph %in% 1, , drop = FALSE]) +
#	theme_bw()+
#  ggtitle("Sarima Prediction of Dengue Cases with Seasonal Differencing")+
#  ylab("Dengue Cases")+
#  xlab("Week")


sqrt(mean((pred$total_cases-w1)^2,na.rm=T))#/sqrt(mean((total_cases)^2))
t<- data.frame(w1,u95,l95,pred$total_cases)
t <- t%>%
  mutate(CI_cov = between(pred$total_cases,l95,u95))
summary(t$CI_cov)

updated_sj_log_sarima_fit$var.coef <- matrix(1, nrow = 8, ncol = 8)
