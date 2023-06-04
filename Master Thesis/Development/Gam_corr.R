attach(training)

# 
lag <- as.integer(c(0:5))
par(mfrow=c(2,2))

# dengue 
short_den <- data.frame(total_cases, total_cases1, total_cases2, total_cases3,total_cases4,total_cases5)
cor(total_cases, short_den, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_den , method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly dengue cases")          
title(main="Dengue cases vs. Dengue cases")

# temp
short_temp <- data.frame(DTR, DTR1, DTR2, DTR3, DTR4, DTR5)
cor(total_cases, short_temp, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_temp, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly temperature reange")          
title(main="Dengue cases vs. Daily Temperature Range")

# rain
short_rain <- data.frame(precipitation_amount, precipitation_amount1,precipitation_amount2, precipitation_amount3, precipitation_amount4,precipitation_amount5)
cor(total_cases, short_rain, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_rain, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly rainfall")          
title(main="Dengue cases vs. Rainfall")

# hum
short_hum <- data.frame(relative_humidity, relative_humidity1, relative_humidity2, relative_humidity3, relative_humidity4,relative_humidity5)
cor(total_cases, short_hum, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_hum, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly adj. relative humidity")          
title(main="Dengue cases vs. relative humidity")

detach(training)

