attach(training)

# 
lag <- as.integer(c(0:20))
par(mfrow=c(2,2))

# dengue 
short_den <- data.frame(total_cases, total_cases1, total_cases2, total_cases3,total_cases4,total_cases5,
                        total_cases6, total_cases7, total_cases8, total_cases9,total_cases10,total_cases11,
                        total_cases12, total_cases13, total_cases14, total_cases15,total_cases16,total_cases17,
                        total_cases18, total_cases19, total_cases20)
cor(total_cases, short_den, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_den , method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly dengue cases")          
title(main="Dengue cases vs. Dengue cases")

# temp
short_temp <- data.frame(DTR, DTR1, DTR2, DTR3,DTR4,DTR5,
                         DTR6, DTR7, DTR8, DTR9,DTR10,DTR11,
                         DTR12, DTR13, DTR14, DTR15,DTR16,DTR17,
                         DTR18, DTR19, DTR20)
cor(total_cases, short_temp, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_temp, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly DTR")          
title(main="Dengue cases vs. Daily Temperature Range")

# rain
short_rain <- data.frame(precipitation_amount, precipitation_amount1, precipitation_amount2, precipitation_amount3,precipitation_amount4,precipitation_amount5,
                         precipitation_amount6, precipitation_amount7, precipitation_amount8, precipitation_amount9,precipitation_amount10,precipitation_amount11,
                         precipitation_amount12, precipitation_amount13, precipitation_amount14, precipitation_amount15,precipitation_amount16,precipitation_amount17,
                         precipitation_amount18, precipitation_amount19, precipitation_amount20)
cor(total_cases, short_rain, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_rain, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly rainfall")          
title(main="Dengue cases vs. Rainfall")

# hum
short_hum <- data.frame(relative_humidity, relative_humidity1, relative_humidity2, relative_humidity3,relative_humidity4,relative_humidity5,
                        relative_humidity6, relative_humidity7, relative_humidity8, relative_humidity9,relative_humidity10,relative_humidity11,
                        relative_humidity12, relative_humidity13, relative_humidity14, relative_humidity15,relative_humidity16,relative_humidity17,
                        relative_humidity18, relative_humidity19, relative_humidity20)
cor(total_cases, short_hum, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(total_cases, short_hum, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly adj. relative humidity")          
title(main="Dengue cases vs. relative humidity")

#campyDE
hum <- data.frame(hum, l1.hum, l2.hum, l3.hum, l4.hum,l5.hum,l6.hum,l7.hum,l8.hum,l9.hum,
                  l10.hum, l11.hum,l12.hum, l13.hum, l14.hum,l15.hum,l16.hum,l17.hum,l18.hum,
                  l19.hum, l20.hum)
cor(case, hum, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(case, hum, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly mean absolute humidity")          
title(main="Campylobacteriosis cases vs. mean absolute humidity")

case_c <- data.frame(case,case1,case2,case3,case4,case5)
cor(case, case_c, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(case, case_c, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly cases")          
title(main="Campylobacteriosis cases vs. Campylobacteriosis cases")

########

#Measles
case <- data.frame(Cases,Cases01,Cases02,Cases03,Cases04,Cases05,Cases06,Cases07,Cases08,Cases09,Cases10,Cases11,Cases12,Cases13,Cases14,Cases15,Cases16,Cases17,Cases18
                           ,Cases19,Cases20)
cor(Cases, case, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(Cases, case, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly Cases")          
title(main="Meases cases vs. Measles cases")

temp <- data.frame(temperature,temperature01,temperature02,temperature03,temperature04,temperature05,temperature06,temperature07,temperature08,temperature09,temperature10,temperature11,temperature12,temperature13,temperature14,temperature15,temperature16,temperature17,temperature18
                   ,temperature19,temperature20)
cor(Cases, temp, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(Cases, temp, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly Cases")          
title(main="Meases cases vs. temperature")

mm <- data.frame(mm,mm01,mm02,mm03,mm04,mm05,mm06,mm07,mm08,mm09,mm10,mm11,mm12,mm13,mm14,mm15,mm16,mm17,mm18
                 ,mm19,mm20)
cor(Cases, mm, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(Cases, mm, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly  rainfall")          
title(main="Meases cases vs. rainfall")

avg_hum <- data.frame(avg_hum,avg_hum01,avg_hum02,avg_hum03,avg_hum04,avg_hum05,avg_hum06,avg_hum07,avg_hum08,avg_hum09,avg_hum10,avg_hum11,avg_hum12,avg_hum13,avg_hum14,avg_hum15,avg_hum16,avg_hum17,avg_hum18
                 ,avg_hum19,avg_hum20)
cor(Cases, avg_hum, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(Cases, avg_hum, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of weekly relative humidity")          
title(main="Meases cases vs. relative humidity")


detach(training)

