# compare different orders p for dengue models

setwd("dengue")
setwd("~/Doks/Uni/Epidemiologie/Master Thesis/Thesis-Code/dengue")
Iquitos <- read.csv("Iquitos_total2.csv")
I <- sts(Iquitos$total_cases)
#
campyDE <- data("campyDE")
I <- sts(campyDE$case)
#
Bauchi <- read.csv("Measles_Bauchi.csv")
Bauchi <- Bauchi[53:313,] #delete first year due to under reporting
I <- sts(Bauchi$Cases)

# load packages:
library(RColorBrewer)
library(surveillance)
library(hhh4addon)

# get and plot data:

names_lag_structures <- c("geom")
cols_lag_structures <- brewer.pal(8, "Dark2")[c(8, 4:7)]
names(cols_lag_structures) <- names_lag_structures

plot(I)

# define controls for different lag weighting schemes:
ctrls_dengue <- list()
ctrls_dengue$ar1 <- list(
  ar = list(f = addSeason2formula(f = ~ 1, S = 2), lag = 1),
  end = list(f = addSeason2formula(f = ~ 1, S = 1, period = 52)),
  subset = 11:(9*52), #Length of training data
  family = "NegBin1"
)
ctrls_dengue$geom <- ctrls_dengue$ar1; ctrls_dengue$pois$funct_lag = geometric_lag; ctrls_dengue$geom$max_lag <- 10


# fit models varying order p:

fit_dengue_ar1 <- hhh4(I, ctrls_dengue$ar1)

fits_dengue_vary_max_lag <- list()

fits_dengue_vary_max_lag$geom[[1]] <- fit_dengue_ar1

for(max_lag in 2:10){
  ctrls_dengue$geom$max_lag <- max_lag
  fits_dengue_vary_max_lag$geom[[max_lag]] <- profile_par_lag(I, ctrls_dengue$geom)

  print(max_lag)
}

lapply(fits_dengue_vary_max_lag$unres, function(x) x$convergence_profile)
# warnings stem from unrestricted with high lags.

save(fits_dengue_vary_max_lag, file = "model_fits/fits_denge_vary_max_lag_iq.rda")
load("model_fits/fits_denge_vary_max_lag_iq.rda")

AICs_vary_max_lag_dengue <- matrix(ncol = 2, nrow = 10,
                                   dimnames = list(NULL, c("max_lag","geom")))
AICs_vary_max_lag_dengue[, "max_lag"] <- 1:10
AICs_vary_max_lag_dengue[, "geom"] <- unlist(lapply(fits_dengue_vary_max_lag$geom, AIC))




write.csv(AICs_vary_max_lag_dengue, file = "AIC/AICs_dengue_vary_max_lag_iq.csv", row.names = FALSE)
