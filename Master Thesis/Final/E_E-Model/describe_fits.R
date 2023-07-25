 # Generate descriptive graphics for dengue analysis:

# setwd("/home/johannes/Documents/hhh4predict/Theory/Article_Theory/data_analysis_forecasting/")
setwd("dengue")

# get some helper functions:
source("../auxiliary_functions.R")
source("../basic_settings.R")

library(surveillance)
library(hhh4addon)

# get and plot data:
data("dengueSJ")
plot(dengueSJ)

# fit models:

# define controls
ctrls_dengue <- list()
ctrls_dengue$ar1 <- list(
  ar = list(f = addSeason2formula(f = ~ 1, S = 2), lag = 1),
  end = list(f = addSeason2formula(f = ~ 1, S = 1, period = 52)),
  subset = 11:(7*52),
  family = "NegBin1"
)
# for different weighting schemes:
# the max_lag values come from choose_order_dengue.R
ctrls_dengue$geom <- ctrls_dengue$ar1; ctrls_dengue$pois$funct_lag <- geometric_lag; ctrls_dengue$geom$max_lag <- 5

# fit models:
fits_dengue <- list()

fits_dengue$geom <- profile_par_lag(I,
                                    control = ctrls_dengue$geom)

# compute AICs:
AICs_dengue <- lapply(fits_dengue, AIC)

# read in the AICs with varying orders as computed in choose_order_dengue:
AICs_vary_max_lag_dengue <- read.csv("AIC/AICs_dengue_vary_max_lag_iq.csv")
ref <- AICs_vary_max_lag_dengue[1, "geom"]
AICs_vary_max_lag_dengue <- AICs_vary_max_lag_dengue - ref

# create figure:
par(mfrow = c(1, 2), las = 1, mar = c(4, 4, 0.5, 1))

# plot AICs for different values of p:
plot(2:10, AICs_vary_max_lag_dengue[2:10, "geom"], type = "b", col = cols_models_dengue["geom"],
     xlab = "p", ylab  = "improvement in AIC", pch = 15, cex = 0.9)

# plot lag weights:
lwd_weights <- 3
plot(1:5 , fits_dengue$geom$distr_lag[1:5], type = "h",ylim = c(0,0.5), xlim = c(0.9, 5.1),
     ylab = "weight",xlab = "lag / serial interval", main = "", col = cols_models_dengue["geom"], lwd = lwd_weights)




