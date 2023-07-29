# Fitting and storing dengue models for all timepoints at which forecasts are generated


# load packages:
library(surveillance)
library(hhh4addon)

# create folder structure if necessary:
list.dirs()
dir.create("model_fits")
for(lag_structure in c("geom")){
  dir.create(paste0("model_fits/dengue_Iq", lag_structure))
}

names_lag_structures <- "geom"

# define controls for different lag weighting schemes:
max_lag_param <- 5
max_lag_unres <- 4
ctrl_dengue <- list(
  ar = list(f = addSeason2formula(f = ~ 1, S = 2), lag = 1),
  end = list(f = addSeason2formula(f = ~ 1, S = 1, period = 52)),
  family = "NegBin1",
  max_lag  = max_lag_param
)

# timepoints for which to fit models:
#tps <- (19*52 - 8):(23*52 - 1)

# fit models for all time points during the evaluation period
# we originally also experimented with models which have a min_lag larger than 1,
# i.e. force the first couple of weights to 0, but this did not yield improvements.
# therefore only run min_lag = 1
for(min_lag in 1:1){

  # adapt control settings to min_lag:
  ctrl_dengue_temp <- ctrl_dengue
  ctrl_dengue_temp$subset <- (min_lag + max_lag_param):nrow(I)
  ctrl_dengue_temp$ar$lag <- ctrl_dengue_temp$ne$lag <- min_lag
  ctrl_dengue_temp$min_lag <- min_lag
  ctrl_dengue_temp$max_lag <- min_lag + max_lag_param - 1

  # run over timepoints in validation period:
  for(ind in tps){

    # steer subset via NAs:
    dengueI_temp <- I
    if(ind < nrow(I)){
      dengueI_temp@observed[(ind + 1):nrow(dengueI_temp), ] <- NA
    }

    # fit model with geometric lags:
    start_par_lag <- ifelse(ind == tps[1], 0.5, fit_dengue_geom_temp$par_lag)
    fit_dengue_geom_temp <- profile_par_lag(dengueI_temp, ctrl_dengue_temp,
                                          start_par_lag = start_par_lag)
    save(fit_dengue_geom_temp, file = paste0("model_fits/dengue_geom",
                                           "/fit_dengue_geom_Iq", ind, ".rda"))

    print(ind)
  }
}
