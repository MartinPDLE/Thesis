# Fitting and storing norovirus models for all timepoints at which forecasts are generated
# Gravity models (full models done in separate file)

# setwd("/home/johannes/Documents/hhh4predict/Theory/Article_Theory/data_analysis_forecasting/")
setwd("noro")
# scp johannes@130.60.71.234://home/johannes/Documents/hhh4predict/Theory/Article_Theory/data_analysis_forecasting/noro/fit_models_gravity_noro.R fit_models_gravity_noro.R

# create folder structure if necessary:
list.dirs()
dir.create("model_fits")
for(lag_structure in c("ar1", "geom", "pois", "lin", "unres")){
  dir.create(paste0("model_fits/noro_gravity_", lag_structure))
}

library(surveillance)
library(hhh4addon)

# get data:
data("noroBE")
# modify neighbourhood matrices to apply power law:
noroBE@neighbourhood <- noroBE@neighbourhood + 1

# define variable for Christmas breaks:
christmas <- numeric(nrow(noroBE@observed))
christmas[(seq_along(christmas) %% 52) %in% c(0, 1)] <- 1

# number of sine/cosine waves to include in the endemic and epidemic components:
S_end <- 1
S_epid <- 1
# min_lag and max_lag:
min_lag <- 1
max_lag <- 5
max_lag_unres <- 3
max_horizon <- 4

# subset:
subs <- (max_lag + 1):nrow(noroBE@observed)

# for the linear lag we adopt a grid-based optimization
vals_kappa_lin <- 1:99/100
vals_par_lag_lin <- log(vals_kappa_lin/(1 - vals_kappa_lin))

# controls for gravity model
ctrl_gravity <- list(end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE) + christmas, S = S_end)),
                     ar = list(f = ~-1),
                     ne = list(f = addSeason2formula(~1 + log(pop), S = S_epid),
                               weights = W_powerlaw(maxlag = 5, normalize = TRUE, log = TRUE)),
                     max_lag = max_lag,
                     family = "NegBin1",
                     subset = subs,
                     data = list(christmas = christmas, pop = noroBE@populationFrac))
ctrl_gravity_pois <- ctrl_gravity; ctrl_gravity_pois$funct_lag <- poisson_lag
ctrl_gravity_lin <- ctrl_gravity; ctrl_gravity_lin$funct_lag <- linear_lag

ctrl_gravity_unres <- ctrl_gravity; ctrl_gravity_unres$funct_lag <- unrestricted_lag
ctrl_gravity_unres$max_lag <- max_lag_unres

# timepoints for which to fit models:
tps <- (4*52 -max_horizon):(7*52)


# fit models for all time points during the evaluation period
for(ind in tps){

  # steer subset via NAs:
  noroBE_temp <- noroBE
  if(ind < nrow(noroBE)){
    noroBE_temp@observed[(ind + 1):nrow(noroBE_temp), ] <- NA
  }

  # fit ar1 model:
  fit_noro_gravity_ar1_temp <- hhh4(noroBE_temp, ctrl_gravity)
  save(fit_noro_gravity_ar1_temp, file = paste0("model_fits/noro_gravity_ar1",
                                                "/fit_noro_gravity_ar1_", ind, ".rda"))

  # fit model with geometric lags:
  start_par_lag <- ifelse(ind == tps[1], 0.5, fit_noro_gravity_geom_temp$par_lag)
  fit_noro_gravity_geom_temp <- profile_par_lag(noroBE_temp, ctrl_gravity,
                                                start_par_lag = start_par_lag)
  save(fit_noro_gravity_geom_temp, file = paste0("model_fits/noro_gravity_geom",
                                                 "/fit_noro_gravity_geom_", ind, ".rda"))

  # fit model with Poisson lags:
  start_par_lag <- ifelse(ind == tps[1], 0.5, fit_noro_gravity_pois_temp$par_lag)
  fit_noro_gravity_pois_temp <- profile_par_lag(noroBE_temp, ctrl_gravity_pois,
                                                start_par_lag = start_par_lag)
  save(fit_noro_gravity_pois_temp, file = paste0("model_fits/noro_gravity_pois",
                                                 "/fit_noro_gravity_pois_",
                                                 ind, ".rda"))

  # fit model with linear lags using grid-based optimization:
  # the linear version is fitted with optimization over a grid to avoid getting stuck in local optima:
  # start_par_lag <- ifelse(ind == tps[1], 0.5, fit_noro_gravity_lin_temp$par_lag)
  # fit_noro_gravity_lin_temp <- profile_par_lag(noroBE_temp, ctrl_gravity_lin_temp,
  #                                              start_par_lag = start_par_lag)
  fit_noro_gravity_lin_temp <- fit_par_lag(noroBE_temp, ctrl_gravity_lin,
                                           range_par = vals_par_lag_lin, use_update = FALSE)$best_mod
  save(fit_noro_gravity_lin_temp, file = paste0("model_fits/noro_gravity_lin",
                                                "/fit_noro_gravity_lin_",
                                                ind, ".rda"))

  # fit model with unconstrained weights:
  start_par_lag <- if(ind == tps[1]){
    rep(0.5, max_lag_unres - 1)
  }else{
    fit_noro_gravity_unres_temp$par_lag
  }
  fit_noro_gravity_unres_temp <- profile_par_lag(noroBE_temp, ctrl_gravity_unres,
                                                 start_par_lag = start_par_lag)
  save(fit_noro_gravity_unres_temp, file = paste0("model_fits/noro_gravity_unres",
                                                  "/fit_noro_gravity_unres_", ind, ".rda"))

  print(ind)
}




