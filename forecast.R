# ---------------------------
# Produce forecast using calibrated model
# ---------------------------

## attach forecast settings to calibrated model
model_to_forecast = (model_calibrated
  %>% extend_end_date(n_days_forecast)
  %>% add_piece_wise(params_timevar_forecast)
)

## simulate ensemble
forecast_ensemble = (model_to_forecast
  %>% simulate_ensemble(PDify = TRUE)
  %>% filter(var %in% unique(model_calibrated$observed$data$var))
)

## invert scaling in forecast_ensemble if present
load(
  file.path("pipeline_environments",
            paste0("calibration_env_",
                   calib_date,
                   ".Rdata"))
)
if("scale_factor" %in% names(calibration_dat)){
  forecast_ensemble <- (forecast_ensemble
     %>% rename(date = Date)
     %>% left_join(calibration_dat %>% select(-value), by = c("date", "var"))
     %>% replace_na(list(scale_factor = 1))
     %>% mutate(
       value = value/scale_factor,
       lwr = lwr/scale_factor,
       upr = upr/scale_factor
    )
   %>% rename(Date = date)
  )
}

# ---------------------------
# Script output
# ---------------------------

env <- clean_env(
  env,
  c("model_to_forecast",
    "forecast_ensemble"))

saveRDS(forecast_ensemble,
        file = file.path("results",
                         paste0("forecast_ensemble_",
                                today(),
                                ".RDS")))
