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
