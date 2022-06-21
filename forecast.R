# ---------------------------
# Produce forecast using calibrated model
# ---------------------------

## attach forecast settings to calibrated model
model_to_forecast = (model_calibrated
  %>% extend_end_date(n_days_forecast)
  %>% add_piece_wise(params_timevar_forecast)
)

## simulate ensemble
forecast_intervals = (model_to_forecast
  %>% simulate_ensemble(PDify = TRUE)
  %>% filter(var %in% unique(model_calibrated$observed$data$var))
)

# ---------------------------
# Script output
# ---------------------------

parameters <- addEnvironment(parameters,
                             c("model_to_forecast",
                               "forecast_intervals"))
