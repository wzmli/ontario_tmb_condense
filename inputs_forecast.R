## number of days to forecast past the calibration end date
n_days_forecast <- 30

## new rows for params_timevar in the forecast period
params_timevar_forecast <- data.frame(
  Date = model_calibrated$end_date + 1, ## start on the date after the calibration end date
  Symbol = "beta0",
  Value = 1, ## status quo beta0 in forecast
  Type = "rel_prev"
)
