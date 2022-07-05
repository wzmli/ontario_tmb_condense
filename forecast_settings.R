# ---------------------------
# Define forecast settings
# ---------------------------

## number of days to forecast past the calibration end date
n_days_forecast <- 30

## time-varying parameters in the forecast period
params_timevar_forecast <- data.frame(
  Date = model_calibrated$end_date + 1, ## start on the date after the calibration end date
  Symbol = "beta0",
  Value = 1, ## status quo beta0 in forecast
  Type = "rel_prev" ## make beta0 value relative to the last calibrated value (scalar multiply with entry in the Value column)
)

# ---------------------------
# Script output
# ---------------------------

env <- clean_env(
  env,
  c("n_days_forecast",
    "params_timevar_forecast"))
