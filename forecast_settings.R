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

## TODO: pull in tv_forecast sheet that has any tv params after the forecast date
## check for a new variant invasion in the forecast period!!! (in a tv_variant sheet, should be in the untrimmed dataframe processed from these sheets)
## still need to include inv_prop for newly invading variant

# ---------------------------
# Script output
# ---------------------------

parameters <- addEnvironment(parameters,
                             c("n_days_forecast",
                               "params_timevar_forecast"))
