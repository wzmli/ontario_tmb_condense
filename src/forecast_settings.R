# ---------------------------
# Define forecast settings
# ---------------------------

## number of days to forecast past the calibration end date
n_days_forecast <-30

## time-varying parameters in the forecast period
params_timevar_forecast <- data.frame(
  Date = model_calibrated$end_date + 1, ## start on the date after the calibration end date
  Symbol = "beta0",
  Value = 1, ## status quo beta0 in forecast
  Type = "rel_prev" ## make beta0 value relative to the last calibrated value (scalar multiply with entry in the Value column)
)

## number of simulations for the ensemble
n_sim <- 100 ### 5e4 produces a nice smooth median and confidence band

## add new invader variant proportion from model,
## if used in calibration
if(file.exists(file.path("obj",
                         paste0("inv_prop_model_",
                                calib_date,
                                ".RDS")))){
  get_obj("inv_prop_model", calib_date)
  df <- data.frame(
    date = seq(max(inv_prop_model$data$date) + days(1),
               max(inv_prop_model$data$date) + days(n_days_forecast),
               by = 1))
  df$inv_prop <- predict(inv_prop_model, newdata = df, type = "response")

  params_timevar_forecast <- bind_rows(
    params_timevar_forecast,
    (df
    %>% pivot_longer(-date,
                     names_to = "Symbol",
                     values_to = "Value")
    %>% rename(Date = date)
    %>% mutate(Type = 'abs')
   ))
}

# ---------------------------
# Script output
# ---------------------------

# env <- clean_env(
#   env,
#   c("n_days_forecast",
#     "params_timevar_forecast",
#     "n_sim"))
