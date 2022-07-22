# ---------------------------
# Load calibration
# ---------------------------

## load calibrated model
get_obj("model_calibrated", calib_date)

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
  %>% simulate_ensemble(PDify = TRUE,
                        n = n_sim)
)

## invert scaling in forecast_ensemble if present
get_obj("obs_scaling", calib_date)
get_obj("calibration_dat", calib_date)

## need this to retrieve obs_scaling df
if("scale_factor" %in% names(calibration_dat)){
  ## handle scaling past calibration date (if end_date was specified as NA)
  fc_scaling <- obs_scaling %>% filter(is.na(end_date))
  keep_scaling <- nrow(fc_scaling) != 0
  if(keep_scaling){
    max_calib_dates <- (calibration_dat
                        %>% group_by(var)
                        %>% summarise(start_date = max(date) + days(1)))

    fc_scaling <- (data.frame(
      var = fc_scaling$var,
      scale_factor = fc_scaling$scale_factor
    ) %>% left_join(max_calib_dates,
                    by = "var")
    %>% mutate(end_date = start_date + days(n_days_forecast))
    ## ORDER OF COLS MATTERS FOR pmap_dfr BELOW!
    %>% relocate(start_date, end_date, .before = "var")
    )

    ## convert to long form
    ## (one value per day in range specified
    ## by start_date and end_date)
    fc_scaling_long <- pmap_dfr(fc_scaling,
                                 function(...){
                                   data.frame(
                                     date = seq.Date(..1, ..2, by = 1),
                                     var = ..3,
                                     scale_factor = ..4
                                   )
                                 })
  } else {
    fc_scaling_long <- NULL
  }

  forecast_ensemble <-
    (forecast_ensemble
     %>% rename(date = Date)
     %>% left_join(
       bind_rows(calibration_dat %>% select(-value),
                 fc_scaling_long),
       by = c("date", "var"))
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

# env <- clean_env(
  # env,
  # c("model_to_forecast",
  #   "forecast_ensemble",
  #   "calib_date"))

saveRDS(forecast_ensemble,
        file = file.path("obj",
                         paste0("forecast_ensemble_",
                                calib_date,
                                ".RDS")))

