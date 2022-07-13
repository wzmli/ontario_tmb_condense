# ---------------------------
# Prep time-varying parameters during calibration period
# for input
#
# (these are not calibrated)
# ---------------------------

params_timevar = (
  bind_rows(
    params_timevar_opt # schedules for optimizing time-varying parameters
    , params_timevar_data # time-varying parameters from data (e.g. vaccine dosing)
  )
  %>% mutate(Type = "abs")
  %>% filter(
    between(
      as.Date(Date),
      calib_start_date,
      calib_end_date - lubridate::days(1)
    )
  )
)

# Attach error distributions for observations
# ---------------------------

attach_error_dist <- function(model){

  ## TODO: make this more compact ("don't repeat yourself")
  ## maybe by writing a function that uses the `as.formula(paste0())` paradigm to initializes the opt params formula and then doing something like lapply over calib_vars?
  if("report_inc" %in% calib_vars){
    model <- (model
              ## only need this step if we're using the negative binomial... don't need for poisson
              %>% update_params(
                nb_disp_report_inc = 1 ## bogus value, this will be calibrated as per opt pars settings below
              )
              %>% add_error_dist(
                report_inc ~ negative_binomial("nb_disp_report_inc")
              )
    )
  }

  if("hosp_preval" %in% calib_vars){
    model <- (model
              ## only need this step if we're using the negative binomial... don't need for poisson
              %>% update_params(
                nb_disp_hosp_preval = 1 ## bogus value, this will be calibrated as per opt pars settings below
              )
              %>% add_error_dist(
                hosp_preval ~ negative_binomial("nb_disp_hosp_preval")
              )
    )
  }

  if("icu_preval" %in% calib_vars){
    model <- (model
              ## only need this step if we're using the negative binomial... don't need for poisson
              %>% update_params(
                nb_disp_icu_preval = 1 ## bogus value, this will be calibrated as per opt pars settings below
              )
              %>% add_error_dist(
                icu_preval ~ negative_binomial("nb_disp_icu_preval")
              )
    )
  }

  return(model)
}

# ---------------------------
# Initialize uncalibrated model with context information
# ---------------------------

model_uncalibrated = (model
                      %>% update_condense_map(condense_map)
                      ## attach time-varying parameters (not fitted)
                      %>% add_piece_wise(params_timevar)
                      ## attach observed data
                      %>% update_observed(calibration_dat %>% select(date, var, value))
                      ## specify observation error distributions for observed data
                      ## distributions, specifications for parameters to optimize over for
                      ## calibration, and priors
                      %>% attach_error_dist()
                      ## specify priors for base parameters that are being fitted
                      %>% attach_opt_params()
                      ## specify priors for time-varying parameters that are being fitted
                      %>% attach_opt_tv_params()
)

# ---------------------------
# Calibrate the model to observed data
# ---------------------------

model_calibrated = calibrate_flexmodel(model_uncalibrated
                                       # , optimizer = 'nlminb'
                                       )

# ---------------------------
# Get predictions
# ---------------------------

fitted_var <- fitted(model_calibrated)

## invert scaling in fitted_var if present
if("scale_factor" %in% names(calibration_dat)){
  fitted_var <- (fitted_var
    %>% left_join(calibration_dat %>% select(-value), by = c("date", "var"))
    %>% mutate(value = value/scale_factor,
               value_fitted = value_fitted/scale_factor)
  )
}

# ---------------------------
# Script output
# ---------------------------

# env <- clean_env(
#   env,
#   c("params_timevar",
#     "model_uncalibrated",
#     "model_calibrated",
#     "fitted_var"))

## save calibrated model
saveRDS(model_calibrated,
        file = file.path("obj",
                         paste0("model_calibrated_",
                                calib_end_date,
                                ".RDS")))

## save predicted values
saveRDS(fitted_var,
        file = file.path("obj",
                         paste0("fitted_var_",
                                calib_end_date,
                                ".RDS")))

