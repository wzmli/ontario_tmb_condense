# ---------------------------
# Prep time-varying parameters during calibration period
# for input
#
# (these are not calibrated)
# ---------------------------

params_timevar = (
  params_timevar
  %>% mutate(Type = "abs")
  %>% filter(
    between(
      as.Date(Date),
      as.Date(simulation_start_date),
      as.Date(calibration_end_date) - lubridate::days(1)
    )
  )
)

# ---------------------------
# Initialize uncalibrated model with context information
# ---------------------------

model_uncalibrated = (model
  %>% update_condense_map(condense_map)
  ## attach time-varying parameters (not fitted)
  %>% add_piece_wise(params_timevar)
  ## attach observed data
  %>% update_observed(calibration_dat)
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
# Script output
# ---------------------------

parameters <- addEnvironment(parameters,c("model_uncalibrated"))

