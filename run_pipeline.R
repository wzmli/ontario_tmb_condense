plot_diagnostics_modelspecific <- FALSE ## flip this switch to make model-specific diagnostic plots (currently will only work if there are vax_dose params in the model)
forecast <- FALSE
save_env <- FALSE

# ---------------------------
# Pipeline Setup
# ---------------------------

source("pipeline_setup.R") ## EDIT RARELY

# ---------------------------
# Get Pipeline Parameters
# ---------------------------

source("pipeline_parameters.R") ## EDIT OFTEN

# ---------------------------
# Get Pipeline Inputs
# ---------------------------

## Load observed data
source("observed_data.R") ## EDIT RARELY

## Define model
source("model.R") ## EDIT RARELY

## Generate time-varying params from data
source("params_timevar_data.R") ## EDIT RARELY

## Set up optimization parameters, schedules for time-varying ones, and priors
source("opt_pars.R") ## EDIT SOMETIMES

# ---------------------------
# Calibration
# ---------------------------
source("calibrate.R") ## EDIT NEVER
source("calibration_plots.R")

# ---------------------------
# Forecast
# ---------------------------

if(forecast){
  source("forecast_settings.R") ## EDIT OFTEN
  source("forecast.R") ## EDIT NEVER
  source("forecast_plots.R")
}

# ---------------------------
# Save environment
# ---------------------------

## FIXME: this is broken... not saving anything
# if(save_env){
#   save(list = parameters,
#        file = file.path(
#          "pipeline_environments",
#          paste0("env_",
#                today(), "_",
#                paste0(hour(now()),
#                       minute(now())),
#                ".Rdata")
#   ))
# }
