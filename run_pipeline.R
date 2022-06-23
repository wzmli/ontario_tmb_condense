plot_diagnostics_modelspecific <- FALSE ## flip this switch to make model-specific diagnostic plots (currently will only work if there are vax_dose params in the model)
forecast <- FALSE
save_env <- FALSE

# ---------------------------
# Pipeline Setup
# ---------------------------

source("pipeline_setup.R")

# ---------------------------
# Get Pipeline Parameters
# ---------------------------

## Ideally a user would only edit this file (unless they're also editing the model...)
source("pipeline_parameters.R")

# ---------------------------
# Get Pipeline Inputs
# ---------------------------

## Load observed data
source("observed_data.R")
## MLi: can we generate a bogus model without addition complexity?
source("define_model.R")

## Generate time-varying params both automatically from data
## and manually
source("time_varying_params.R")

# ---------------------------
# Calibration
# ---------------------------
source("calibration_settings.R")
source("calibrate.R")
source("calibration_plots.R")

# ---------------------------
# Forecast
# ---------------------------

if(forecast){
  source("forecast_settings.R")
  source("forecast.R")
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
