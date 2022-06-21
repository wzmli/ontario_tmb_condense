plot_diagnostics_modelspecific <- FALSE ## flip this switch to make model-specific diagnostic plots (currently will only work if there are vax_dose params in the model)
forecast <- FALSE

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
# Calibration Setup
# ---------------------------
source("calibration_settings.R")

# ---------------------------
# Calibration
# ---------------------------
source("calibrate.R")

# ---------------------------
# Model-specific Diagnostics
#
# for calibration specifically
# ---------------------------

## MLi: If you do it under the vaccination umbrella, this will go under there

if(plot_diagnostics_modelspecific){
  source("check_vaccine_admin.R")
}

# ---------------------------
# Forecast Setup
# ---------------------------

## MLi: Everything below is beautiful!

if(forecast) source("forecast_settings.R")

# ---------------------------
# Forecast
# ---------------------------
if(forecast) source("forecast.R")

# ---------------------------
# Save environment
# ---------------------------

# i: save environment in a time-stamped file?
