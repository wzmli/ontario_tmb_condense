plot_diagnostics_modelspecific <- FALSE ## flip this switch to make model-specific diagnostic plots (currently will only work if there are vax_dose params in the model)

# ---------------------------
# Pipeline Setup
# ---------------------------

## MLi: combine this?!?

source("pipeline_setup.R")
source("plot_settings.R") ## global plot settings

# ---------------------------
# Get Inputs
# ---------------------------

## MLi: What happened to the vaccine scripts?!?
## MLi: Do not roll, need to rename the files as their role
## MLi: See simple_architect.pdf

source("prep_observations.R")
source("prep_tv_params.R")
source("prep_variant_data.R") ## roll this inot prep_tv_params.R when it's done

# ---------------------------
# Define Model
# ---------------------------
source("define_model.R")

# ---------------------------
# Calibration Setup
# ---------------------------
source("prep_opt_tv_params_schedule.R")
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

source("forecast_settings.R")

# ---------------------------
# Forecast
# ---------------------------
source("forecast.R")

