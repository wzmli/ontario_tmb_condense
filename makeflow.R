library(shellpipes)


plot_diagnostics_modelspecific <- FALSE ## flip this switch to make model-specific diagnostic plots (currently will only work if there are vax_dose params in the model)

# ---------------------------
# Pipeline Setup
# ---------------------------

source("pipeline_setup.R")

# ---------------------------
# Get Inputs
# ---------------------------

## MLi: What happened to the vaccine scripts?!?
## MLi: Do not roll, need to rename the files as their role
## MLi: See simple_architect.pdf
## MLi: Separate into obs and inputs
## MLi: Define vanilla model first, and it should be about to run with obs only
## MLi: We should be able to add-on, if not, then we need to talk to steve

## Parameters should be able to run stand-alone
source("parameters.R")

source("observed_data.R")

## Can we generate a bogus model without addition complexity
source("define_model.R")

## Dealing with input data
source("inputs_vaccination.R")
source("inputs_variant.R") ## roll this inot prep_tv_params.R when it's done

source("time_varying_params.R")

# ---------------------------
# Calibration Setup
# ---------------------------
source("calibration_settings.R")

# ---------------------------
# Calibration
# ---------------------------
source("calibrate.R")

quit()

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


