plot_diagnostics_modelspecific <- FALSE ## flip this switch to make model-specific diagnostic plots (currently will only work if there are vax_dose params in the model)

# ---------------------------
# Pipeline Setup
# ---------------------------

source("src/pipeline_setup.R") ## EDIT RARELY

# ---------------------------
# Get Pipeline Parameters
# ---------------------------

source("src/pipeline_parameters.R") ## EDIT OFTEN

# ---------------------------
# Get Pipeline Inputs
# ---------------------------

## Load model params
source("src/get_params.R")

## Load observed data
source("src/observed_data.R") ## EDIT RARELY

## Define model
source("src/model.R") ## EDIT RARELY

## Generate time-varying params from data
source("src/params_timevar_data.R") ## EDIT RARELY

## Set up optimization parameters, schedules for time-varying ones, and priors
source("src/opt_settings.R") ## EDIT SOMETIMES

# ---------------------------
# Calibration
# ---------------------------
source("src/calibrate.R") ## EDIT NEVER
source("src/calibration_plots.R")

# ---------------------------
# Save environment
# ---------------------------

save(list = env,
       file = file.path(
         "pipeline_environments",
         paste0("calibration_env_",
               today(),
               ".Rdata")
))
