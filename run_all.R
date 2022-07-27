rm(list = ls()) ## start fresh

# ---------------------------
# Settings
# ---------------------------

pt <- "ON" ## which region?

calibrate <- TRUE ## should we calibrate?
forecast <- TRUE ## should we forecast?

timing <- TRUE ## should the run be timed?
beeping <- TRUE ## should run end with a beep?

# ------------------------------------------------------

if(timing) start_time <- Sys.time()

# ---------------------------
# Pipeline Setup
# ---------------------------

source(file.path("src","pipeline_setup.R")) ## EDIT RARELY
source(file.path("pipeline_parameters",
                 paste0(pt, ".R"))) ## EDIT OFTEN

# ---------------------------
# Calibration
# ---------------------------

if(calibrate){

## Load model params
source(file.path("src","get_params.R"))

## Load observed data
source(file.path("src","observed_data.R")) ## EDIT RARELY

## Define model
source(file.path("src","model.R")) ## EDIT RARELY

## Generate time-varying params from data
source(file.path("src","params_timevar_data.R")) ## EDIT RARELY

## Set up optimization parameters, schedules for time-varying ones, and priors
source(file.path("src","opt_settings.R")) ## EDIT SOMETIMES

## Calibrate and plot
source(file.path("src","calibrate.R")) ## EDIT NEVER
source(file.path("src","calibration_plots.R"))

}

# ---------------------------
# Forecast
# ---------------------------

if(forecast){

## Specify forecast settings
source(file.path("src","forecast_settings.R")) ## EDIT OFTEN

## Forecast and plot
source(file.path("src","forecast.R")) ## EDIT NEVER
source(file.path("src","forecast_plots.R"))

}

# ---------------------------

if(timing){
  end_time <- Sys.time()
  print(end_time - start_time)
}

if(beeping){
  beepr::beep(sound = 5)
}
