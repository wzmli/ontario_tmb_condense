rm(list = ls()) ## start fresh

# ---------------------------
# Settings
# ---------------------------

region <- "ON" ## which region?

calibrate <- TRUE ## should we calibrate?
forecast <- TRUE ## should we forecast?

diagnostics <- TRUE ## should we output diagnostics

timing <- TRUE ## should the run be timed?
beeping <- TRUE ## should run end with a beep?

# ------------------------------------------------------

if(timing) start_time <- Sys.time()

# ---------------------------
# Pipeline Setup
# ---------------------------

source(file.path("src","pipeline_setup.R")) ## EDIT RARELY

source(file.path("src", "observed_data",
                 paste0(region, ".R"))) ## get all observations for the region (now, so that it can be used in pipeline_parameters)

source(file.path("src", "pipeline_parameters",
                 paste0(region, ".R"))) ## EDIT OFTEN
source(file.path("src", "check_pipeline_parameters.R"))

# ---------------------------
# Calibration
# ---------------------------

if(calibrate) run_calibration(region)

# ---------------------------
# Forecast
# ---------------------------

if(forecast) run_forecast(region)

# ---------------------------

if(timing){
  end_time <- Sys.time()
  print(end_time - start_time)
}

if(beeping){
  beepr::beep(sound = 5)
}
