rm(list = setdiff(ls(), "start_time")) ## start fresh

# ---------------------------
# Pipeline Setup
# ---------------------------

source("src/pipeline_setup.R") ## EDIT RARELY

# ---------------------------
# Load calibration
# ---------------------------

## specify calibration date to use
## if NULL, use most recent
calib_date <- NULL

if(is.null(calib_date)){
  calibs <- list.files("obj", pattern = "^model_calibrated")
  if(length(calibs)==0) stop("no calibrations found from which to forecast. please source run_calibration.R first.")
  calib_date <- max(as.Date(str_replace(
    str_replace(calibs, "model_calibrated_", ""),
    ".RDS", "")))
}

## load calibrated model
get_obj("model_calibrated", calib_date)

# ---------------------------
# Initialize environment
# ---------------------------

# ## save names of initialized parameters in a list
# env <- ls()
# env <- clean_env(env, "env")

# ---------------------------
# Forecast
# ---------------------------
source("src/forecast_settings.R") ## EDIT OFTEN
source("src/forecast.R") ## EDIT NEVER
source("src/forecast_plots.R")
