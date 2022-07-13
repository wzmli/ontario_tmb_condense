rm(list = ls()) ## start fresh

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
  calibs <- list.files("results", pattern = "^model_calibrated")
  if(length(calibs)==0) stop("no calibrations found from which to forecast. please source run_calibration.R first.")
  calib_date <- max(as.Date(str_replace(
    str_replace(calibs, "model_calibrated_", ""),
    ".RDS", "")))
}

## load calibrated model
model_calibrated <- readRDS(
  file.path("results",
            paste0("model_calibrated_",
                   calib_date,
                   ".RDS"))
)

# ---------------------------
# Initialize environment
# ---------------------------

## save names of initialized parameters in a list
env <- ls()
env <- clean_env(env, "env")

# ---------------------------
# Forecast
# ---------------------------
source("src/forecast_settings.R") ## EDIT OFTEN
source("src/forecast.R") ## EDIT NEVER
source("src/forecast_plots.R")

# ---------------------------
# Save environment
# ---------------------------

save(list = env,
     file = file.path(
       "pipeline_environments",
       paste0("forecast_env_",
              today(),
              ".Rdata")
 ))
