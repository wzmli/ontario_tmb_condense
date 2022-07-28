# ---------------------------
# Prepare schedules for time-varying parameters
# generated from data
# ---------------------------

cat("loading ...\n")

params_timevar_data <- NULL ## initialize variable name

## get time-varying parameters from data
source(file.path("src", "params_timevar_vaxdosing.R"))
source(file.path("src", "params_timevar_invader.R"))

cat("all time-varying parameter schedules loaded\n\n")
