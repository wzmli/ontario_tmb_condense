# ---------------------------
# Prepare schedules for time-varying parameters
# generated from data
# ---------------------------

params_timevar_data <- NULL ## initialize variable name

## get time-varying parameters from data
source("src/params_timevar_vaxdosing.R")
source("src/params_timevar_invader.R")

# ---------------------------
# Script output
# ---------------------------

# env <- clean_env(
#   env,
#   c("params_timevar_data"))
