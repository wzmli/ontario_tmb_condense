# ---------------------------
# Prepare schedules for time-varying parameters
# generated from data
# ---------------------------

params_timevar_data <- NULL ## initialize variable name

## get time-varying parameters from data
source("scripts/params_timevar_vaxdosing.R")
source("scripts/params_timevar_invader.R")

# ---------------------------
# Script output
# ---------------------------

parameters <- addEnvironment(parameters
	, c("params_timevar_data")
)
