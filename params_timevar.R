# ---------------------------
# Prepare schedules for time-varying parameters that are being optimized
# ---------------------------

## Users can define manually (good to have a reason)
## Separate out what is automatic and what is manual and then combine in the end

params_timevar <- NULL ## initialize variable name

## add rows for each time-varying parameter
source("scripts/params_timevar_beta.R")
source("scripts/params_timevar_mu.R")
source("scripts/params_timevar_rho.R")
source("scripts/params_timevar_vaxdosing.R")
source("scripts/params_timevar_invader.R")

# ---------------------------
# Script output
# ---------------------------

params_timevar <- (
  params_timevar
  %>% filter(
    between(Date,
            calib_start_date, calib_end_date)))

parameters <- addEnvironment(parameters
	, c("params_timevar")
)
