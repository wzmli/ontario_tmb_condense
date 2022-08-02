## testing

# ---------------------------
# Prepare schedules for time-varying parameters
# generated from data
# ---------------------------
cat("loading ...\n")

params_timevar_data <- NULL ## initialize variable name

# ---------------------------
# Vaccine dosing
# ---------------------------

cat("-- vaccine dosing schedule\n")

params_timevar_vaxdosing <- get_params_timevar_vaxdosing(
  region,
  diagnostics = diagnostics
)

## add schedule
params_timevar_data <- bind_rows(
  params_timevar_data,
  params_timevar_vaxdosing
)

cat("---- loaded\n")


source(file.path("src", "params_timevar_invader.R"))

cat("all time-varying parameter schedules loaded\n\n")
