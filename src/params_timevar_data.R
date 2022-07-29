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

## get vaxdosing data, tidy, plot diagnostics
df <- load_vaxdosing(region)
plot_vaxdosing(df)

## format as params_timevar lines
params_timevar_vaxdosing <- ptv_vaxdosing(df)

## diagnostic plot to check dosing
print(
  plot_ptv(params_timevar_vaxdosing)
  + labs(title = "Time-varying parameters input for vaccination")
)

## add schedule
params_timevar_data <- bind_rows(
  params_timevar_data,
  params_timevar_vaxdosing
)

cat("---- loaded\n")


source(file.path("src", "params_timevar_invader.R"))

cat("all time-varying parameter schedules loaded\n\n")
