rm(list = setdiff(ls(), "start_time")) ## start fresh

# ---------------------------
# Pipeline Setup
# ---------------------------

source("src/pipeline_setup.R") ## EDIT RARELY

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
