library(shellpipes)

plot_diagnostics_modelspecific <- FALSE ## flip this switch to make model-specific diagnostic plots (will likely break if underlying model has changed)

source("package_conflict_rules.R")
## MLi: want to get rid of plot settings, the goal of this repo is not to make pretty plots
source("plot_settings.R") ## global plot settings

source("observed_data.R")
source("inputs_vaccine.R")

quit()

source("inputs_data.R")
source("model_definition.R")
source("break_date_estimate.R")
source("context_information.R")

source("calibration.R")

if(plot_diagnostics_modelspecific){
  source("check_vaccine_admin.R")
}

source("inputs_forecast.R")
source("forecast.R")
