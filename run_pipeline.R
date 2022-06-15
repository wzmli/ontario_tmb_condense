plot_diagnostics_modelspecific <- TRUE ## flip this switch to make model-specific diagnostic plots (will likely break if underlying model has changed)

## Combine all three scripts?
source("package_conflict_rules.R")
source("load_libraries.R")
source("plot_settings.R") ## global plot settings

# source("inputs_variants.R")
# quit()

source("observed_data.R")
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

