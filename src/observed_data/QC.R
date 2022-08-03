# ---------------------------
# Prepare observations
#
# load and tidy data to which the model is being calibrated
# ---------------------------

cat("loading observed data...\n")

## Download latest observations if the cache file doesn't exist
if(!file.exists(file.path("obj", "observed_data_all.RDS"))) download_observed_data()

## Load latest observations
get_obj("observed_data_all", date = NULL)

## Update observations if last download was before today
if(attr(observed_data_all, "save_date") < today()) download_observed_data()

## Pull out observed data for the given province
observed_data <- (observed_data_all
                  %>% filter(province == region)
                  %>% select(-province)
)

## set calibration end date to date of last observation by default (can override this in the pipeline_parameters)
calib_end_date <- max(observed_data$date)

cat("observed data loaded...\n")