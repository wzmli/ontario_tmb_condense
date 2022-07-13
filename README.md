# ontario_tmb_condense

## to calibrate the model

Simply run `source("run_calibration.R")` from the main directory.

## to forecast from a calibrated model

Simply run `source("run_forecast.R")` from the main directory. By default, the code will use the most recent calibration to forecast from, but you can optionally specify a calibration date instead. A calibration file must exist in the results for the specified date, or else you will see an error.
