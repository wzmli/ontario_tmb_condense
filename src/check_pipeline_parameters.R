# ---------------------------
# Check pipeline parameters
# ---------------------------

if(calib_start_date >= min(observed_data$date)) stop("calibration start date must be strictly before date of first observation as macpan does not calibrate the initial state vector.")
