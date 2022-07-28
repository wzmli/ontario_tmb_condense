# ---------------------------
# Check pipeline parameters
# ---------------------------

if(calib_start_date >= min(observed_data$date)) stop("calibration start date must be strictly before date of first observation as macpan does not calibrate the initial state vector.")

if(length(log_beta_prior_mean) != n_breaks_auto_beta + length(manual_beta_breaks)) stop("the number of prior means provided for beta does not match the number of breaks (specified for auto-detection + manually).")
