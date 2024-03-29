# ---------------------------
# Region-specific parameters for pipeline
# (also model specific, should include
# changeable knobs in the pipeline)
# ---------------------------

# ---------------------------
# Dates
# ---------------------------

calib_start_date <- as.Date("2020-02-01")
# calib_end_date <- today() ## don't specify anything here if you want the calibration end date to be the date of the last observation in the observed data

report_end_date <- as.Date("2021-12-28") ## when we assume the report signal stops being reliable (can't be after calibration_end_date!)

# ---------------------------
# Observations for calibration
# ---------------------------

## vector of observation names to calibrate to
## options are
## report_inc: daily infection report incidence via PCR testing
## hosp_preval: daily acute-care occupancy (prevalence), excluding ICU
## icu_preval: daily ICU occupancy (prevalence)
## if NULL, use all available
calib_vars <- c("report_inc", "hosp_preval")

## should the observations be adjusted (scaled)?
## (e.g., want to halve hospital occupancy during the omicron wave to adjust for incidental hospitalizations)
## if so, need to define a data frame stipulating these changes
## set this variable to NULL if you want to use observations as is
## can do this for multiple variables and multiple time intervals!
## if end_date is NA, will continue scaling indefinitely
obs_scaling <- data.frame(
  start_date = as.Date(c("2021-12-01")), ## need this to start where prevalence is low so that the transition looks smooth (unless you want to be fancy and implement a smoother scaling!)
  end_date = as.Date(c(NA)),
  var = c("hosp_preval"),
  scale_factor = c(0.5)
)
save_obj("obs_scaling", calib_end_date)

# ---------------------------
# Calibration settings
# ---------------------------

# beta0
# ---------------------------

# settings for beta from auto-detected breaks

n_breaks_auto_beta <- 10 ## number of breaks
log_auto_beta_prior_mean <- c(
  # -1.0563457, -2.1741484, -2.5953720,
  # -2.5761787,
  -2.1924082, -1.4564057,
  -1.6901006, -2.1915752, -1.4090154,
  -1.5156526, -1.8297348, -0.2460687,
  -0.2818156, 0.6694880) ## these priors are based on previous calibrations with non-time-varying mu

# settings for manually specified beta breaks
# (after reports drop out)
manual_beta_breaks <- c(
  as.Date("2021-12-12"),
  seq(
  as.Date("2022-01-01")
  , calib_end_date-days(1), by = 14)
)
log_manual_beta_prior_mean <- rep(-0.2818156,
                                  length(manual_beta_breaks))

log_beta_prior_mean <- c(log_auto_beta_prior_mean,
                         log_manual_beta_prior_mean)

## prior means for mu
mu_prior_mean <- c(
  ## wild-type
  0.97, 0.94, 0.98, 0.98,
  ## Alpha
  rep(0.985, 2),
  ## Delta
  rep(0.96, 2),
  ## BA.1
  0.987, 0.987,
  ## BA.2
  0.993, 0.995,
  ## BA.4/5
  0.985
)
