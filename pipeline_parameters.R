# ---------------------------
# Master parameter file for pipeline
# (model specific,
# should include changeable knobs in the pipeline)
# ---------------------------

# ---------------------------
# Dates
# ---------------------------

calib_start_date <- as.Date("2020-01-01") ## start date for each simulation in the calibration (may be before obs_start_date to enable a burn-in period before observations that we're calibrating to start)
## FIXME: should be able to make this work even if calib_start_date > min(observed_data$date)
# calib_end_date <- as.Date("2021-09-01")
calib_end_date <- today()

report_end_date <- as.Date("2021-12-15") ## when we assume the report signal stops being reliable (can't be after calibration_end_date!)

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

# ---------------------------
# Model parameters
# ---------------------------

params_url <- "https://docs.google.com/spreadsheets/d/13GBes6A2PMXITwfkyYw7E3Lt3odpb3tbiFznpVy8VhU/edit?usp=sharing"

## get default params
load_params("default", params_url)

## province-specific overwrites
load_params("overwrite_ON", params_url)

## map to simplify variant data (bucket multiple strains under a single label)
variant_map <- data.frame(
  strain = c("Alpha", "B.1.438.1"
             , "Beta", "Gamma"
             , "Delta", "Delta AY.25", "Delta AY.27"
             , "Omicron BA.1", "Omicron BA.1.1"
             , "Omicron BA.2"
  )
  ## these labels should match those used in "variant_*" sheet loaded below
  , label = c("Alpha", "Alpha"
              , "Alpha", "Alpha" ## Hack! Changing beta and gamma to alpha
              , "Delta", "Delta", "Delta"
              , "Omicron1", "Omicron1"
              , "Omicron2"
  )
)

## load invading variant properties, including label, corresponding start date, end date, and vaccine efficacies against each variant
invader_properties <- (
  read_sheet(
    params_url,
    sheet = "variant_ON",
    col_types = 'c' ## in order to parse fractional values later
  )
  %>% select(label, symbol, value)
  %>% pivot_wider(id_cols = "label",
                  names_from = "symbol")
  ## convert columns to appropriate types
  %>% mutate(across(ends_with("date"), as.Date),
             across(starts_with("inv_"), as.numeric))
  ## convert to data frame
  %>% as.data.frame()
)

# ---------------------------
# Calibration settings
# ---------------------------

# Number of break dates for beta0
# to be auto-detected from report time series
# and corresponding priors
# ---------------------------
n_breaks_beta0 <- 11 ## number of breaks
log_beta0_prior_mean <- c(
  # -1.0563457, -2.1741484, -2.5953720,
  -2.5761787, -2.1924082, -1.4564057,
  -1.6901006, -2.1915752, -1.4090154,
  -1.5156526, -1.8297348, -0.2460687,
  -0.2818156, 0.6694880) ## these priors are based on previous calibrations with non-time-varying mu
try(if(!(length(log_beta0_prior_mean) == 1 |
       length(log_beta0_prior_mean) == n_breaks_beta0)) stop("either specify only one prior mean for time-varying beta0 or exactly as many as there are desired breaks"))

# Manual break dates for beta0
# after reports drop out
# and corresponding priors
# ---------------------------
manual_beta0_breaks <- as.Date(
  c(
    # "2021-12-19" ## increase in public health restrictions
    # , "2022-01-31" ## begin easing restrictions (change in capacity limits)
    # ,
    "2022-02-17" ## next phase of reopening (change in capacity limits)
    , "2022-03-01" ## proof of vaccine mandate lifted
    , "2022-03-21" ## most indoor mask mandates lifted
  ))
manual_beta0_breaks <- manual_beta0_breaks[
  between(
  manual_beta0_breaks,
  calib_start_date,
  calib_end_date)]
log_beta0_prior_mean <- c(log_beta0_prior_mean,
                          rep(-0.2818156,
                              length(manual_beta0_breaks)))

# ---------------------------
# Script output
# ---------------------------

## save names of initialized parameters in a list
env <- ls()
env <- clean_env(env, "env")
