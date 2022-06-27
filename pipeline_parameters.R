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
# Obervations for calibration
# ---------------------------

## vector of observation names to calibrate to
## options are
## report_inc: daily infection report incidence via PCR testing
## hosp_preval: daily acute-care occupancy (prevalence), excluding ICU
## icu_preval: daily ICU occupancy (prevalence)
calibration_vars <- c("report_inc", "hosp_preval")

# ---------------------------
# Model parameters
# ---------------------------

# base model parameters
# ---------------------------

beta0 = 0.25
Ca = 0.666666666666667
Cp = 1
Cm = 1
Cs = 1
alpha = 0.333333333333333
sigma = 0.192307692307692
gamma_a = 0.142857142857143
gamma_m = 0.142857142857143
gamma_s = 0.174825174825175
gamma_p = 2
rho = 0.1
delta = 0
mu = 0.956
N = 14e+06 ## roughly pop of ontario
E0 = 5
S0 = 1-14e-5  # initial proportion of susceptible individuals
nonhosp_mort = 0
iso_m = 0
iso_s = 0
phi1 = 0.76
phi2 = 0.5
psi1 = 0.05
psi2 = 0.125
psi3 = 0.2
c_prop = 0.1
c_delay_mean = 11
c_delay_cv = 0.25
proc_disp = 0
zeta = 0

# vaccination
# ---------------------------

## vax dosing (daily incidence a.k.a. inc)
# shut off initially
# adjusted by params_timevar
vax_dose1_inc = 0
vax_dose2_inc = 0
vax_dose3_inc = 0
vax_dose4_inc = 0

## vax immune response rates
vax_response_rate = 0.0714285714285714
vax_response_rate_R = 0.142857142857143

## dose 1 properties against wild type
vax_VE_trans_dose1 = 0.6
vax_alpha_dose1 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose1 = 0.4

## dose 2 properties against wild type
vax_VE_trans_dose2 = 0.9
vax_alpha_dose2 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose2 = 0.7

## dose 3 properties against wild type
vax_VE_trans_dose3 = 0.9
vax_alpha_dose3 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose3 = 0.9

## dose 4 properties against wild type
vax_VE_trans_dose4 = 0.9
vax_alpha_dose4 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose4 = 0.9

# infection-based immunity waning
# ---------------------------

inf_imm_wane_rate = 0.005555556 ## 1/(180 days ~ 6 months)

# variants
# ---------------------------

## these need to be set as is (used in time-varying script to get invader and resident propreties set correctly)
inv_prop = 0 ## invader proportion
trans_adv = 1 ## resident transmission advantage relative to wild type (should be 1!)

## the following are all bogus params for now, will change in params_timevar upon each invasion
inv_trans_adv = 1 ## invader transmission advantage relative to wild-type
inv_vax_VE_trans_dose1 = vax_VE_trans_dose1 ## invader VE against transmission dose 1
inv_vax_VE_trans_dose2 = vax_VE_trans_dose2 ## invader VE against transmission dose 2
inv_vax_VE_trans_dose3 = vax_VE_trans_dose3 ## invader VE against transmission dose 3
inv_vax_VE_trans_dose4 = vax_VE_trans_dose4 ## invader VE against transmission dose 4
inv_vax_VE_hosp_dose1 = vax_VE_hosp_dose1 ## invader VE against hospitalization dose 1
inv_vax_VE_hosp_dose2 = vax_VE_hosp_dose2 ## invader VE against hospitalization dose 2
inv_vax_VE_hosp_dose3 = vax_VE_hosp_dose3 ## invader VE against hospitalization dose 3
inv_vax_VE_hosp_dose4 = vax_VE_hosp_dose4 ## invader VE against hospitalization dose 4

## map to simplify variant data (bucket multiple strains under a single label)
variant_map <- data.frame(
  strain = c("Alpha", "B.1.438.1"
             , "Beta", "Gamma"
             , "Delta", "Delta AY.25", "Delta AY.27"
             , "Omicron BA.1", "Omicron BA.1.1"
             , "Omicron BA.2"
  )
  , label = c("Alpha", "Alpha"
              , "Alpha", "Alpha" ## Hack! Changing beta and gamma to alpha
              , "Delta", "Delta", "Delta"
              , "Omicron1", "Omicron1"
              , "Omicron2"
  )
)

## invading variant properties, including label, corresponding start date, end date, transmission avantage relative to resident strain at the time of invasion, and vaccine efficacies against each variant
variant_labels <- c("Alpha", "Delta", "Omicron1", "Omicron2")
invader_properties <- data.frame(
  label = variant_labels
  , start_date = as.Date(c("2020-12-07","2021-03-08","2021-11-22","2022-01-10"))
  , end_date = as.Date(c("2021-03-07","2021-11-21","2022-01-09","2022-04-04"))
  ## vax efficacy
  , inv_vax_VE_trans_dose1 = c(
    0.6 ## alpha
    , 0.3 ## delta
    , 0.15 ## BA.1
    , 0.15 ## BA.2
  )
  , inv_vax_VE_trans_dose2 = c(
    0.9, 0.9, 0.4, 0.4
  )
  , inv_vax_VE_trans_dose3 = c(
    0.9, 0.9, 0.7, 0.7
  )
  , inv_vax_VE_trans_dose4 = c(
    0.9, 0.9, 0.7, 0.7
  )
  ## transmission advantage (relative to wild type!)
  , inv_trans_adv = c(
    1.5, ## alpha relative to wt (need to find source, PHE?)
    1.5*1.8, ## delta relative to alpha (https://www.yalemedicine.org/news/covid-19-variants-of-concern-omicron)
    1.5*1.8*2.5, ## BA.1 relative to delta (no source yet)
    1.5*1.8*2.5*1.2 ## BA.2 relative to BA.1
  )
    vax_VE_hosp_dose1, length(variant_labels)
  )
  , inv_vax_VE_hosp_dose2 = rep(
    vax_VE_hosp_dose2, length(variant_labels)
  )
  , inv_vax_VE_hosp_dose3 = rep(
    vax_VE_hosp_dose3, length(variant_labels)
  )
  , inv_vax_VE_hosp_dose4 = rep(
    vax_VE_hosp_dose4, length(variant_labels)
  )
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
  c("2021-12-19" ## increase in public health restrictions
    , "2022-01-31" ## begin easing restrictions (change in capacity limits)
    , "2022-02-17" ## next phase of reopening (change in capacity limits)
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

# Error distributions for observations
# ---------------------------

## error

attach_error_dist <- function(model){

  ## TODO: make this more compact ("don't repeat yourself")
  ## maybe by writing a function that uses the `as.formula(paste0())` paradigm to initializes the opt params formula and then doing something like lapply over calibration_vars?
  if("report_inc" %in% calibration_vars){
    model <- (model
      ## only need this step if we're using the negative binomial... don't need for poisson
      %>% update_params(
        nb_disp_report_inc = 1 ## bogus value, this will be calibrated as per opt pars settings below
      )
      %>% add_error_dist(
        report_inc ~ negative_binomial("nb_disp_report_inc")
      )
    )
  }

  if("hosp_preval" %in% calibration_vars){
    model <- (model
    ## only need this step if we're using the negative binomial... don't need for poisson
      %>% update_params(
        nb_disp_hosp_preval = 1 ## bogus value, this will be calibrated as per opt pars settings below
      )
      %>% add_error_dist(
        hosp_preval ~ negative_binomial("nb_disp_hosp_preval")
      )
    )
  }

  if("icu_preval" %in% calibration_vars){
    model <- (model
    ## only need this step if we're using the negative binomial... don't need for poisson
      %>% update_params(
        nb_disp_icu_preval = 1 ## bogus value, this will be calibrated as per opt pars settings below
      )
      %>% add_error_dist(
        icu_preval ~ negative_binomial("nb_disp_icu_preval")
      )
    )
  }

  return(model)
}

# Optimization variables + priors
# ---------------------------

## these are encoded in functions because of pipeline that gets done to set up
## the uncalibrated model in calibration_settings.R

# constant params (not time-varying)
attach_opt_params <- function(model){

  model <- (model
   %>% update_opt_params(
     log_beta0 ~ log_normal(
       ## try diff mean and  variance later on when vaccines kick in

       -1.7, 1
     )
     , logit_mu ~ logit_normal(
       qlogis(0.9853643)
       , 0.01
     ) ## starting value on the logit scale
     , log_rho ~ log_normal(
       log(1/10) ## setting approx avg length of stay = 10 days
       , 0.008 ## calculated so that 5-15 days are within 1 standard deviation of the mean
     )
     )
   )

  ## optimize dispersion params for whichever observations are being included in the calibration
  ## TODO: there is probably a cleaner way to do this...
  ## maybe by writing a function that uses the `as.formula(paste0())` paradigm to initializes the opt params formula and then doing something like lapply over calibration_vars?
  if("report_inc" %in% calibration_vars){
    model <- (model
      ## add_ to avoid overwriting
      %>% add_opt_params(
        log_nb_disp_report_inc ~ log_normal(2.7, 1)
      )
    )
  }

  if("hosp_preval" %in% calibration_vars){
    model <- (model
      ## add_ to avoid overwriting
      %>% add_opt_params(
        log_nb_disp_hosp_preval ~ log_normal(10, 1)
      )
    )
  }

  if("icu_preval" %in% calibration_vars){
    model <- (model
      ## add_ to avoid overwriting
      %>% add_opt_params(
        log_nb_disp_icu_preval ~ log_flat(-1)
      )
    )
  }

  return(model)
}

# tv params
attach_opt_tv_params <- function(model){

  model <- (model
    %>% update_opt_tv_params(
      'abs'
      , as.formula(
        paste0("log_beta0 ~ log_normal(c("
        , paste(log_beta0_prior_mean, collapse = ",") ## mean
        , "), 0.25)")) ## variance
      , logit_mu ~ logit_normal(
        qlogis(0.9853643) ## mean
        , 0.1 ## variance
        ) ## starting value on the logit scale
      , log_rho ~ log_normal(
        log(1/10) ## setting approx avg length of stay = 10 days
        , 0.008 ## variance calculated so that 5-15 days are within 1 standard deviation of the mean
        )
      )
    )

  return(model)
}

## MLi: This is again setup; put the reference somewhere else?

# ---------------------------
# Condense Map
#
# map internal model variables (in the simulation history of the model defined
# in define_model.R) to names of observed variables (named in
# `observed_data.R`)
# ---------------------------

condense_map = c(
  conv_Incidence = 'report_inc',
  Htotal = 'hosp_preval',
  ICU = 'icu_preval'
)

# default map for reference:
# condense_map = c(
#   Stotal = "S",
#   Etotal = "E",
#   Itotal = "I",
#   Htotal = 'H',
#   ICU = 'ICU',
#   Rtotal = "R",
#   lag_1_diff_Xtotal = 'hosp',
#   Xtotal = "X",
#   lag_1_diff_Dtotal = 'death',
#   Dtotal = "D",
#   S_unvax_to_E_unvax = "foi_unvax",
#   S_vaxdose1_to_E_vaxdose1 = "foi_vaxdose1",
#   S_vaxprotect1_to_E_vaxprotect1 = "foi_vaxprotect1",
#   S_vaxdose2_to_E_vaxdose2 = "foi_vaxdose2",
#   S_vaxprotect2_to_E_vaxprotect2 = "foi_vaxprotect2",
#   Incidence_unvax = "incidence_unvax",
#   Incidence_vaxdose1 = "incidence_vaxdose1",
#   Incidence_vaxprotect1 = "incidence_vaxprotect1",
#   Incidence_vaxdose2 = "incidence_vaxdose2",
#   Incidence_vaxprotect2 = "incidence_vaxprotect2",
#   Incidence = 'incidence',
#   conv_Incidence_unvax = 'report_unvax',
#   conv_Incidence_vaxdose1 = 'report_vaxdose1',
#   conv_Incidence_vaxprotect1 = 'report_vaxprotect1',
#   conv_Incidence_vaxdose2 = 'report_vaxdose2',
#   conv_Incidence_vaxprotect2 = 'report_vaxprotect2',
#   conv_Incidence = 'report'
# )

# ---------------------------
# Script output
# ---------------------------

parameters <- ls()
addEnvironment(parameters,c("parameters"))
