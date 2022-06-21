# ---------------------------
# Master parameter file for pipeline
# (model specific,
# should include changeable knobs in the pipeline)
# ---------------------------

# ---------------------------
# Dates
# ---------------------------

## TODO: clean these up throughout the pipeline

start_date <- as.Date("2020-03-01")
end_date <- as.Date("2021-12-15")

min_obs_date <- as.Date("2020-02-06")
max_obs_date <- as.Date("2022-06-16")

## MLi: I don't like this, make start and end first thing when cleaning tomorrow
simulation_start_date = as.Date("2020-01-01")
calibration_end_date = as.Date("2022-06-10")

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
# calibration_vars <- c("report_inc")

# ---------------------------
# Model parameters
# ---------------------------

# base model parameters
# ---------------------------

beta0 = 0.25  # guys ... i have no idea
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

## dose 1 properties
vax_VE_trans_dose1 = 0.6
vax_alpha_dose1 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose1 = 0.4

## dose 2 properties
vax_VE_trans_dose2 = 0.9
vax_alpha_dose2 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose2 = 0.7

## dose 3 properties
vax_VE_trans_dose3 = 0.9
vax_alpha_dose3 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose3 = 0.9

## dose 4 properties
vax_VE_trans_dose4 = 0.9
vax_alpha_dose4 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose4 = 0.9

# infection-based immunity waning
# ---------------------------

wane_rate = 0.005555556 ## 1/(180 days ~ 6 months)

# variants
# ---------------------------

delta_invasion_date <- as.Date("2021-03-15")
omicron_invasion_date <- as.Date("2021-12-01")

vax_delta_VE_trans_dose1 = 0.3
vax_delta_VE_trans_dose2 = 0.8
vax_delta_VE_trans_dose3 = 0.9
vax_delta_VE_trans_dose4 = 0.9

vax_omicron_VE_trans_dose1 = 0.15
vax_omicron_VE_trans_dose2 = 0.4
vax_omicron_VE_trans_dose3 = 0.7
vax_omicron_VE_trans_dose4 = 0.7

## TODO: add all other knobs from the pipeline to this file

# ---------------------------
# Calibration settings
# ---------------------------

# Error distributions for observations
# ---------------------------

## error

attach_error_dist <- function(model){

  ## TODO: make this more compact ("don't repeat yourself")
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
      , log_beta0 ~ log_normal(
        ## from calibration with just base mu
        ## MLi: another reason not to do this is if you change the length of tvbetas, this part will fail
        ## TODO: set priors for first n beta0, fill
        ## with something general if more breakdates are specified
        c(-1.0563457, -2.1741484, -2.5953720,
          -2.5761787, -2.1924082, -1.4564057,
          -1.6901006, -2.1915752, -1.4090154,
          -1.5156526, -1.8297348, -0.2460687,
          -0.2818156,  0.6694880,
          rep(-0.2818156, 5)) ## mean
        , 0.25) ## variance
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
