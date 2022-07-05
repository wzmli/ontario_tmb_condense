# Optimization variables + priors
# ---------------------------

# constant params (not time-varying)
# ---------------------------

# encoded as a function to pipe it into the initialization of the uncalibrated model object
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
  ## maybe by writing a function that uses the `as.formula(paste0())` paradigm to initializes the opt params formula and then doing something like lapply over calib_vars?
  if("report_inc" %in% calib_vars){
    model <- (model
              ## add_ to avoid overwriting
              %>% add_opt_params(
                log_nb_disp_report_inc ~ log_normal(2.7, 1)
              )
    )
  }

  if("hosp_preval" %in% calib_vars){
    model <- (model
              ## add_ to avoid overwriting
              %>% add_opt_params(
                log_nb_disp_hosp_preval ~ log_normal(10, 1)
              )
    )
  }

  if("icu_preval" %in% calib_vars){
    model <- (model
              ## add_ to avoid overwriting
              %>% add_opt_params(
                log_nb_disp_icu_preval ~ log_flat(-1)
              )
    )
  }

  return(model)
}

# time-varying parameters
# ---------------------------

# set up
# schedules for time-varying parameters
# to be optimized

params_timevar_opt <- NULL ## initialize variable name

## schedules for time-varying parameters that are being fitted
## add rows for each time-varying parameter
source("scripts/params_timevar_beta.R")
source("scripts/params_timevar_mu.R")
source("scripts/params_timevar_rho.R")

# time-varying params to optimize and priors
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
