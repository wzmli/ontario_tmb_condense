# ---------------------------
# Define calibration settings
# ---------------------------

# ---------------------------
# Technical Options
# ---------------------------
options(MP_default_do_sim_constraint = TRUE)
options(MP_get_bbmle_init_from_nlminb = TRUE)

# ---------------------------
# Simulation Bounds
# ---------------------------

simulation_start_date = lubridate::ymd(20200101)
calibration_end_date = lubridate::ymd(20220610)  # TODO: should we infer this from calibration data?

# ---------------------------
# Time-Variation of Parameters in the Calibration Period
# ---------------------------

params_timevar = (
  bind_rows(
    params_timevar_vaccine ## the dosing schedule, generated in inputs_vaccine.R, which is called by inputs_data.R
    , params_timevar_beta
    , params_timevar_mu
    , params_timevar_rho
    , params_timevar_VE ## VE changes based on variant invasion
  )
  %>% mutate(Type = "abs")
  %>% filter(
    between(
      as.Date(Date),
      as.Date(simulation_start_date),
      as.Date(calibration_end_date) - lubridate::days(1)
    )
  )
)

# ---------------------------
# Condense Map
#
# map internal model variables (in the simulation history of the model defined
# in model_definition.R) to names of observed variables (named in
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
# Calibration Data
# ---------------------------

calibration_dat = filter(observed_data
  ## filter to calibration period
  , between(
    as.Date(date),
    as.Date(simulation_start_date),
    as.Date(calibration_end_date)
  )
  # ,
  ## remove reports after date when testing became unreliable
  , !(var == "report_inc" & date > lubridate::ymd(20211215))
  ## remove ICU prevalence
  , !(var == "icu_preval")
)

p1 <- (ggplot(calibration_dat %>% drop_na()
        , aes(x = date, y = value, colour = var))
  + geom_point(alpha = 0.3, size = 1.5)
  + facet_wrap(
    ~ var
    , ncol = 1
    , scales = "free_y"
)
  + labs(title = "Observed data used in calibration")
  + guides(colour = "none")
)
ggsave(
  file.path("figs", "context_information-observed_data.png")
  , p1
  , width = fig.width
  , height = 1.3*fig.width
)

# ---------------------------
# Initial uncalibrated model with context information
# ---------------------------

model_uncalibrated = (flexmodel(
    params = params,
    state = state,
    start_date = simulation_start_date,
    end_date = calibration_end_date,
    do_hazard = TRUE,
    do_make_state = TRUE
  )
  %>% add_model_structure
  %>% update_condense_map(condense_map)
  ## attach time-varying parameters (not fitted)
  %>% add_piece_wise(params_timevar)
  ## attach observed data
  %>% update_observed(
    calibration_dat
  )
  ## specify observation error distributions for observed data
  %>% update_params(
    nb_disp_report_inc = 1
    , nb_disp_hosp_preval = 100
  )
  %>% update_error_dist(
    report_inc ~ negative_binomial("nb_disp_report_inc")
    , hosp_preval ~ negative_binomial("nb_disp_hosp_preval")
  )
  ## specify priors for base parameters that are being fitted
  %>% update_opt_params(log_beta0 ~ log_normal(
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
    , log_nb_disp_report_inc ~ log_normal(2.7, 1)
    , log_nb_disp_hosp_preval ~ log_normal(10, 1)
    #, log_nb_disp_icu_preval ~ log_flat(-1)
  )
  ## specify priors for time-varying parameters that are being fitted
  %>% update_opt_tv_params('abs'
    , log_beta0 ~ log_normal(
      ## from calibration with just base mu
      c(-1.0563457, -2.1741484, -2.5953720,
        -2.5761787, -2.1924082, -1.4564057,
        -1.6901006, -2.1915752, -1.4090154,
        -1.5156526, -1.8297348, -0.2460687,
        -0.2818156,  0.6694880, rep(-0.2818156, 5)),
        0.25
      )
    , logit_mu ~ logit_normal(
      qlogis(0.9853643)
      , 0.1
    ) ## starting value on the logit scale
    , log_rho ~ log_normal(
      log(1/10) ## setting approx avg length of stay = 10 days
      , 0.008 ## calculated so that 5-15 days are within 1 standard deviation of the mean
    )
  )
)
