# ---------------------------
# Technical Options
# ---------------------------
options(MP_default_do_sim_constraint = TRUE)

# ---------------------------
# Simulation Bounds
# ---------------------------

simulation_start_date = lubridate::ymd(20200101)   # guys ... i have no idea
calibration_end_date = lubridate::ymd(20201201)  # TODO: should we infer this from calibration data?
forecast_period_days = 14   # number of days to forecast beyond calibration_end_date

# ---------------------------
# Time-Variation of Parameters in the Calibration Period
# ---------------------------

params_timevar_beta_break_dates = beta_break_dates

params_timevar = (
  bind_rows(
    params_timevar_vaccine, ## generated in inputs_vaccine.R, which is called by inputs_data.R
    params_timevar_beta_break_dates
  )
  %>% mutate(Type = "abs")
  %>% filter(
    between(
      as.Date(Date),
      as.Date(simulation_start_date),
      as.Date(calibration_end_date) - lubridate::days(1)
    )
  )
  #%>% filter(Symbol == 'beta0', Date != min(as.Date('2020-03-28')))
)

# ---------------------------
# Condense Map
#
# map variables in the simulation history of the model
# in model_definition.R to names of variables that
# you are interested in. the only technical requirement
# here is that the variables that appear in observed
# data must be present in the map
# ---------------------------

# TODO: mike really needs to check that these mappings are correct
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
# calibration data
# ---------------------------

calibration_dat = filter(
  observed_data,
  between(
    as.Date(date),
    as.Date(simulation_start_date),
    as.Date(calibration_end_date)
  )
)

# ---------------------------
# initial uncalibrated model with context information
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
  %>% add_piece_wise(params_timevar)
  %>% update_observed(filter(calibration_dat, var == 'report_inc'))
  %>% update_opt_params(log_beta0 ~ log_normal(-1.7,1)
    , log_nb_disp_report_inc ~ log_normal(10, 1)
    #, log_nb_disp_hosp_preval ~ log_flat(-1)
    #, log_nb_disp_icu_preval ~ log_flat(-1)
  )
  %>% update_opt_tv_params('abs'
    , log_beta0 ~ log_flat(
      c(-1.9676436)
      )
  )
)

# ---------------------------
# Time-Variation of Parameters in the Forecast Period
# i.e. -- scenarios
# ---------------------------


