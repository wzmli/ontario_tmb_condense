# ---------------------------
# Define model
#
# parameters, flows, condensation, report variables
# ---------------------------

spec_check("0.1.0", "model structure")

## Draw out diagram before doing this
## Check your work

## Defining states, and flows

# ---------------------------
# State Variables and Subcategories
# ---------------------------

# epidemiological states
epi_states = c(
  "S", "E", "Ia", "Ip", "Im", "Is", "H", "H2", "ICUs", "ICUd",
  "D", "R", "X", "V"
)

# parallel accumulators in the epi-states
accum = c("X", "V")

# non-parallel accumulator epi-states
non_accum = base::setdiff(epi_states, accum)

# non-susceptible/non-accumulator states
non_accum_non_S = base::setdiff(non_accum, "S")

# asymptomatic epi-states (those that get dosed with vaccine)
asymp_cat = c("S", "E", "Ia", "Ip", "R")

# vaccination categories/layers
vax_cat = c("unvax", "vaxdose1", "vaxprotect1", "vaxdose2", "vaxprotect2", "vaxdose3", "vaxprotect3", "vaxdose4", "vaxprotect4"
)

# dosing transitions across vaccination layers
dose_from = rep(asymp_cat, 2)
dose_to = c(asymp_cat, rep("V", length(asymp_cat)))

# ---------------------------
# Default Parameters
# ---------------------------

params = c(beta0 = beta0
	, Ca = Ca
  	, Cp = Cp
   , Cm = Cm
  	, Cs = Cs
  	, alpha = alpha
  	, sigma = sigma
  	, gamma_a = gamma_a
  	, gamma_m = gamma_m
  	, gamma_s = gamma_s
  	, gamma_p = gamma_p
  	, rho = rho
  	, delta = delta
  	, mu = mu
  	, N = N
  	, E0 = E0
  	, S0 = S0
  	, nonhosp_mort = nonhosp_mort
  	, iso_m = iso_m
  	, iso_s = iso_s
  	, phi1 = phi1
  	, phi2 = phi2
  	, psi1 = psi1
  	, psi2 = psi2
  	, psi3 = psi3
  	, c_prop = c_prop
  	, c_delay_mean = c_delay_mean
  	, c_delay_cv = c_delay_cv
  	, proc_disp = proc_disp
  	, zeta = zeta
  	, vax_dose1_inc = vax_dose1_inc
  	, vax_dose2_inc = vax_dose2_inc
  	, vax_dose3_inc = vax_dose3_inc
  	, vax_dose4_inc = vax_dose4_inc
  	, vax_response_rate = vax_response_rate
  	, vax_response_rate_R = vax_response_rate_R
  	, vax_VE_trans_dose1 = vax_VE_trans_dose1
  	, vax_alpha_dose1 = vax_alpha_dose1
  	, vax_VE_hosp_dose1 = vax_VE_hosp_dose1
  	, vax_VE_trans_dose2 = vax_VE_trans_dose2
  	, vax_alpha_dose2 = vax_alpha_dose2
  	, vax_VE_hosp_dose2 = vax_VE_hosp_dose2
  	, vax_VE_trans_dose3 = vax_VE_trans_dose3
  	, vax_alpha_dose3 = vax_alpha_dose3
  	, vax_VE_hosp_dose3 = vax_VE_hosp_dose3
  	, vax_VE_trans_dose4 = vax_VE_trans_dose4
  	, vax_alpha_dose4 = vax_alpha_dose4
  	, vax_VE_hosp_dose4 = vax_VE_hosp_dose4
  	, wane_rate = wane_rate
)

# ---------------------------
# Zero State Vector
# ---------------------------

state = layered_zero_state(epi_states, vax_cat)

# ---------------------------
# Model symbols setup
# ---------------------------

# Symbolic column vector containing all of the I-states,
# in the order Ia_unvax, Ip_unvax, ..., Ia_vaxdose1, ...
Istate = (c('Ia', 'Ip', 'Im', 'Is')
	%>% expand_names(vax_cat)
	%>% vec
)

# Symbolic column vector containing the baseline transmission rates
# for each I-state
baseline_trans_rates =	(vec('Ca'
		, 'Cp'
		, '(1 - iso_m) * (Cm)'
		, '(1 - iso_s) * (Cs)'
		)
	* struc('(beta0) * (1/N)')
)

# Symbolic matrix describing how transmission is reduced by
# vaccination status. Each row and column corresponds to one
# of the vaccination statuses. Each column is identical (for some reason)
vax_trans_red = struc_block(vec(
    	'1' # unvax
    	, '1' # vaxdose1
    	, '(1 - vax_VE_trans_dose1)' 	# vaxprotect1
    	, '(1 - vax_VE_trans_dose1)'	# vaxdose2
    	, '(1 - vax_VE_trans_dose2)' 	# vaxprotect2
    	, '(1 - vax_VE_trans_dose2)'	# vaxdose3
    	, '(1 - vax_VE_trans_dose3)'	# vaxprotect3
    	, '(1 - vax_VE_trans_dose3)' 	# vaxdose4
    	, '(1 - vax_VE_trans_dose4)'  # vaxprotect4
	 	)
   , row_times = 1
	, col_times = length(vax_cat)
)

# names of the alpha parameters for each vaccination layer
alpha = c("alpha", "alpha"
	, rep(paste0("vax_alpha_dose", 1:3), each = 2)
	, "vax_alpha_dose4"
)
# modify severity based on vax efficacy against hospitalization
severity = (vec("1", "1"
	, paste0("(",complement(c(rep(paste0("vax_VE_hosp_dose", 1:3),each = 2)
		, "vax_VE_hosp_dose4")) ,")"
		)
	) * struc(complement("mu"))
)
# map severity terms to names that can be used in expressions further below
severity_nms = "severity" %_% 1:nrow(severity)

# symbolic scalars used below
sigma   = struc("sigma")
gamma_p = struc("gamma_p")

# Symbolic vectors of rates within vaccination categories that
# depend on vaccination status
E_to_Ia_rates  = vec(           alpha ) * sigma
E_to_Ip_rates  = vec(complement(alpha)) * sigma
Ip_to_Im_rates = vec(complement(severity_nms)) * gamma_p
Ip_to_Is_rates = vec(           severity_nms) * gamma_p

# Symbolic vectors of the names of FOIs and S classes
foi_vec = vec("S" %_% vax_cat %_% "to" %_% "E" %_% vax_cat)
S_vec = vec("S" %_% vax_cat)

# ---------------------------
# Model initialization
# ---------------------------

model = (flexmodel(params = params
	, state = state
	, start_date = simulation_start_date
	, end_date = calibration_end_date
	, do_hazard = TRUE
	, do_make_state = TRUE
	)
# Flow within vaccination categories,
# with constant rates across categories
	%>% rep_rate("Ia",   "R",    ~                      (gamma_a))
   %>% rep_rate("Im",   "R",    ~                      (gamma_m))
   %>% rep_rate("Is",   "D",    ~ (    nonhosp_mort) * (gamma_s))
   %>% rep_rate("Is",   "H",    ~ (1 - nonhosp_mort) * (gamma_s) * (    phi1))
   %>% rep_rate("Is",   "X",    ~ (1 - nonhosp_mort) * (gamma_s) * (    phi1))
   %>% rep_rate("Is",   "ICUs", ~ (1 - nonhosp_mort) * (gamma_s) * (1 - phi1) * (1 - phi2))
   %>% rep_rate("Is",   "ICUd", ~ (1 - nonhosp_mort) * (gamma_s) * (1 - phi1) * (    phi2))
   %>% rep_rate("ICUs", "H2",   ~                                  (    psi1))
   %>% rep_rate("ICUd", "D",    ~                                  (    psi2))
   %>% rep_rate("H2",   "R",    ~                                  (    psi3))
   %>% rep_rate("H",    "R",    ~ (rho))

   ## add state steps (currently need to be before vec_factr)
   # Sums across vaccination categories
   %>% add_state_param_sum("asymp_unvax_N",       asymp_cat %_% "unvax")
   %>% add_state_param_sum("asymp_vaxprotect1_N", asymp_cat %_% "vaxprotect1")
   %>% add_state_param_sum("asymp_vaxprotect2_N", asymp_cat %_% "vaxprotect2")
   %>% add_state_param_sum("asymp_vaxprotect3_N", asymp_cat %_% "vaxprotect3")

   # Condensation
   %>% add_state_param_sum("Stotal", "^S" %_% alt_group(vax_cat))
   %>% add_state_param_sum("Etotal", "^E" %_% alt_group(vax_cat))
   %>% add_state_param_sum("Itotal", "^I(a|s|p|m)" %_% alt_group(vax_cat))
   %>% add_state_param_sum("Htotal", "^H2?" %_% alt_group(vax_cat))
   %>% add_state_param_sum("ICU", "^ICU(s|d)" %_% alt_group(vax_cat))
   %>% add_state_param_sum("Rtotal", "^R" %_% alt_group(vax_cat))
   %>% add_state_param_sum("Xtotal", "^X" %_% alt_group(vax_cat))
   %>% add_state_param_sum("Dtotal", "^D" %_% alt_group(vax_cat))

   # Flow within vaccination categories,
   # with rates that depend on category
   # (see struc objects created above)
   %>% vec_rate("E", "Ia",  E_to_Ia_rates)
   %>% vec_rate("E", "Ip",  E_to_Ip_rates)
   # map placeholder severity names to model params
   %>% vec_factr(severity_nms, severity)
   %>% vec_rate("Ip", "Im", Ip_to_Im_rates)
   %>% vec_rate("Ip", "Is", Ip_to_Is_rates)

   # Vaccination Response Rates
   %>% add_rate("R_vaxdose1", "R_vaxprotect1",  ~ (vax_response_rate_R))
   %>% add_rate("R_vaxdose2", "R_vaxprotect2",  ~ (vax_response_rate_R))
   %>% add_rate("R_vaxdose3", "R_vaxprotect3",  ~ (vax_response_rate_R))
   %>% add_rate("R_vaxdose4", "R_vaxprotect4",  ~ (vax_response_rate_R))
   %>% add_rate("S_vaxdose1", "S_vaxprotect1",  ~ (vax_response_rate))
   %>% add_rate("S_vaxdose2", "S_vaxprotect2",  ~ (vax_response_rate))
   %>% add_rate("S_vaxdose3", "S_vaxprotect3",  ~ (vax_response_rate))
   %>% add_rate("S_vaxdose4", "S_vaxprotect4",  ~ (vax_response_rate))

   # Forces of Infection
   %>% vec_rate("S" %_% vax_cat
		, "E" %_% vax_cat
		, kronecker(vax_trans_red, t(baseline_trans_rates)) %*% Istate
   )

   # Flow among vaccination categories
   # (see dose_* above for epi states that are involved)
   %>% rep_rate(dose_from %_% 'unvax'
		, dose_to   %_% 'vaxdose1'
		, ~ (vax_dose1_inc) * (1 / asymp_unvax_N)
	)
   %>% rep_rate(dose_from %_% 'vaxprotect1'
		, dose_to   %_% 'vaxdose2'
		, ~ (vax_dose2_inc) * (1 / asymp_vaxprotect1_N)
	)
   %>% rep_rate(dose_from %_% 'vaxprotect2'
		, dose_to   %_% 'vaxdose3'
		, ~ (vax_dose3_inc) * (1 / asymp_vaxprotect2_N)
	)
   %>% rep_rate(dose_from %_% 'vaxprotect3'
		, dose_to   %_% 'vaxdose4'
		, ~ (vax_dose4_inc) * (1 / asymp_vaxprotect3_N)
	)

   # waning (disease-based) immunity
   %>% rep_rate("R" %_% vax_cat
		, "S" %_% vax_cat
		, ~ (wane_rate)
   )

   # handle accumulators by restricting outflow appropriately
   # note: wrap_exact and alt_group are conveniences for building regex patterns
   %>% add_outflow(from = ".+" # flow from anything ...
   	, wrap_exact(alt_group(non_accum) %_% alt_group(vax_cat)) # ... to non-accumulators
   )

   # Update parameters for use with the linearized model
   ## FIXME: ASK STEVE ABOUT THIS
   %>% update_linearized_params('^N$', 1) # scale population to 1
   %>% update_linearized_params('^E0$', 1e-5)
   %>% update_linearized_params('^vax_dose1_inc$', 0)
   %>% update_linearized_params('^vax_dose2_inc$', 0)
   %>% update_linearized_params('^vax_dose3_inc$', 0)
   %>% update_linearized_params('^vax_dose4_inc$', 0)
   %>% update_linearized_params('^vax_response_rate$', 0)
   %>% update_linearized_params('^vax_response_rate_R$', 0)

   # Set the disease-free equilibrium of the linearized model
   %>% update_disease_free_state('S_unvax', 'S0')

   # Perturb the disease-free equilibrium of the linearized model
   %>% update_disease_free_state('E_unvax', 'E0')

   # Define outflows for the linearized model
   # -- confirmed that this is producing the correct indices
   %>% add_linearized_outflow("^S", "^S") # S_pos, S_pos
   %>% add_linearized_outflow(
      wrap_exact(alt_group(non_accum_non_S) %_% alt_group(vax_cat)) #notS_pos
		, wrap_exact(alt_group(non_accum)       %_% alt_group(vax_cat)) # p_states
	)
   # Define state mappings used to put the initial state values in
   # the correct positions of the initial state vector
   %>% add_state_mappings(

      # regular expression to find states to drop before computing
      # the eigenvector of the linearized system
      # -- generated indices are correct
      eigen_drop_pattern = '^(X|V)'

      # regular expression to find states to drop from the eigenvector
      # before distributing individuals among infected compartments
      # -- generated indices are correct
      , infected_drop_pattern = '^(S|D|R)'

      # regular expression to find states in the initial population
      # of susceptibles
      , initial_susceptible_pattern = wrap_exact('S_unvax')
    )

    # Set the total number of individuals and the total number of
    # infected individuals in the initial state vector
    %>% initial_population(total = 'N', infected = 'E0')

    %>% add_sim_report_expr("Incidence_unvax", ~ (S_unvax_to_E_unvax) * (S_unvax))
    %>% add_sim_report_expr("Incidence_vaxdose1", ~ (S_vaxdose1_to_E_vaxdose1) * (S_vaxdose1))
    %>% add_sim_report_expr("Incidence_vaxprotect1", ~ (S_vaxprotect1_to_E_vaxprotect1) * (S_vaxprotect1))
    %>% add_sim_report_expr("Incidence_vaxdose2", ~ (S_vaxdose2_to_E_vaxdose2) * (S_vaxdose2))
    %>% add_sim_report_expr("Incidence_vaxprotect2", ~ (S_vaxprotect2_to_E_vaxprotect2) * (S_vaxprotect2))
    %>% add_sim_report_expr("Incidence_vaxdose3", ~ (S_vaxdose3_to_E_vaxdose3) * (S_vaxdose3))
    %>% add_sim_report_expr("Incidence_vaxprotect3", ~ (S_vaxprotect3_to_E_vaxprotect3) * (S_vaxprotect3))
    %>% add_sim_report_expr("Incidence_vaxdose4", ~ (S_vaxdose4_to_E_vaxdose4) * (S_vaxdose4))
    %>% add_sim_report_expr("Incidence_vaxprotect4", ~ (S_vaxprotect4_to_E_vaxprotect4) * (S_vaxprotect4))
    %>% add_sim_report_expr("Incidence", sum(foi_vec * S_vec))
    %>% add_conv("^Incidence")
    %>% add_lag_diff("^(X|D)total$")
    %>% update_tmb_indices
    %>% update_initial_state(silent = TRUE)
)

# ---------------------------
# Script output
# ---------------------------

parameters <- addEnvironment(parameters,c("model"))
