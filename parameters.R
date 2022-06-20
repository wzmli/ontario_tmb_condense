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

## VAX DOSING (daily incidence a.k.a. inc)
# shut off initially
# adjusted by params_timevar
vax_dose1_inc = 0
vax_dose2_inc = 0
vax_dose3_inc = 0
vax_dose4_inc = 0
## VAX IMMUNE RESPONSE
vax_response_rate = 0.0714285714285714
vax_response_rate_R = 0.142857142857143
## DOSE 1 PROPERTIES
vax_VE_trans_dose1 = 0.6
vax_alpha_dose1 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose1 = 0.4
## DOSE 2 PROPERTIES
vax_VE_trans_dose2 = 0.9
vax_alpha_dose2 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose2 = 0.7
## DOSE 3 PROPERTIES
vax_VE_trans_dose3 = 0.9
vax_alpha_dose3 = 0.333333333333333 ## same as baseline
vax_VE_hosp_dose3 = 0.9
## DOSE 4 PROPERTIES
vax_VE_trans_dose4 = 0.9
= 0.333333333333333 ## same as baseline
vax_VE_hosp_dose4 = 0.9
## WANING (from disease-based immunity)
wane_rate = 0.005555556 ## 1/(180 days ~ 6 months)

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




