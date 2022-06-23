## inv_prop = 0, inv_trans_adv = 1
## this is identical to the model without variant stuff
sim1 <- simulation_history(model_calibrated)

## this should be identical to sim3 for _any_ inv_prop between 0 and 1
sim2 <- simulation_history(
  (model_calibrated %>% update_params(inv_prop = 0.25))
)

compare_sims_plot(sim1, sim2, "conv_Incidence")

## check that VE is the same for resident and invader
pars <- pars_base_opt(model_calibrated)
testthat::expect_equal(
  unname(pars[grepl("^vax_VE_trans", names(pars))]),
  unname(pars[grepl("^inv_vax_VE_trans", names(pars))])
)
