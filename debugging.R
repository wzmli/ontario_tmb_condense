source('pipeline_parameters.R')
inv_prop = 0
source('define_model.R')
model1 <- model
sim1 <- simulation_history(model1)

inv_prop = 1
source('define_model.R')
model2 <- model
sim2 <- simulation_history(model2)

compare_sims(sim1, sim2)

## inv_prop = 0, inv_trans_adv = 1
## this is identical to the model without variant stuff
model_calibrated1 <- model_calibrated
sim3 <- simulation_history(model_calibrated1)

## this should be identical to sim3 for _any_ inv_prop between 0 and 1
sim4 <- simulation_history(
  (model_calibrated1 %>% update_params(inv_prop = 0.25))
)

compare_sims_plot(sim3, sim4, "conv_Incidence")

## check that VE is the same for resident and invader
pars <- pars_base_opt(model_calibrated1)
testthat::expect_equal(
  unname(pars[grepl("^vax_VE_trans", names(pars))]),
  unname(pars[grepl("^inv_vax_VE_trans", names(pars))])
)

#---------------------------

source('pipeline_setup.R')
source('pipeline_parameters.R')
source('define_model.R')
kron_prod <- kronecker(avg_trans_factor, t(baseline_trans_rates)) %*% Istate

#
# df <- (sim1
#   %>% pivot_longer(-Date)
#   %>% mutate(inv_prop = inv_prop)
# )
#
# (ggplot(df,
#        aes(x = Date, y = value, colour = name, shape = inv_prop))
#   + geom_point()
#   )
