source('context_information.R')

model_calibrated = calibrate_flexmodel(model_uncalibrated, optimizer = 'nlminb')
convergence_info(model_calibrated)

(model_calibrated
  %>% fitted
  %>% ggplot
  + facet_wrap(~var)
  + geom_line(aes(date, value))
  + geom_line(aes(date, value_fitted), colour = 'red')
  + geom_vline(
    aes(xintercept = Date),
    linetype = 'dashed',
    data = filter(params_timevar, Symbol == "beta0")
  )
)

model_calibrated$opt_par
model_calibrated$params
filter(model_calibrated$timevar$piece_wise$schedule, Symbol == 'beta0')


simulate_ensemble(model_calibrated, PDify = TRUE)
