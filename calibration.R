model_calibrated = calibrate_flexmodel(model_uncalibrated
                                       # , optimizer = 'nlminb'
                                       )
convergence_info(model_calibrated)

p1 <- (model_calibrated
  %>% fitted
  %>% ggplot
  + facet_wrap(~ var
               , ncol = 1)
  + geom_point(aes(date, value),
               alpha = 0.3, size = 1.5)
  + geom_line(aes(date, value_fitted), colour = 'red')
  + geom_vline(
    aes(xintercept = Date),
    linetype = 'dashed',
    data = filter(params_timevar, Symbol == "beta0")
  )
  + labs(title = "Calibration")
)

ggsave(
  file.path("figs", "calibration.png"),
  p1,
  width = fig.width,
  height = 1.3*fig.width
)

model_calibrated$opt_par
model_calibrated$params
filter(model_calibrated$timevar$piece_wise$schedule, Symbol == 'beta0')


# simulate_ensemble(model_calibrated, PDify = TRUE)
