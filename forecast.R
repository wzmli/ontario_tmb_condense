model_to_forecast = (model_calibrated
  %>% extend_end_date(n_days_forecast)
  %>% add_piece_wise(params_timevar_forecast)
)
forecast_intervals = (model_to_forecast
  %>% simulate_ensemble(PDify = TRUE)
)
(ggplot(forecast_intervals)
  + facet_wrap(~var)
  + geom_line(aes(Date, value))
  + geom_ribbon(aes(Date, ymax = upr, ymin = lwr), alpha = 0.5)
  + geom_line(aes(date, value), data = model_calibrated$observed$data, colour = 'red')
)
