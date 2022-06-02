model_to_forecast = (model_calibrated
  %>% extend_end_date(n_days_forecast)
  %>% add_piece_wise(params_timevar_forecast)
)
forecast_intervals = (model_to_forecast
  %>% simulate_ensemble(PDify = TRUE)
)
p1 <- (ggplot(forecast_intervals)
  + facet_wrap(~var)
  + geom_ribbon(aes(Date, ymax = upr, ymin = lwr),
                alpha = 0.2)
  + geom_point(aes(date, value), data = model_calibrated$observed$data %>% rename(name = var),
               size = 1.5, alpha = 0.2)
  + geom_line(aes(Date, value), colour = 'red')
  + facet_wrap(
    ~ name
    , ncol = 1
    , scales = "free_y"
    , strip.position = "top"
    )
  + labs(title = "Forecast")
)
ggsave(
  file.path("figs", "forecast.png"),
  p1,
  width = fig.width,
  height = 1.3*fig.width
)
