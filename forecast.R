# ---------------------------
# Produce forecast using calibrated model
# ---------------------------

## attach forecast settings to calibrated model
model_to_forecast = (model_calibrated
  %>% extend_end_date(n_days_forecast)
  %>% add_piece_wise(params_timevar_forecast)
)

## simulate ensemble
forecast_intervals = (model_to_forecast
  %>% simulate_ensemble(PDify = TRUE)
  %>% filter(var %in% unique(model_calibrated$observed$data$var))
)

## plot
p1 <- (ggplot(forecast_intervals)
  + geom_point(aes(date, value),
               data = model_calibrated$observed$data,
               size = 1.5, alpha = 0.2)
  + geom_ribbon(aes(Date, ymax = upr, ymin = lwr),
                alpha = 0.2, fill = 'grey30')
  + geom_line(aes(Date, value), colour = 'red')
  + facet_wrap(
    ~ var
    , ncol = 1
    , scales = "free_y"
    , strip.position = "top"
    )
  + labs(title = "Forecast")
)
p1
ggsave(
  file.path("figs", "forecast.png"),
  p1,
  width = fig.width,
  height = 1.3*fig.width
)
