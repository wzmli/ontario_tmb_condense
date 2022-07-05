# ---------------------------
# Produce forecast plots
# ---------------------------

## plot forecast
p1 <- (ggplot(forecast_ensemble)
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

env <- clean_env(
  env,
  c(""))
