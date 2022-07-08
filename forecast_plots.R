# ---------------------------
# Produce forecast plots
# ---------------------------

## plot forecast
p1 <- (ggplot(forecast_ensemble)
       + geom_point(aes(date, value),
                    data = observed_data %>% filter(var %in% calib_vars),
                    size = 1.5, alpha = 0.2)
       + geom_ribbon(aes(Date, ymax = upr, ymin = lwr),
                     alpha = 0.2, fill = 'grey30')
       + geom_line(aes(Date, value), colour = 'red')
       + geom_vline(
         aes(xintercept = Date),
         linetype = 'dashed',
         data = filter(params_timevar, Symbol == "beta0")
       )
       + facet_wrap(
         ~ var
         , ncol = 1
         , scales = "free_y"
         , strip.position = "top"
       )
       + labs(title = "Forecast")
       + scale_x_date(limits = date_range)
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
