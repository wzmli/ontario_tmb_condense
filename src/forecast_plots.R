# ---------------------------
# Produce forecast plots
# ---------------------------

get_obj("observed_data", calib_date)
calib_vars <- calibration_dat %>% pull(var) %>% unique()
## plot forecast
p1 <- (ggplot(forecast_ensemble)
       + geom_point(aes(date, value),
                    data = observed_data %>% filter(var %in% calib_vars),
                    size = 1.5, alpha = 0.2)
       + geom_ribbon(aes(Date, ymax = upr, ymin = lwr),
                     alpha = 0.2, fill = 'grey30')
       + geom_line(aes(Date, value), colour = 'red')
       # + geom_vline(
       #   aes(xintercept = Date),
       #   linetype = 'dashed',
       #   data = filter(params_timevar, Symbol == "beta0")
       # )
       + facet_wrap(
         ~ var
         , ncol = 1
         , scales = "free_y"
         , strip.position = "top"
       )
       + labs(title = "Forecast")
       + scale_x_date(limits = c(date_range[1],
                                 date_range[2] + n_days_forecast))
)
p1
ggsave(
  file.path("figs", "forecast.png"),
  p1,
  width = fig.width,
  height = 1.5*fig.width
)

# ---------------------------
# Implied seroprevalence
#
# A diagnostic plot that looks at seroprevalence as implied by the model vs estimates from blood donor data
# ---------------------------

plot_seroprev(region)