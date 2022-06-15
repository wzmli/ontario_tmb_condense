model_calibrated = calibrate_flexmodel(model_uncalibrated
                                       # , optimizer = 'nlminb'
                                       )
print(convergence_info(model_calibrated))

## plot fit
fitted_var <- fitted(model_calibrated)

p1 <- (fitted_var
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
  # + scale_y_continuous(trans = "log")
  + labs(title = "Calibration")
)

ggsave(
  file.path("figs", "calibration.png"),
  p1,
  width = fig.width,
  height = 1.3*fig.width
)

## plot fitted time-varying parameters
## get fitted time-varying parameters
fitted_tv_params <- (
  ## pull just fitted params with their schedule
  model_uncalibrated$timevar$piece_wise$schedule
  %>% filter(is.na(Value))
  %>% select(Date, Symbol)
  ## get fitted values values
  %>% left_join(
    (model_calibrated$timevar$piece_wise$schedule
     %>% select(Date, Symbol, Value)),
    by = c("Date", "Symbol"))
  ## lowercase names
  %>% janitor::clean_names()
)

prep_piecewise_df <- function(var, df){
  df <- df %>% filter(symbol == var)
  df2 <- (df
          %>% mutate(date = lead(date - 1))
          %>% replace_na(list(date = model_calibrated$end_date))
  )
  return(bind_rows(df, df2) %>% arrange(date))
}

fitted_tv_param_names <- (model_uncalibrated$timevar$piece_wise$schedule
                          %>% filter(is.na(Value))
                          %>% pull(Symbol)
                          %>% unique())
fitted_tv_params <- bind_rows(map(
  fitted_tv_param_names,
  prep_piecewise_df,
  df = fitted_tv_params
)) %>% arrange(symbol, date)

p2 <- (
  ggplot(fitted_tv_params,
         aes(x = date, y = value, colour = symbol))
  + geom_line()
  + facet_wrap( ~ symbol, ncol = 1, scales = "free_y")
  + geom_vline(
    aes(xintercept = Date),
    linetype = 'dashed',
    data = filter(params_timevar, Symbol == "beta0")
  )
  + guides(colour = "none")
)

p3 <- ((p1 + theme(
  axis.title.x = element_blank()
  , axis.text.x = element_blank()
  , axis.ticks.x = element_blank())
  ) / p2)

ggsave(
  file.path("figs", "calibration_with-params.png"),
  p3,
  width = fig.width,
  height = 1.5*fig.width
)

# simulate_ensemble(model_calibrated, PDify = TRUE)
