# ---------------------------
# Calibration plots
# ---------------------------

print(convergence_info(model_calibrated))

## plot fit

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
       + labs(title = "Calibration")
       + coord_cartesian(xlim = date_range)
)

ggsave(
  file.path("figs", "calibration.png"),
  p1,
  width = fig.width,
  height = 1.3*fig.width
)

## plot fitted time-varying parameters
## get fitted time-varying parameters
fitted_tv_params <- bind_rows(
  ## pull just fitted time-varying params with their schedule
  (model_uncalibrated$timevar$piece_wise$schedule
  %>% filter(is.na(Value))
  %>% select(Date, Symbol)
  ## get fitted values values
  %>% left_join(
    (model_calibrated$timevar$piece_wise$schedule
     %>% select(Date, Symbol, Value)),
    by = c("Date", "Symbol"))
  ## lowercase names
  %>% janitor::clean_names())
)

## attach first fitted value (before first breakpoint)
param_names <- (fitted_tv_params$symbol %>% unique())

fitted_tv_params <- (bind_rows(
  fitted_tv_params,
  data.frame(
    date = calib_start_date,
    symbol = param_names,
    value = unname(model_calibrated$params[param_names])
  )
) %>% arrange(symbol, date)
  %>% mutate(plot_vline = TRUE)
)

prep_piecewise_df <- function(var, df){
  df <- df %>% filter(symbol == var)
  df2 <- (df
          %>% mutate(date = lead(date - 1),
                     plot_vline = FALSE)
          %>% replace_na(list(date = model_calibrated$end_date))
  )
  return(bind_rows(df, df2) %>% arrange(date))
}

fitted_tv_params <- bind_rows(map(
  param_names,
  prep_piecewise_df,
  df = fitted_tv_params
)) %>% arrange(symbol, date)

p2 <- (
  ggplot(fitted_tv_params,
         aes(x = date, y = value, colour = symbol))
  + geom_line()
  + facet_wrap( ~ symbol, ncol = 1, scales = "free_y")
  + geom_vline(
    aes(xintercept = date, colour = symbol),
    linetype = 'dashed',
    data = fitted_tv_params %>% filter(plot_vline)
  )
  + guides(colour = "none")
  + coord_cartesian(xlim = date_range)
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

# ---------------------------
# Model-specific diagnostics
# ---------------------------

if(plot_diagnostics_modelspecific){
  source("src/check_vaccine_admin.R")
}

# ---------------------------
# Script output
# ---------------------------

# env <- clean_env(
#   env,
#   c(""))
