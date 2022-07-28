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
## get model output
pop_size <- unname(get_obj("model_calibrated",
                   calib_date)$params["N"])

ens <- (get_obj("forecast_ensemble", calib_date)
  %>% ungroup()
  %>% filter(var == 'recov_preval')
  %>% rename(date = Date)
  %>% select(-var, -scale_factor)
  ## rescale to seroprev scale (% of the pop)
  %>% mutate(
    population = pop_size,
    value = value/population*100,
    lwr = lwr/population*100,
    upr = upr/population*100)
  %>% select(-population)
)

## pull seroprev data
obs <- (read_csv("https://github.com/wzmli/COVID19-Canada/raw/master/seroprevalence.csv",
                 show_col_types = FALSE)
        %>% rename(date = Date)
        %>% filter(Province == "ON")
        %>% select(-Province)
        %>% mutate(across(-date, as.numeric))
        %>% pivot_longer(-date)
        %>% separate(name,
                     into = c("trash", "assay", "value_type"))
        %>% select(-trash)
)

p2 <- (ggplot(ens, aes(x = date))
       + geom_ribbon(
         aes(ymin = lwr, ymax = upr),
         alpha = 0.2, fill = 'grey30')
       + geom_line(aes(y = value), colour = 'red')
       + geom_errorbar(
         data = obs %>% filter(value_type != 'est') %>% pivot_wider(id_cols = c('date', 'assay'),
                                                                    names_from = 'value_type'),
         aes(x = date, ymin = lwr, ymax = upr, colour = assay))
       + geom_point(
         data = obs %>% filter(value_type == 'est'),
         aes(y = value, colour = assay)
       )
       + scale_x_date(expand = expansion(mult = 0),
                      limits = c(date_range[1],
                                 date_range[2] + n_days_forecast))
       + scale_y_continuous(expand = expansion(mult = 0))
       + scale_colour_manual(values = c('dodgerblue', 'forestgreen'))
       + labs(title = 'Seroprevalence (%) over time as implied by model')
       + theme(axis.title = element_blank(),
               legend.position = c(0,1),
               legend.justification = c(0.01,1),
               legend.background = element_rect(fill = NA))
)

ggsave(
  file.path('figs',
            'seroprev.png'),
  p2,
  width = 6,
  height = 0.75*6
)


# env <- clean_env(
#   env,
#   c(""))
