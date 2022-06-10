## THIS IS A MODEL-SPECIFIC SCRIPT
## works with vaccine models as set up by irena :)

## compare vaccine administration as input
## to resulting vaccine admin in simulation (post-calibration)

## input vaccine data
vaccine_obs <- (
  model_calibrated$timevar$piece_wise$schedule
  %>% filter(str_detect(Symbol, "^vax_"))
  %>% select(Date, Symbol, Value)
  %>% mutate(Data_Type = "observed")
)

## check vaccine administration
sim <- simulation_history(model_calibrated)

## simulated
vaccine_sim <- (sim
  ## keep only vaccine accumulators in dosing states
  %>% select(Date,
             matches("^V_[[:alpha:]]+dose"))
  ## get incident dosing
  %>% mutate(
    across(where(is.numeric),
           ~ .x - lag(.x),
           .names = "{.col}_inc")
  )
  ## keep only incidence
  %>% select("Date", ends_with("_inc"))
  ## replace NA with 0 for all numeric columns
  %>% mutate(across(where(is.numeric),
                      ~ ifelse((.x < 0 | is.na(.x)), 0, .x)))
  %>% pivot_longer(
    -Date
    , names_to = "Symbol"
    , values_to = "Value")
  # convert state names to parameter names
  %>% mutate(Symbol = str_replace(Symbol,
                                  "V_vax",
                                  "vax_"))
  %>% mutate(Data_Type = "simulated")
)

vaccine_check <- (bind_rows(
  vaccine_obs,
  vaccine_sim
  )
)

p1 <- (ggplot(vaccine_check
  , aes(x = Date, y = Value, colour = Data_Type)
  )
  + geom_point(alpha = 0.3, size = 2)
  + facet_wrap(
    ~ Symbol
    , ncol = 1
    , scales = "free_y"
    , strip.position = "top"
    )
  + labs(title = "Comparing vaccine parameters as input (observed) vs as simulated")
)
ggsave(
  file.path(
    "figs", "check_vaccine_admin.png"),
  p1,
  width = fig.width*1.1,
  height = fig.width
)
