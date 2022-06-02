## THIS IS A MODEL-SPECIFIC SCRIPT

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
  ## calculate model parameter quantities
  %>% transmute(
    Date
    , vax_doses_per_day = rowSums(across(ends_with("inc")))
    , vax_prop_first_dose = V_vaxdose1_inc/vax_doses_per_day
  )
  %>% pivot_longer(
    -Date
    , names_to = "Symbol"
    , values_to = "Value")
  %>% mutate(Data_Type = "simulated")
)

vaccine_check <- (bind_rows(
  vaccine_obs,
  vaccine_sim
)
  ## filter out days without vaccination
  %>% filter(!(Symbol == "vax_doses_per_day" & Value == 0))
  %>% drop_na()
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
  + theme(
    legend.position = "bottom"
  )
  + labs(title = "Comparing vaccine parameters as input (observed) vs as simulated")
)
ggsave(
  file.path(
    "figs", "check_vaccine_admin.png"),
  p1,
  width = fig.width,
  height = fig.width
)
