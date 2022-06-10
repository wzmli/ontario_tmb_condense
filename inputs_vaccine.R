## data inputs
prov <- "ON"
url <- paste0('https://api.covid19tracker.ca/reports/province/'
              , prov
              , '?fill_dates=true'
)

## pull data
vaccine_database <- fromJSON(url)
vaccine_raw <- (
  vaccine_database$data
  ## select only relevant columns and rename to tidier names
  %>% transmute(
    province = prov
    , date
    , doseall_total = total_vaccinations ## total number of doses administered
    ## preval = "prevalence", vs inc = "incidence"
    ## preval = cumulative vax, (daily) incidence = new vax
    , dose2_preval = total_vaccinated ## total number of people with two doses (second doses given)
    , dose3_preval = total_boosters_1 ## third doses given
    , dose4_preval = total_boosters_2 ## fourth doses given
    )
)

## tidy data
## We want everything to ending up long format

vaccine_tidy <- (vaccine_raw
  ## convert date col to dates
  %>% mutate(
    date = as.Date(date)
  )
  ## get dose1 prevalence
  %>% mutate(
    ## count up all non-dose1 prevalence
    not_dose1_preval = rowSums(across(ends_with("preval"))),
    ## get dose1 prev by subtracting non-dose1 from total vaccines administered
    dose1_preval = doseall_total - not_dose1_preval
  )
  ## drop unneeded cols
  %>% select(-ends_with("total"), -starts_with("not"))
  ## reorder
  %>% relocate(dose1_preval, .after = "date")
  ## calculate incidence
  %>% transmute(
    date
    , across(ends_with("preval"), ~ .x - lag(.x))
  )
  %>% rename_with(~ str_replace(.x, "preval", "inc"),
                  ends_with("preval"))
  ## replace negative incidence or NA
  ## with 0 for all numeric columns
  %>% mutate(across(where(is.numeric),
                    ~ ifelse((.x < 0 | is.na(.x)), 0, .x)))
  %>% mutate(total = rowSums(across(ends_with("inc"))))
  %>% filter(total != 0)
  %>% select(-total)
  %>% pivot_longer(-date)
  %>% group_by(name)
  %>% mutate(cumval = cumsum(value))
  %>% ungroup()
)


p1 <- (ggplot(vaccine_tidy,aes(x = date, y = value))
  + geom_point(alpha = 0.3)
  + facet_wrap(~ name,ncol = 1)
  + labs(title = "Daily vaccine doses")
)

print(p1)

p2 <- (ggplot(vaccine_tidy,aes(x = date, y = cumval/12e6))
  + geom_point(alpha = 0.3)
  + facet_wrap(~ name,ncol = 1)
  + labs(title = "Cumulative vaccine doses")
)

print(p2)
#ggsave(
#  file.path("figs","inputs_vaccine.png"),
#  p1,
#  width = fig.width,
#  height = 1.3*fig.width
#)

## fit/forecast (if need be)

## final output (processed ts or params_timevar df)

## for the two-dose model
params_timevar_vaccine <- (
  vaccine_tidy
  %>% select(-cumval)
  %>% pivot_wider(
    id_cols = date)
  ## keep only first and second doses
  %>% select(
    date
    , matches("^dose(1|2)")
  )
  ## sum total daily doses
  %>% mutate(doseall_inc = rowSums(across(where(is.numeric))))
  ## drop days where no vaccines were administered
  %>% filter(doseall_inc != 0)
  ## calculate parameters for two-dose model
  %>% transmute(
    date
    , vax_doses_per_day = doseall_inc
    , vax_prop_first_dose = dose1_inc/vax_doses_per_day
  )
  ## format as params_timevar
  %>% pivot_longer(
    -date,
    names_to = "Symbol",
    values_to = "Value"
  )
  %>% rename(Date = date)
  %>% as.data.frame()
)
head(params_timevar_vaccine)
tail(params_timevar_vaccine)

# plot to check
p3 <- (ggplot(
  params_timevar_vaccine,
  aes(x = Date, y = Value, colour = Symbol)
  )
  + geom_point(alpha = 0.3)
  + facet_wrap(
    ~ Symbol,
    ncol = 1,
    scales = "free_y",
    strip.position = "top"
  )
  + guides(colour = "none")
  + labs(title = "Time-varying parameters input for vaccination",
         x = "date")
)
print(p3)
