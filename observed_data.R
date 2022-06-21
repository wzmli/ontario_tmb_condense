# ---------------------------
# Prepare observations
#
# load and tidy data to which the model is being calibrated
# ---------------------------

# load raw data
observed_data_raw <- read_csv("https://data.ontario.ca/datastore/dump/ed270bb8-340b-41f9-a7c6-e8ef587e6d11?bom=True")

## tidy observed data into long form for calibration
observed_data <- (observed_data_raw
	%>% transmute(date = as.Date(`Reported Date`)
		, report_inc = diff(c(0,`Total Cases`))
		, hosp_preval = `Number of patients hospitalized with COVID-19`
		, icu_preval = `Number of patients in ICU due to COVID-19`
	)
	%>% pivot_longer(names_to = "var", -"date")
)

print(observed_data)

## plot observed data
p1 <- (ggplot(observed_data)
  + facet_wrap(~var, nrow=3, scales = "free_y")
  + geom_line(aes(date, value))
  + labs(title = "Observed data for calibration")
)

print(p1)

## If this is not good enough for calibration, then modify here!

calibration_dat = (observed_data
  ## filter to calibration period
  %>% filter(between(date, as.Date(simulation_start_date), as.Date(calibration_end_date)))
  # ,
  ## remove reports after date when testing became unreliable 
  ## MLi: throw this in parameters.R
  %>% filter(!(var == "report_inc" & date > lubridate::ymd(20211215)))
  ## remove ICU prevalence
  %>% filter(!(var == "icu_preval"))
)

p1 <- (ggplot(calibration_dat %>% drop_na()
        , aes(x = date, y = value, colour = var))
  + geom_point(alpha = 0.3, size = 1.5)
  + facet_wrap(
    ~ var
    , ncol = 1
    , scales = "free_y"
)
  + labs(title = "Observed data used in calibration")
  + guides(colour = "none")
)
ggsave(
  file.path("figs", "context_information-observed_data.png")
  , p1
  , width = fig.width
  , height = 1.3*fig.width
)



## Output
## There must be a better way to do this
## Ideally, repackaged an new environment and only save the final output
calibration_dat <- calibration_dat


