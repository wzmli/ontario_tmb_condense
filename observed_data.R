## Reading in Ontario public data
ontario_dat <- read_csv("https://data.ontario.ca/datastore/dump/ed270bb8-340b-41f9-a7c6-e8ef587e6d11?bom=True")

print(ontario_dat)

## Cleaning and picking the time series of interest for calibration
observed_data <- (ontario_dat
	%>% transmute(date = as.Date(`Reported Date`)
		, report_inc = diff(c(0,`Total Cases`))
		, hosp_preval = `Number of patients hospitalized with COVID-19`
		, icu_preval = `Number of patients in ICU due to COVID-19`
	)
	%>% pivot_longer(names_to = "var", -"date")
)

## filter out ICU (don't want to fit to it)
# observed_data <- (observed_data
#   %>% filter(var != "icu_preval")
# )

print(observed_data)

print(ggplot(observed_data)
  + facet_wrap(~var, nrow=3, scales = "free_y")
  + geom_line(aes(date, value))
  + labs(title = "Observed data for calibration")
)
