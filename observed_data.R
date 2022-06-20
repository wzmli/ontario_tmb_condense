# ---------------------------
# Prepare observations
#
# load and tidy data to which the model is being calibrated
# ---------------------------

# load raw data
observed_data_raw <- read_csv("https://data.ontario.ca/datastore/dump/ed270bb8-340b-41f9-a7c6-e8ef587e6d11?bom=True")

## tidy observed data
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
