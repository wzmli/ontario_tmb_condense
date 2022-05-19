library(tidyverse)
library(jsonlite)


## Reading in Ontario public data
ontario_dat <- read_csv("https://data.ontario.ca/datastore/dump/ed270bb8-340b-41f9-a7c6-e8ef587e6d11?bom=True")

print(ontario_dat)

## Cleaning and picking the time series of interest for calibration
calibration_dat <- (ontario_dat
	%>% transmute(date = as.Date(`Reported Date`)
		, report_inc = diff(c(`Total Cases`,0))
		, hosp_preval = `Number of patients hospitalized with COVID-19`
		, icu_preval = `Number of patients in ICU due to COVID-19`
	)
	%>% pivot_longer(names_to = "var", -"date")
)

print(calibration_dat)

## Input data (vaccination)

url <- paste0('https://api.covid19tracker.ca/reports/province/'
	, prov
	, '?fill_dates=true'
)

vaccine_database <- fromJSON(url)
vaccine_dat <- (vaccine_database$data
	%>% transmute(province = prov
			, date
			, total_vaccinations
			, total_vaccinated
			, total_boosters_1
			, total_boosters_2
	)
)

print(vaccine_dat)
