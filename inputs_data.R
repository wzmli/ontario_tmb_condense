library(tidyverse)
library(jsonlite)

## Inputs data: these are data that becomes inputs as fixed parameters in the model
## Example of Inputs data: Testing, vaccination, voc

prov <- "ON"

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
