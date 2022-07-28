# ---------------------------
# Prepare observations
#
# load and tidy data to which the model is being calibrated
# ---------------------------

cat("loading observed data...\n")

# load raw data
observed_data_raw <- read_csv(
  "https://data.ontario.ca/datastore/dump/ed270bb8-340b-41f9-a7c6-e8ef587e6d11?bom=True",
  show_col_types = FALSE)

## tidy all observed data into long form for calibration
observed_data <- (observed_data_raw
                  %>% transmute(date = as.Date(`Reported Date`)
                                , report_inc = diff(c(0,`Total Cases`))
                                , hosp_preval = `Number of patients hospitalized with COVID-19`
                                , icu_preval = `Number of patients in ICU due to COVID-19`
                  )
                  %>% pivot_longer(names_to = "var", -"date")
)

## set calibration end date to date of last observation by default,
calib_end_date <- max(observed_data$date)
