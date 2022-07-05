# ---------------------------
# Prepare observations
#
# load and tidy data to which the model is being calibrated
# ---------------------------

# Load and tidy minimally
# ----------------------------

# load raw data
observed_data_raw <- read_csv("https://data.ontario.ca/datastore/dump/ed270bb8-340b-41f9-a7c6-e8ef587e6d11?bom=True")

## tidy all observed data into long form for calibration
observed_data <- (observed_data_raw
	%>% transmute(date = as.Date(`Reported Date`)
		, report_inc = diff(c(0,`Total Cases`))
		, hosp_preval = `Number of patients hospitalized with COVID-19`
		, icu_preval = `Number of patients in ICU due to COVID-19`
	)
	%>% pivot_longer(names_to = "var", -"date")
)

## if user sets calib_vars to NULL, use all available observation variables in calibration
if(is.null(calib_vars)) calib_vars <- unique(observed_data$var)

# Filter obs for calibration
# ----------------------------

## make calibration data
calibration_dat = (observed_data
  ## filter to calibration period
  %>% filter(between(date, as.Date(calib_start_date), as.Date(calib_end_date)))
  ## keep only desired observations
  %>% filter(var %in% calib_vars)
  ## remove reports after date when testing became unreliable
  %>% filter(!(var == "report_inc" & date > report_end_date))
)

# ---------------------------
# Condense Map
#
# map internal model variables (in the simulation history of the model defined
# in define_model.R) to names of tidied observed variables (defined above)
#
# vector names = internal model variables
# vector values = tidied obs variable names
# ---------------------------

condense_map = c(
  conv_Incidence = 'report_inc',
  Htotal = 'hosp_preval',
  ICU = 'icu_preval'
)

# ---------------------------
# Diagnostics
# ---------------------------

print(observed_data)

## plot observed data
p1 <- (ggplot(observed_data)
       + facet_wrap(~var, nrow=3, scales = "free_y")
       + geom_line(aes(date, value))
       + labs(title = "Observed data available for calibration")
)
ggsave(
  file.path("figs", "observed_data-available.png")
  , p1
  , width = fig.width
  , height = 1.3*fig.width
)

p2 <- (ggplot(calibration_dat %>% drop_na()
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
  file.path("figs", "observed_data-used.png")
  , p2
  , width = fig.width
  , height = 1.3*fig.width
)

# ---------------------------
# Script output
# ---------------------------

env <- clean_env(
  env,
  c("observed_data",
    "calibration_dat",
    "condense_map"))


