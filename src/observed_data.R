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

## save for later
save_obj("observed_data", calib_end_date)

## if user sets calib_vars to NULL, use all available observation variables in calibration
if(is.null(calib_vars)) calib_vars <- unique(observed_data$var)

# Prep obs for calibration
# ----------------------------

## make calibration data
calibration_dat <- (observed_data
   ## filter to calibration period
   %>% filter(between(date, as.Date(calib_start_date), as.Date(calib_end_date)))
   ## keep only desired observations
   %>% filter(var %in% calib_vars)
   ## remove reports after date when testing became unreliable
   %>% filter(!(var == "report_inc" & date > report_end_date))
)

# Apply scaling factor adjustment
# (if requested)
# ----------------------------
if(!is.null(obs_scaling)){
  ## convert to long form
  ## (one value per day in range specified
  ## by start_date and end_date)
  obs_scaling_long <- pmap_dfr((obs_scaling %>% replace_na(list(end_date = calib_end_date))),
                               function(...){
    data.frame(
      date = seq.Date(..1, ..2, by = 1),
      var = ..3,
      scale_factor = ..4
    )
  })

  calibration_dat <- (calibration_dat
    %>% left_join(obs_scaling_long,
                  by = c("date", "var"))
    %>% replace_na(list(scale_factor = 1))
    %>% mutate(value = value * scale_factor)
    )
}

## save to invert scaling later, if it was done
if("scale_factor" %in% names(calibration_dat)){
 save_obj("calibration_dat", calib_end_date)
}

# ---------------------------
# Condense Map
#
# map internal model variables (in the simulation history of the model defined
# in define_model.R) to names of tidied observed variables (defined above)
#
# vector names = internal model variables
# vector values = tidied obs variable names
# ---------------------------

## default observations that get fed in
condense_map = c(
  conv_Incidence = 'report_inc',
  Htotal = 'hosp_preval',
  ICU = 'icu_preval'
)

## filter only the obs we want to calibrate to
condense_map <- condense_map[condense_map %in% calib_vars]

## additional calculated values
condense_map = c(
  condense_map,
  lag_1_diff_Xtotal = 'hosp_inc',
  Rtotal = 'recov_preval'
)

# ---------------------------
# Diagnostics
# ---------------------------

print(observed_data)

df <- bind_rows(
  observed_data %>% mutate(type = "as reported"),
  calibration_dat %>% select(-scale_factor) %>% mutate(type = "used in calibration")
)

## plot observed data
p1 <- (ggplot(df)
       + facet_wrap(~var, nrow=3, scales = "free_y")
       + geom_point(aes(x = date, y = value,
                       colour = type, alpha = type))
       + labs(title = "Observed data")
       # + scale_size_manual(
         # values = c(2, 1)
       # )
       + scale_colour_manual(
         values = c("grey75", "palevioletred1")
       )
       + scale_alpha_manual(
         values = c(0.75, 0.25)
       )
       + guides(alpha = "none")
       + theme(legend.title = element_blank())
)

ggsave(
  file.path("figs", "observed_data.png")
  , p1
  , width = fig.width
  , height = 1.3*fig.width
)

# ---------------------------
# Script output
# ---------------------------

# env <- clean_env(
#   env,
#   c("observed_data",
#     "calibration_dat",
#     "condense_map"))


