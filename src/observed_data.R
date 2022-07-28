# ---------------------------
# Prepare observations
#
# load and tidy data to which the model is being calibrated
# ---------------------------

cat("loading observed data...\n")

## Download latest observations if the cache file doesn't exist
if(!file.exists(file.path("obj", "observed_data_all.RDS"))) download_observed_data()

## Load latest observations
get_obj("observed_data_all", date = NULL)

## Update observations if last download was before today
if(attr(observed_data_all, "save_date") < today()) download_observed_data()

## Pull out observed data for the given province
observed_data <- (observed_data_all
  %>% filter(province == region)
)

## Select calibration variables
## (if user sets calib_vars to NULL, use all available observation variables in calibration, otherwise use user-provided list)
if(is.null(calib_vars)) calib_vars <- unique(observed_data$var)

# Prep obs for calibration
# ----------------------------

## select province
observed_data <- (observed_data
  %>% filter(province == region)
)

## make calibration data
calibration_dat <- (observed_data
   %>% filter(
     ## select calibration period
     between(date, as.Date(calib_start_date), as.Date(calib_end_date))
     )
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

df <- bind_rows(
  (observed_data
    %>% mutate(type = "as reported")),
  (calibration_dat
    %>% select(-scale_factor)
    %>% mutate(type = "used in calibration"))
)

## plot observed data
p1 <- (ggplot(df)
       + facet_wrap(~var, nrow=3, scales = "free_y")
       + geom_point(aes(x = date, y = value,
                       colour = type, alpha = type))
       + labs(title = "Observed data")
       + scale_colour_manual(
         values = c("grey75", "palevioletred1")
       )
       + scale_alpha_manual(
         values = c(0.75, 0.25)
       )
       + guides(alpha = "none")
       + theme(legend.title = element_blank())
)

suppressWarnings(ggsave(
  file.path("figs", "observed_data.png")
  , p1
  , width = fig.width
  , height = 1.3*fig.width
))

cat("observed data loaded\n\n")
