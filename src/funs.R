# ---------------------------
# Working in the repo
# ---------------------------

#' Save object
#'
#' @param obj_name name of the object used in these scripts (as a character string)
#' @param date date (character string) attached to the .RDS filename where the object is stored. if NULL, don't save with date filename and instead attach an attribute called `save_date` with the date the object was saved
save_obj <- function(obj_name, date){
  x <- get(obj_name, envir = .GlobalEnv)
  filename_suffix <- ifelse(is.null(date),
                            "",
                            paste0("_", date))
  if(is.null(date)) attr(x, "save_date") <- today()
  saveRDS(x,
          file.path(
            "obj",
            paste0(obj_name, filename_suffix, ".RDS")
          ))
}

#' Retrieve saved object
#'
#' @inheritParams save_obj
get_obj <- function(obj_name, date){
  filename_suffix <- ifelse(is.null(date),
                            "",
                            paste0("_", date))
  x <- readRDS(file.path(
    "obj",
    paste0(obj_name, filename_suffix, ".RDS")
  ))
  assign(obj_name, x, envir = .GlobalEnv)
}

#' Clean up environment
#'
#' @param env existing environment list
#' @param vars_to_add new variables to add to environment list
clean_env <- function(env, vars_to_add){
  rm(list=setdiff(ls(envir = .GlobalEnv), c(env, vars_to_add)),
     envir = .GlobalEnv)
  return(ls(envir = .GlobalEnv))
}

#' Get most recent calibration date
#' based on filenames in obj directory
get_calib_date <- function(){

  calibs <- list.files("obj", pattern = "^model_calibrated")
  if(length(calibs)==0) stop("no calibrations found from which to forecast. please source run_calibration.R first.")

  calib_date <- max(as.Date(str_replace(
    str_replace(calibs, "model_calibrated_", ""),
    ".RDS", "")))

  assign("calib_date", calib_date, envir = .GlobalEnv)
}

# ---------------------------
# Data getting and prepping
# ---------------------------

#' Load parameters into global environment
#'
#' @param sheet sheet name
#' @param params_url url to a Google Sheet
load_params <- function(sheet, params_url){
  pp <- (
    suppressMessages(read_sheet(
      params_url,
      sheet = sheet,
      col_types = 'c' ## in order to parse fractional values later
    ))
    %>% select(symbol, value)
  )

  invisible(map2(pp$symbol,
                 pp$value,
                 function(sym,val){
                   assign(sym,
                          ## parse and evaluate from character string
                          eval(parse(text = val)),
                          envir = .GlobalEnv)}))
}

#' Download all observed data locally
#'
#' from Michael Li's COVID19-Canada repository.
#'
#' Caches the data daily to speed up the pipeline.
download_observed_data <- function(){
  print("downloading observed data for all regions...")

  # load raw data
  observed_data_raw <- suppressMessages(read_csv(
    "https://raw.githubusercontent.com/wzmli/COVID19-Canada/master/git_push/clean.Rout.csv"
  ))

  ## tidy all observed data
  observed_data_pt <- (observed_data_raw
    ## a little clean-up
    %>% janitor::clean_names()
    %>% mutate(province = str_replace(province, "^PEI$", "PE"))
    ## transform into desired variable names and counts
    %>% transmute(
      date
      , province
      , report_inc = new_confirmations
      , hosp_preval = hospitalization - icu
      , icu_preval = icu
    )
    ## drop rows where all counts are missing
    %>% filter(!if_all(where(is.numeric), is.na))
    ## pivot to long format
    %>% pivot_longer(names_to = "var", -c("date", "province"))
  )

  ## summarize for canada
  observed_data_CA <- (observed_data_pt
                       %>% group_by(date, var)
                       %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
                       %>% mutate(province = "CA")
                       %>% relocate(province, .after = "date")
  )

  ## bind together and pivot longer
  observed_data_all <- (bind_rows(
    observed_data,
    observed_data_CA
  ))

  ## assign to global environment so that save_obj works
  assign("observed_data_all", observed_data, envir = .GlobalEnv)

  ## save for later
  save_obj("observed_data_all", NULL)
}

#' Prepare time-varying parameters for variant invasions
#'
#' Set up time-varying parameters (matching the supplied `params_prefix`) for each variant invasion by also updating
#' default parameters for the corresponding resident strain
#' (whichever strain immediately preceded the invader)
#'
#' This function calls the invader_properties data frame set up earlier in the pipeline.
#'
#' @param params_prefix prefix for a set of parameters (the non-invader version)
#'
#' @return a `data.frame` of time-varying parameter changes
#'
#' @examples params_prefix("vax_VE_trans")
prep_invasion_params <- function(
    params_prefix
){
  df <- (invader_properties
         %>% select(start_date, contains(params_prefix))
         ## attach corresponding resident VEs for each invasion
         ## (take VE from previous variant)
         %>% mutate(across(contains(params_prefix),
                           lag,
                           .names = "not_{.col}"))
  )

  df <- (
    df
    %>% rename(Date = start_date)
    %>% pivot_longer(-Date,
                     names_to = "Symbol",
                     values_to = "Value")
    ## correct parameter names
    %>% mutate(Symbol = str_replace(Symbol,
                                    "not_inv_",
                                    ""))
    ## drop NA from the lag operation
    ## (wild-type values will be being used
    ## regardless for the first invasion because
    ## they're in the default parameters list)
    %>% drop_na()
  )

  return(df)
}

# ---------------------------
# Running stuff
# ---------------------------

#' Run calibration
#'
#' @param region province/territory for which to run the calibration as a two-letter abreviation
run_calibration <- function(region = "ON"){
  ## Load model params
  source(file.path("src","get_params.R"))

  ## Prep observed data for calibration
  source(file.path("src","calibration_dat.R"))

  ## Define model
  source(file.path("src","model.R"))

  ## Generate time-varying params from data
  source(file.path("src","params_timevar_data.R")) ## EDIT RARELY

  ## Set up optimization parameters, schedules for time-varying ones, and priors
  source(file.path("src","opt_settings.R")) ## EDIT SOMETIMES

  ## Calibrate and plot
  source(file.path("src","calibrate.R")) ## EDIT NEVER
  source(file.path("src","calibration_plots.R"))
}

#' Run forecast
#'
#' @inheritParams run_calibration
run_forecast <- function(region = "ON"){
  ## Specify forecast settings
  source(file.path("src","forecast_settings.R")) ## EDIT OFTEN

  ## Forecast and plot
  source(file.path("src","forecast.R")) ## EDIT NEVER
  source(file.path("src","forecast_plots.R"))
}
