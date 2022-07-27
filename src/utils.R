#' Clean up environment
#'
#' @param env existing environment list
#' @param vars_to_add new variables to add to environment list
clean_env <- function(env, vars_to_add){
  rm(list=setdiff(ls(envir = .GlobalEnv), c(env, vars_to_add)),
     envir = .GlobalEnv)
  return(ls(envir = .GlobalEnv))
}

#' Load parameters into global environment
#'
#' @param sheet sheet name
#' @param params_url url to a Google Sheet
load_params <- function(sheet, params_url){
  pp <- (
    read_sheet(
      params_url,
      sheet = sheet,
      col_types = 'c' ## in order to parse fractional values later
    )
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

#' Save object
#'
#' @param obj_name name of the object used in these scripts (as a character string)
#' @param date date (character string) attached to the .RDS filename where the object is stored
save_obj <- function(obj_name, date){
  x <- get(obj_name, envir = .GlobalEnv)
  saveRDS(x,
    file.path(
      "obj",
      paste0(obj_name, "_", date, ".RDS")
  ))
}

#' Retrieve saved object
#'
#' @inheritParams save_obj
get_obj <- function(obj_name, date){
  x <- readRDS(file.path(
    "obj",
    paste0(obj_name, "_", date, ".RDS")
  ))
  assign(obj_name, x, envir = .GlobalEnv)
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

#' Run calibration
#'
#' @param region province/territory for which to run the calibration as a two-letter abreviation
run_calibration <- function(region = "ON"){
  ## Load model params
  source(file.path("src","get_params.R"))

  ## Load observed data
  source(file.path("src","observed_data.R")) ## EDIT RARELY

  ## Define model
  source(file.path("src","model.R")) ## EDIT RARELY

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
