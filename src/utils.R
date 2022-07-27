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
