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
  
  ## check if sheet exists, if not, do nothing
  sheet_list <- sheet_names("https://docs.google.com/spreadsheets/d/13GBes6A2PMXITwfkyYw7E3Lt3odpb3tbiFznpVy8VhU/edit?usp=sharing")
  if(!(sheet %in% sheet_list)) return()
  
  ## load
  pp <- (
    suppressMessages(read_sheet(
      params_url,
      sheet = sheet,
      col_types = 'c' ## in order to parse fractional values later
    ))
    %>% select(symbol, value)
  )

  ## assign each variable to the global environment
  invisible(map2(pp$symbol,
                 pp$value,
                 function(sym,val){
                   assign(sym,
                          ## parse and evaluate from character string
                          eval(parse(text = val)),
                          envir = .GlobalEnv)}))
}

get_popsize <- function(region){
  (suppressMessages(get_cansim("98-10-0001-01"))
    %>% left_join(read_csv("data/region_lookup.csv", show_col_types = FALSE),
                           by = c("GEO" = "province"))
    %>% filter(prov == region)
    %>% pull(`Population and dwelling counts (11): Population, 2021 [1]`)
    %>% as.numeric()
  )
}

#' Download all observed data locally
#'
#' from Michael Li's COVID19-Canada repository.
#'
#' Caches the data daily to speed up the pipeline.
download_observed_data <- function(){
  cat("-- downloading observed data for all regions...\n")

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
    observed_data_pt,
    observed_data_CA
  ))

  ## assign to global environment so that save_obj works
  assign("observed_data_all", observed_data_all, envir = .GlobalEnv)

  ## save for later
  save_obj("observed_data_all", NULL)
}

#' Get vaccine dosing timeseries and prepare for attachment to model
#'
#' @param region two-letter abbreviation of the region (province or territory; use "CA" for Canada)
#'
#' @return
#' @export
#'
#' @examples
get_params_timevar_vaxdosing <- function(
    region,
    diagnostics = TRUE
){
  ## get vaxdosing data, tidy, plot diagnostics
  ## data inputs
  url_seg <- ifelse(region == "CA",
                    "",
                    paste0("/province/", region))
  url <- paste0('https://api.covid19tracker.ca/reports'
                , url_seg
                , '?fill_dates=true'
  )

  ## pull data
  db <- fromJSON(url)

  ## select and rename
  df <- (
    db$data
    ## select only relevant columns and rename to tidier names
    %>% transmute(
      province = region
      , date
      , doseall_total = total_vaccinations ## total number of doses administered
      ## preval = "prevalence", vs inc = "incidence"
      ## preval = cumulative vax, (daily) incidence = new vax
      , dose2_preval = total_vaccinated ## total number of people with two doses (second doses given)
      , dose3_preval = total_boosters_1 ## third doses given
      , dose4_preval = total_boosters_2 ## fourth doses given
    )
  )

  ## tidy data
  df <- (df
     ## convert date col to dates
     %>% mutate(
       date = as.Date(date)
     )
     ## get dose1 prevalence
     %>% mutate(
       ## count up all non-dose1 prevalence
       not_dose1_preval = rowSums(across(ends_with("preval"))),
       ## get dose1 prev by subtracting non-dose1 from total vaccines administered
       dose1_preval = doseall_total - not_dose1_preval
     )
     ## drop unneeded cols
     %>% select(-ends_with("total"), -starts_with("not"))
     ## reorder
     %>% relocate(dose1_preval, .after = "date")
     ## calculate incidence and relabel cols
     %>% transmute(
       date
       , across(ends_with("preval"), ~ .x - lag(.x))
     )
     %>% rename_with(~ str_replace(.x, "preval", "inc"),
                     ends_with("preval"))
     ## replace negative incidence or NA
     ## with 0 for all numeric columns
     %>% mutate(across(where(is.numeric),
                       ~ ifelse((.x < 0 | is.na(.x)), 0, .x)))
     %>% mutate(total = rowSums(across(ends_with("inc"))))
     %>% filter(total != 0)
     %>% select(-total)
     %>% pivot_longer(-date)
     %>% group_by(name)
     %>% mutate(cumval = cumsum(value))
     %>% ungroup()
  )

  if(diagnostics){
    p1 <- (ggplot(df,
                  aes(x = date, y = value))
           + geom_point(alpha = 0.3)
           + facet_wrap(~ name,ncol = 1)
           + labs(title = paste0("Daily vaccine doses for ", region))
    )

    print(p1)

    p2 <- (ggplot(df,
                  aes(x = date, y = cumval))
           + geom_point(alpha = 0.3)
           + facet_wrap(~ name,ncol = 1)
           + labs(title = paste0("Cumulative vaccine doses for ", region))
    )

    print(p2)
  }

  ## format as params_timevar lines
  df <- (df
     %>% select(-cumval)
     ## keep only doses 1-4
     %>% filter(str_detect(name, "^dose(1|2|3|4)"))
     ## rename params to match names in model definition
     %>% mutate(name = paste0("vax_", name))
     %>% pivot_wider(
       id_cols = date)
     # ## sum total daily doses
     %>% mutate(doseall_inc = rowSums(across(where(is.numeric))))
     ## drop days where no vaccines were administered at all
     %>% filter(doseall_inc != 0)
     ## format as params_timevar
     %>% select(-doseall_inc)
     %>% pivot_longer(
       -date,
       names_to = "Symbol",
       values_to = "Value"
     )
     %>% rename(Date = date)
     %>% as.data.frame()
   )

  ## diagnostic plot to check dosing
  if(diagnostics){
    print(
      plot_ptv(df)
      + labs(title = paste0("Time-varying parameters input for vaccination for ", region))
    )
  }

  return(df)
}

#' Plot a params_timevar format `data.frame`
#'
#' @param df output from any [ptv_*] function in this repo
plot_ptv <- function(df){
  pp <- (ggplot(
    df,
    aes(x = Date, y = Value, colour = Symbol)
  )
  + geom_point(alpha = 0.3)
  + facet_wrap(
    ~ Symbol,
    ncol = 1,
    scales = "free_y",
    strip.position = "top"
  )
  + guides(colour = "none")
  + theme(axis.title = element_blank())
  )
  print(pp)
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
#' @param region two-letter abbreviation of the region (province or territory; use "CA" for Canada)
run_calibration <- function(region){
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
run_forecast <- function(region){
  ## Specify forecast settings
  source(file.path("src","forecast_settings.R")) ## EDIT OFTEN

  ## Forecast and plot
  source(file.path("src","forecast.R")) ## EDIT NEVER
  source(file.path("src","forecast_plots.R"))
}

#' Plot model-inferred seroprevalence against observed data
#'
#' @param region two-letter abbreviation of the region (province or territory; use "CA" for Canada)
plot_seroprev <- function(region){
  ## check whether seroprev data exists
  obs_raw <- read_csv("https://github.com/wzmli/COVID19-Canada/raw/master/seroprevalence.csv",
                      show_col_types = FALSE)
  if(!(region %in% unique(obs_raw$Province))){
    warning(paste0("seroprevalence data not available for ", region))
    return()
  }
  
  ## pull seroprev data
  obs <- (obs_raw
          %>% rename(date = Date)
          %>% filter(Province == region)
          %>% select(-Province)
          %>% mutate(across(-date, as.numeric))
          %>% pivot_longer(-date)
          %>% separate(name,
                       into = c("trash", "assay", "value_type"))
          %>% select(-trash)
  )
  
  ## get model output
  pop_size <- unname(get_obj("model_calibrated",
                             calib_date)$params["N"])
  
  ens <- (get_obj("forecast_ensemble", calib_date)
          %>% ungroup()
          %>% filter(var == 'recov_preval')
          %>% rename(date = Date)
          %>% select(-var, -scale_factor)
          ## rescale to seroprev scale (% of the pop)
          %>% mutate(
            population = pop_size,
            value = value/population*100,
            lwr = lwr/population*100,
            upr = upr/population*100)
          %>% select(-population)
  )
  
  p <- (ggplot(ens, aes(x = date))
        + geom_ribbon(
          aes(ymin = lwr, ymax = upr),
          alpha = 0.2, fill = 'grey30')
        + geom_line(aes(y = value), colour = 'red')
        + geom_errorbar(
          data = obs %>% filter(value_type != 'est') %>% pivot_wider(id_cols = c('date', 'assay'),
                                                                     names_from = 'value_type'),
          aes(x = date, ymin = lwr, ymax = upr, colour = assay))
        + geom_point(
          data = obs %>% filter(value_type == 'est'),
          aes(y = value, colour = assay)
        )
        + scale_x_date(expand = expansion(mult = 0),
                       limits = c(date_range[1],
                                  date_range[2] + n_days_forecast))
        + scale_y_continuous(expand = expansion(mult = 0))
        + scale_colour_manual(values = c('dodgerblue', 'forestgreen'))
        + labs(title = 'Seroprevalence (%) over time as implied by model')
        + theme(axis.title = element_blank(),
                legend.position = c(0,1),
                legend.justification = c(0.01,1),
                legend.background = element_rect(fill = NA))
  )
  
  ggsave(
    file.path('figs',
              'seroprev.png'),
    p,
    width = 6,
    height = 0.75*6
  ) 
}
