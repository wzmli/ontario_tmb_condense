# ---------------------------
# Prepare schedules for time-varying parameters that are being optimized
# ---------------------------

## Users can define manually (good to have a reason)
## Separate out what is automatic and what is manual and then combine in the end

# ---------------------------
# beta0 - transmission rate
# ---------------------------

## auto-detect break dates in infection reports time-series

## filter on observed report time series
## smooth it with log 7 day moving average
report_dat <- (observed_data
	%>% filter(var == "report_inc")
	%>% mutate(lmavg = log(frollmean(value,n=7,align = "right")))
	%>% filter(!is.infinite(lmavg))
	%>% filter(between(
	  date, calib_start_date, calib_end_date
	))
	%>% filter(date < report_end_date)
)

## check
gg <- ggplot(report_dat,aes(x=date,y=lmavg)) + geom_line()
gg

## fit a loglinear regression
fit <- lm(lmavg ~ date,data=report_dat)

## refit with piecewise break dates. We can pick as many break points as we want
## here, we are arbitrary picking 14 (we had roughly 6 waves)

refit <- segmented(fit, seg.Z = ~date, npsi=n_breaks_beta0)

## extract breakdates
break_date <- as.Date(c(round(refit$indexU$date)))

print(gg + geom_vline(xintercept=break_date))

reporting_lag <- 8
auto_beta <- data.frame(Date = break_date - reporting_lag
	, Symbol = "beta0"
	, Value = NA
)

## manual breaks (after reports drop out)
if(length(manual_beta0_breaks) != 0){
  manual_beta <-data.frame(
    Date = manual_beta0_breaks
    , Symbol = "beta0"
    , Value = NA
  )
} else {
  manual_beta <- NULL
}

# ---------------------------
# Mildness

## periodically re-fit severity (mu) and hospital occupancy (rho)
## to get better fits, especially after the reports signal drops out
date_seq_mu <- c(
  ## monthly until reports become unreliable
  seq(min(observed_data$date), report_end_date,
      by='months')
  ## every 10 days after that point
  # , seq(report_end_date, ymd(max(observed_data$date)),
  #     by = 10)
  )

manual_mu <- data.frame(
  Date = date_seq_mu
  , Symbol = "mu"
  , Value = NA
)

# ---------------------------
# Length of stay, acute care
# ---------------------------

date_seq_rho <- c(
  ## monthly until reports become unreliable
  seq(min(observed_data$date), report_end_date,
      by='months')
  ## every 10 days after that point
  , seq(report_end_date, max(observed_data$date),
      by = 10)
)

manual_rho <- data.frame(
  Date = date_seq_rho
  , Symbol = "rho"
  , Value = NA
)

# ---------------------------
# Prepare time-varing parameters
#
# params that are fixed and not fitted
# e.g., daily vaccine dosing, testing, variant frequencies
# ---------------------------

# ---------------------------
# Vaccine Dosing
# ---------------------------

## data inputs
prov <- "ON"
url <- paste0('https://api.covid19tracker.ca/reports/province/'
              , prov
              , '?fill_dates=true'
)

## pull data
vaccine_database <- fromJSON(url)
vaccine_raw <- (
  vaccine_database$data
  ## select only relevant columns and rename to tidier names
  %>% transmute(
    province = prov
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
## We want everything to ending up long format
vaccine_tidy <- (vaccine_raw
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


p1 <- (ggplot(vaccine_tidy,aes(x = date, y = value))
       + geom_point(alpha = 0.3)
       + facet_wrap(~ name,ncol = 1)
       + labs(title = "Daily vaccine doses")
)

print(p1)

p2 <- (ggplot(vaccine_tidy,aes(x = date, y = cumval/12e6))
       + geom_point(alpha = 0.3)
       + facet_wrap(~ name,ncol = 1)
       + labs(title = "Cumulative vaccine doses")
)

print(p2)

## final output (processed ts or params_timevar df)
## for the four-dose model
params_timevar_vaxdosing <- (
  vaccine_tidy
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

# plot to check dosing
p3 <- (ggplot(
  params_timevar_vaxdosing,
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
+ labs(title = "Time-varying parameters input for vaccination",
       x = "date")
)
print(p3)

# ---------------------------
# Variant proportion
#
# load and tidy data on variant counts and frequencies
# ---------------------------

variants_raw <- readRDS("metadata/covvarnet_voc.rds")

## This repo is for Ontario Only
## We will make a new repo later for other pts
major_prov = c("Ontario")

variants_tidy <- data.frame()

for(i in names(variants_raw)){
  date <- rownames(variants_raw[[i]])
  tempdf <- (variants_raw[[i]]
             %>% mutate(province = i
                        , date = date
             )
             %>% select(date,province,everything())
  )
  rownames(tempdf) <- NULL

  variants_tidy <- bind_rows(variants_tidy,tempdf)
}

print(names(variants_tidy))

variants_long <- (variants_tidy
   %>% filter(province %in% major_prov)
   %>% pivot_longer(names_to = "strain",
                    values_to= "count",
                    -c("date","province"))
   ## tack on lookup table for variant names
   %>% left_join(variant_map, by = "strain")
   %>% mutate(
     label = ifelse(is.na(label), "other", label)
   , date = as.Date(date)
  )
)

variants_ts_all <- (variants_long
   %>% group_by(date, province, label)
   %>% summarise(simple_count = sum(count,na.rm=TRUE), .groups = "drop")
   %>% group_by(date,province)
   %>% mutate(simple_sum = sum(simple_count,na.rm=TRUE))
   %>% filter(simple_sum != 0)
   %>% mutate(inv_prop = simple_count/simple_sum)
   %>% arrange(province, date, label)
   %>% ungroup()
)

## filter each variant down to desired start and end dates
variants_ts <- (
  variants_ts_all
  %>% left_join(
    invader_properties %>% select(label, start_date, end_date),
    by = "label")
  %>% filter(!is.na(start_date))
  %>% mutate(is_invading =
               between(date,
                       start_date,
                       end_date))
  %>% filter(is_invading)
)

gg <- (
  ggplot(variants_ts_all,
  aes(x = date, y = inv_prop, color = label,
      group = label))
       + geom_line()
       + facet_wrap(~province)
       + scale_x_date(date_labels = "%Y")
       + theme(legend.position = "bottom")
)

print(gg)

print(gg %+% variants_ts)

## make params_timevar lines for invader proportion
## NOTE: there is no interpolation here! inv_prop will be piecewise constant
params_timevar_inv_prop <- (
  variants_ts
  %>% rename(Date = date,
             Value = inv_prop)
  %>% mutate(Symbol = "inv_prop")
  %>% select(Date, Symbol, Value)
  %>% as.data.frame()
)

# ---------------------------
# Variant parameter changes
#
# For each invasion, there is a resident and invader
# Several parameters have to change roles from invader to resident upon a new invasion
# That gets taken care of here
# ---------------------------

#' Prepare time-varying invasion parameters
#'
#' Subset the invader_properties df defined in pipeline_parameters.R to pull out a specific set of parameters, then attach resident parameters for each invasion and put in standard params_timevar format
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
  ## fill in wild-type parameter values for first invasion (get from base parameters list)
  df[1, names(df)[grep("^not_inv",
                       names(df))]] <- c(
                         unname(params[grep(paste0("^", params_prefix), names(params))]))

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
  )

  return(df)
}

## prep all parameters that need to change upon an invasion

inv_params_list <- c("vax_VE_trans", "vax_VE_hosp",
                     "trans_adv")

params_timevar_inv_params <- bind_rows(
  lapply(inv_params_list,
         prep_invasion_params)
)

# ---------------------------
# Script output
# ---------------------------

params_timevar_beta <- bind_rows(auto_beta, manual_beta)
params_timevar_mu <- manual_mu
params_timevar_rho <- manual_rho

params_timevar_vaxdosing <- params_timevar_vaxdosing

params_timevar_inv_prop <- params_timevar_inv_prop
params_timevar_inv_params <- params_timevar_inv_params

params_timevar <- (bind_rows(
  params_timevar_beta
  , params_timevar_mu
  , params_timevar_rho
  , params_timevar_vaxdosing
  , params_timevar_inv_prop
  , params_timevar_inv_params
)
  %>% filter(
    between(Date,
            calib_start_date, calib_end_date)))

parameters <- addEnvironment(parameters
	, c("params_timevar")
)
