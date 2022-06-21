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
	%>% filter(date < report_end_date)
)

## check
gg <- ggplot(report_dat,aes(x=date,y=lmavg)) + geom_line()
gg

## fit a loglinear regression
fit <- lm(lmavg ~ date,data=report_dat)

## refit with piecewise break dates. We can pick as many break points as we want
## here, we are arbitrary picking 14 (we had roughly 6 waves)

n.breaks <- 11
## MLi: TODO: I want to use 11, using 14 to match IPs manual input
n.breaks <- 14
refit <- segmented(fit, seg.Z = ~date, npsi=n.breaks)

## extract breakdates
break_date <- as.Date(c(round(refit$indexU$date)))

print(gg + geom_vline(xintercept=break_date))

reporting_lag <- 8
auto_beta <- data.frame(Date = break_date - reporting_lag
	, Symbol = "beta0"
	, Value = NA
)


## Manual time varying parameters
## TODO: Going to clean it in the future, will move on for now


manual_beta <-data.frame(
	Date = ymd(c("2021-12-19" ## increase in public health restrictions
		, "2022-01-31" ## begin easing restrictions (change in capacity limits)
   	, "2022-02-17" ## next phase of reopening (change in capacity limits)
   	, "2022-03-01" ## proof of vaccine mandate lifted
   	, "2022-03-21" ## most indoor mask mandates lifted
   	)
	)
	, Symbol = "beta0"
   , Value = NA
)


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
# Vaccine Efficacy (against transmission)
# ---------------------------

## change VE on a rough schedule based on variant invasion
date_seq_VE <- c(delta_invasion_date, omicron_invasion_date)

params_timevar_VE <- data.frame(
  Date = rep(date_seq_VE, each = 4)
  , Symbol = rep(paste0("vax_VE_trans_dose", 1:4), length(date_seq_VE))
  , Value = c(vax_delta_VE_trans_dose1
              , vax_delta_VE_trans_dose2
              , vax_delta_VE_trans_dose3
              , vax_delta_VE_trans_dose4

              , vax_omicron_VE_trans_dose1
              , vax_omicron_VE_trans_dose2
              , vax_omicron_VE_trans_dose3
              , vax_omicron_VE_trans_dose4
  )
)

# ---------------------------
# Variant proportion
#
# load and tidy data on variant counts and frequencies
# ---------------------------

library(tidyverse)

dd <- readRDS("metadata/covvarnet_voc.rds")

## This repo is for Ontario Only
## We will make a new repo later for other pts
major_prov = c("Ontario")

df <- data.frame()

for(i in names(dd)){
  dates <- rownames(dd[[i]])
  tempdf <- (dd[[i]]
             %>% mutate(province = i
                        , dates = dates
             )
             %>% select(dates,province,everything())
  )
  rownames(tempdf) <- NULL

  df <- bind_rows(df,tempdf)
}

print(names(df))

vardat <- data.frame(
  variant = c("Alpha", "B.1.438.1"
              , "Beta", "Gamma"
              , "Delta", "Delta AY.25", "Delta AY.27"
              , "Omicron BA.1", "Omicron BA.1.1"
              , "Omicron BA.2"
  )
  , varname = c("Alpha", "Alpha"
                , "Alpha", "Alpha" ## Hack! Changing beta and gamma to alpha
                , "Delta", "Delta", "Delta"
                , "Omicron1", "Omicron1"
                , "Omicron2"
  )
)

invaderframe <- data.frame(
  varname = c("Alpha", "Delta", "Omicron1", "Omicron2")
  , start_date = as.Date(c("2020-12-07","2021-03-08","2021-11-22","2022-01-10"))
  , end_date = as.Date(c("2021-03-07","2021-11-21","2022-01-09","2022-04-04"))
)

variantLong <- (df
                %>% filter(province %in% major_prov)
                %>% pivot_longer(names_to = "variant", values_to="count",-c("dates","province"))
                %>% left_join(.,vardat)
                %>% mutate(varname = ifelse(is.na(varname),"other",varname)
                           , dates = as.Date(dates)
                )
)
i
print(variantLong,n=Inf)

simple_dat <- (variantLong
               %>% group_by(dates,province,varname)
               %>% summarise(simple_count = sum(count,na.rm=TRUE))
               %>% group_by(dates,province)
               %>% mutate(simple_prop = simple_count/sum(simple_count,na.rm=TRUE))
               %>% arrange(province,dates,varname)
               %>% ungroup()
)

invaderdat <- (simple_dat
               %>% left_join(.,invaderframe)
               %>% filter(!is.na(start_date))
               %>% mutate(invader = between(dates,start_date,end_date))
               %>% filter(invader)
)

print(simple_dat,n=Inf)

gg <- (ggplot(simple_dat, aes(x=dates, y=simple_prop, color=varname, group = varname))
       + geom_line()
       + facet_wrap(~province)
       + scale_x_date(date_labels = "%Y")
       + theme(legend.position = "bottom")
)

print(gg)

print(gg %+% invaderdat)

print(invaderdat)

## TODO: move whatever can be moved to pipeline_parameters.R
## and generate
## params_timevar_invaderprop
## params_timevar_invaderVE
##
## after adding variant to define_model.R,
## need to also automate switching resident and invader VE based on invaderdat
##
## add params_timevar_invaderprop and VE to script outputs below

# ---------------------------
# Script output
# ---------------------------

params_timevar_beta <- bind_rows(auto_beta, manual_beta)
params_timevar_mu <- manual_mu
params_timevar_rho <- manual_rho

params_timevar_vaxdosing <- params_timevar_vaxdosing
params_timevar_VE <- params_timevar_VE

params_timevar <- bind_rows(
  params_timevar_beta
  , params_timevar_mu
  , params_timevar_rho
  , params_timevar_vaxdosing
  , params_timevar_VE
  # , params_timevar_variantprop
)

parameters <- addEnvironment(parameters
	, c("params_timevar")
)
