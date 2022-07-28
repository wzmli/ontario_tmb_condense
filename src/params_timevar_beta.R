# ---------------------------
# beta0 - transmission rate
# ---------------------------

## auto-detect break dates in infection reports time-series

## filter on observed report time series
## smooth it with log 7 day moving average
report_dat <- (observed_data
               %>% filter(var == "report_inc",
                          date < report_end_date)
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

refit <- segmented(fit, seg.Z = ~date, npsi=n_breaks_auto_beta)

## extract breakdates
break_date <- as.Date(c(round(refit$indexU$date)))

print(gg + geom_vline(xintercept=break_date))

reporting_lag <- 8
auto_beta <- data.frame(Date = break_date - reporting_lag
                        , Symbol = "beta0"
                        , Value = NA
)

## manual breaks (after reports drop out)
if(length(manual_beta_breaks) != 0){
  manual_beta <-data.frame(
    Date = manual_beta_breaks
    , Symbol = "beta0"
    , Value = NA
  )
} else {
  manual_beta <- NULL
}

# ---------------------------
# Script output
# ---------------------------

params_timevar_opt <- bind_rows(
  params_timevar_opt,
  auto_beta,
  manual_beta)
