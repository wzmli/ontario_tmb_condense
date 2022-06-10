library(segmented)
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)


## filter on observed report time series
## smooth it with log 7 day moving average
report_dat <- (observed_data
	%>% filter(var == "report_inc")
	%>% mutate(lmavg = log(frollmean(value,n=7,align = "right")))
	%>% filter(!is.infinite(lmavg))
	%>% filter(date < as.Date("2022-01-01"))
)

## check
gg <- ggplot(report_dat,aes(x=date,y=lmavg)) + geom_line()
gg

## fit a loglinear regression
fit <- lm(lmavg ~ date,data=report_dat)

## refit with piecewise break dates. We can pick as many break points as we want
## here, we are arbitrary picking 14 (we had roughly 6 waves)

n.breaks <- 14
refit <- segmented(fit, seg.Z = ~date, npsi=n.breaks)

## extract breakdates
break_date <- as.Date(c(round(refit$indexU$date)))

gg + geom_vline(xintercept=break_date)

reporting_lag <- 8
params_timevar_beta <- data.frame(Date = break_date - reporting_lag
	, Symbol = "beta0"
	, Value = NA
)
print(params_timevar_beta)

## break dates for severity
## (changes in severity based on variant invasion)
params_timevar_mu <- data.frame(
  Date = as.Date(c(
    "2020-07-01" ## diff "severity" in first wave? people more likely to go to hospital out of panic?
    , "2020-12-25" ## when Alpha started taking over
    , "2021-02-15" ## just before Delta
    , "2021-03-15" ## when Delta started taking over
    , "2021-12-01" ## when Omicron started taking over
  ))
  , Symbol = "mu"
  , Value = NA
)
