
# ---------------------------
# Prepare schedules for time-varying parameters that are being optimized
# ---------------------------

## Users can define manually
## Here is an automatic way

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
refit <- segmented(fit, seg.Z = ~date, npsi=n.breaks)

## extract breakdates
break_date <- as.Date(c(round(refit$indexU$date)))

print(gg + geom_vline(xintercept=break_date))

reporting_lag <- 8
params_timevar_beta <- data.frame(Date = break_date - reporting_lag
	, Symbol = "beta0"
	, Value = NA
)


## MLi: Need to think about where this belongs

## add in break dates after reports drop out (changes in behaviour in jan)


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

params_timevar_beta <- bind_rows(params_timevar_beta, manual_beta)


# ---------------------------
# Mildness
# ---------------------------


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

params_timevar_mu <- data.frame(
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

params_timevar_rho <- data.frame(
  Date = date_seq_rho
  , Symbol = "rho"
  , Value = NA
)
