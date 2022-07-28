# ---------------------------
# Mildness
# ---------------------------

cat("-- settings for mu...\n")

## periodically re-fit severity (mu) and hospital occupancy (rho)
## to get better fits, especially after the reports signal drops out
date_seq_mu <- c(
  ## a few changes in hosp-seeking behaviour at the start of the pandemic
  as.Date("2020-04-01"),
  as.Date("2020-06-01"),
  ## update for each variant invasion
  invader_properties$start_date

  # + weeks(2) ## offset from initial variant invasion to compensate for delayed effect on hospitalizations

  # ## monthly until reports become unreliable
  # seq(min(observed_data$date), report_end_date,
  #     by='months')
  # ## and once more on the start date of BA.2
  # , invader_properties %>% filter(label == "Omicron2") %>% pull(start_date)

  # # monthly update
  # seq(min(observed_data$date),
  #     max(observed_data$date),
  #     by='months')

  ## every 10 days after that point
  # , seq(report_end_date, ymd(max(observed_data$date)), by = 10)

  ## monthly until Omicron1 invasion
  # seq(min(observed_data$date),
  #     (invader_properties %>% filter(label == 'Omicron1') %>% pull(start_date)),
  #     by='months'),
  # ## halfway through omicron 1
  # (invader_properties %>% filter(label == 'Omicron1') %>% mutate(halfway_date = start_date + (end_date-start_date)/2) %>% pull(halfway_date)),
  # ## start of omicron2
  # (invader_properties %>% filter(label == 'Omicron2') %>% pull(start_date)),
  # ## halfway through omicron2
  # (invader_properties %>% filter(label == 'Omicron2') %>% mutate(halfway_date = start_date + (end_date-start_date)/2) %>% pull(halfway_date))
)

## insert midpoint dates for finer-grain estimates
midpoints <- (tibble(date = date_seq_mu) %>% mutate(date2 = lag(date), midpoint = date + (date2-date)/2) %>% pull(midpoint))
#
date_seq_mu <- c(date_seq_mu, midpoints)

# ## add some breaks before alpha to to compensate for changes in hospital-seeking behaviour
# date_seq_mu <- c(date_seq_mu,
#    as.Date(c(
#      "2020-05-15",
#      "2020-09-01"
#    )))

date_seq_mu <- sort(date_seq_mu)

manual_mu <- data.frame(
  Date = date_seq_mu
  , Symbol = "mu"
  , Value = NA
)

# ---------------------------
# Script output
# ---------------------------

params_timevar_opt <- bind_rows(
  params_timevar_opt,
  manual_mu
)

cat("---- loaded\n")
