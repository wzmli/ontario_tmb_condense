# ---------------------------
# Length of stay, acute care
# ---------------------------

date_seq_rho <- c(
  ## start of each variant invasion
  # invader_properties$start_date,
  ## approx date of each peak (to compensate for asymmetric length of stay about peak---early on likely to get shorter occupancies, later on likely to get longer ones)
  # as.Date(c(
  #   "2020-05-15", ## wild-type
  #   "2021-01-12", ## Alpha
  #   "2021-04-20", ## Delta
  #   "2022-01-20", ## Omicron1
  #   "2022-05-01" ## Omicron2
  # ))

  ## monthly
  # seq(min(observed_data$date),
      # max(observed_data$date),
      # by='months')

  ## monthly until just before reports become unreliable
  # seq(min(observed_data$date), report_end_date - 1 ,
  #     by='months')
  # ## every 10 days after that point
  # , seq(report_end_date, max(observed_data$date),
  #       by = 10)

  ## monthly until omicron 1 invasion
  seq(min(observed_data$date),
      (invader_properties %>% filter(label == 'Omicron1') %>% pull(start_date)),
      by='months'),
  ## halfway through omicron 1
  (invader_properties %>% filter(label == 'Omicron1') %>% mutate(halfway_date = start_date + (end_date-start_date)/2) %>% pull(halfway_date)),
  ## start of omicron2
  (invader_properties %>% filter(label == 'Omicron2') %>% pull(start_date)),
  ## halfway through omicron2
  (invader_properties %>% filter(label == 'Omicron2') %>% mutate(halfway_date = start_date + (end_date-start_date)/2) %>% pull(halfway_date))
)

# ## insert midpoint dates
# midpoints <- (tibble(date = date_seq_rho) %>% mutate(date2 = lag(date), midpoint = date + (date2-date)/2) %>% pull(midpoint))

## sort dates
date_seq_rho <- sort(c(date_seq_rho))

manual_rho <- data.frame(
  Date = date_seq_rho
  , Symbol = "rho"
  , Value = NA
)

# ---------------------------
# Script output
# ---------------------------

params_timevar_opt <- bind_rows(
  params_timevar_opt,
  manual_rho
)
