# ---------------------------
# Mildness

## periodically re-fit severity (mu) and hospital occupancy (rho)
## to get better fits, especially after the reports signal drops out
date_seq_mu <- c(
  ## update for each variant invasion
  invader_properties$start_date
  # + weeks(2) ## offset from initial variant invasion to compensate for delayed effect on hospitalizations

  # ## monthly until reports become unreliable
  # seq(min(observed_data$date), report_end_date,
  #     by='months')
  # ## and once more on the start date of BA.2
  # , invader_properties %>% filter(label == "Omicron2") %>% pull(start_date)

  ## monthly update
  # seq(min(observed_data$date),
  #     max(observed_data$date),
  #     by='months')

  ## every 10 days after that point
  # , seq(report_end_date, ymd(max(observed_data$date)), by = 10)
)

## insert midpoint dates
midpoints <- (tibble(date = date_seq_mu) %>% mutate(date2 = lag(date), midpoint = date + (date2-date)/2) %>% pull(midpoint))

date_seq_mu <- sort(c(date_seq_mu, midpoints))

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
