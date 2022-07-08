# ---------------------------
# Length of stay, acute care
# ---------------------------

date_seq_rho <- c(
  ## update for each variant invasion
  invader_properties$start_date
  # + weeks(2) ## offset from initial variant invasion to compensate for delayed effect on hospitalizations

  # ## monthly until reports become unreliable
  # seq(min(observed_data$date), report_end_date,
  #     by='months')
  # ## every 10 days after that point
  # , seq(report_end_date, max(observed_data$date),
  #       by = 10)
)

## insert midpoint dates
midpoints <- (tibble(date = date_seq_rho) %>% mutate(date2 = lag(date), midpoint = date + (date2-date)/2) %>% pull(midpoint))

date_seq_rho <- sort(c(date_seq_rho, midpoints))

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
