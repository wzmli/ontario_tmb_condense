# ---------------------------
# Mildness

## periodically re-fit severity (mu) and hospital occupancy (rho)
## to get better fits, especially after the reports signal drops out
date_seq_mu <- c(
  ## monthly until reports become unreliable
  seq(min(observed_data$date), report_end_date,
      by='months')
  ## and once more on the start date of BA.2
  , invader_properties %>% filter(label == "Omicron2") %>% pull(start_date)
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
# Script output
# ---------------------------

params_timevar <- bind_rows(
  params_timevar,
  manual_mu
)
