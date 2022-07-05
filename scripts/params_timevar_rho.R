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
# Script output
# ---------------------------

params_timevar_opt <- bind_rows(
  params_timevar_opt,
  manual_rho
)
