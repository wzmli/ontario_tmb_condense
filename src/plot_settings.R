# ---------------------------
# Define shared plot settings
# ---------------------------

theme_set(theme_bw())
theme_update(
  axis.title.y = element_blank()
  , strip.background = element_rect(fill = "white")
  , legend.position = "bottom"
)
fig.width <- 6 ## inches
date_range <- c(as.Date("2020-01-01"),
                today()) ## x-scale for plots
