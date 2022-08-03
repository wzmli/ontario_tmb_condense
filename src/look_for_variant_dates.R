library(tidyverse)
theme_set(theme_bw())
library(googlesheets4)
gs4_deauth()

region <- "ON"

## map to simplify variant data (bucket multiple strains under a single label)
variant_map <- (
  read_csv(file.path("data", "variant_map.csv"), show_col_types = FALSE)
  %>% select(-notes)
  %>% as.data.frame()
)

## this will error partway through, don't worry about it
invisible(try(source("src/params_timevar_invader.R")))

params_url <- "https://docs.google.com/spreadsheets/d/13GBes6A2PMXITwfkyYw7E3Lt3odpb3tbiFznpVy8VhU/edit?usp=sharing"
sheet_name <- paste0("variant_", region)
if(!(sheet_name %in% sheet_names(params_url))) stop(paste0("please create a ", sheet_name, " sheet with variant start and end dates in the params google sheet"))

variant_dates <- (read_sheet(
  params_url,
  sheet = sheet_name,
  col_types = 'c') ## in order to parse fractional values later
  %>% select(label, symbol, value)
  %>% pivot_wider(names_from = symbol)
  %>% mutate(across(ends_with("date"), as.Date))
)

df <- (variants_ts_all
  %>% filter(label != "other")
  %>% left_join(variant_dates, by = "label")
  %>% filter(date >= start_date, date <= end_date)
)

g <- (
  ggplot(df, aes(x = date, y = inv_prop, colour = label))
  + geom_line(size = 1.25)
  + scale_x_date(date_breaks = "2 months",
                 date_labels = "%b %Y")
  + labs(title = paste0("Model input for variant proportion in ", region))
  + theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.title = element_blank())
)

print(g)
