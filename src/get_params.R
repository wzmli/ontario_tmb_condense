# ---------------------------
# Load model parameters
# ---------------------------

cat("loading model parameters...\n")

params_url <- "https://docs.google.com/spreadsheets/d/13GBes6A2PMXITwfkyYw7E3Lt3odpb3tbiFznpVy8VhU/edit?usp=sharing"

## get default params
load_params("base_default", params_url)

## province-specific overwrites
load_params(paste0("base_", region), params_url)

## update population size automatically from statcan data
N = get_popsize(region)

## map to simplify variant data (bucket multiple strains under a single label)
variant_map <- (
  read_csv(file.path("data", "variant_map.csv"), show_col_types = FALSE)
  %>% select(-notes)
  %>% as.data.frame()
)

## default variant params
invader_default <- suppressMessages(read_sheet(
    params_url,
    sheet = "variant_default",
    col_types = 'c' ## in order to parse fractional values later
  ))

if(!(paste0("variant_", region) %in% sheet_names(params_url))){
  stop(paste0("please prepare variant_", region, " sheet in parameter spreadsheet"))
}

## region-specific variant params
invader_pt <- suppressMessages(read_sheet(
    params_url,
    sheet = paste0("variant_", region),
    col_types = 'c' ## in order to parse fractional values later
  ))

## load invading variant properties, including label, corresponding start date, end date, and vaccine efficacies against each variant
invader_properties <- (bind_rows(
    invader_default
  , invader_pt
  )
  %>% select(label, symbol, value)
  %>% pivot_wider(id_cols = "label",
                  names_from = "symbol")
  ## convert columns to appropriate types
  %>% mutate(across(ends_with("date"), as.Date),
             across(starts_with("inv_"), as.numeric))
  ## convert to data frame
  %>% as.data.frame()
)

cat("model parameters loaded\n\n")
