# ---------------------------
# Load model parameters
# ---------------------------

cat("loading model parameters...\n")

params_url <- "https://docs.google.com/spreadsheets/d/13GBes6A2PMXITwfkyYw7E3Lt3odpb3tbiFznpVy8VhU/edit?usp=sharing"

## get default params
load_params("base_default", params_url)

## province-specific overwrites
load_params(paste0("base_", region), params_url)

## map to simplify variant data (bucket multiple strains under a single label)
variant_map <- data.frame(
  strain = c("Alpha", "B.1.438.1"
             , "Beta", "Gamma"
             , "Delta", "Delta AY.25", "Delta AY.27"
             , "Omicron BA.1", "Omicron BA.1.1"
             , "Omicron BA.2"
  )
  ## these labels should match those used in "variant_*" sheet loaded below
  , label = c("Alpha", "Alpha"
              , "Alpha", "Alpha" ## Hack! Changing beta and gamma to alpha
              , "Delta", "Delta", "Delta"
              , "Omicron1", "Omicron1"
              , "Omicron2"
  )
)

## load invading variant properties, including label, corresponding start date, end date, and vaccine efficacies against each variant
invader_properties <- (bind_rows(
  ## default variant params
  suppressMessages(read_sheet(
    params_url,
    sheet = "variant_default",
    col_types = 'c' ## in order to parse fractional values later
  ))
,
  ## region-specific variant params
  suppressMessages(read_sheet(
    params_url,
    sheet = paste0("variant_", region),
    col_types = 'c' ## in order to parse fractional values later
  )))
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
