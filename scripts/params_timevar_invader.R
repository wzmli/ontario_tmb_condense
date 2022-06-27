# ---------------------------
# Variant proportion
#
# load and tidy data on variant counts and frequencies
# ---------------------------

variants_raw <- readRDS("metadata/covvarnet_voc.rds")

## This repo is for Ontario Only
## We will make a new repo later for other pts
major_prov = c("Ontario")

variants_tidy <- data.frame()

for(i in names(variants_raw)){
  date <- rownames(variants_raw[[i]])
  tempdf <- (variants_raw[[i]]
             %>% mutate(province = i
                        , date = date
             )
             %>% select(date,province,everything())
  )
  rownames(tempdf) <- NULL

  variants_tidy <- bind_rows(variants_tidy,tempdf)
}

print(names(variants_tidy))

variants_long <- (variants_tidy
                  %>% filter(province %in% major_prov)
                  %>% pivot_longer(names_to = "strain",
                                   values_to= "count",
                                   -c("date","province"))
                  ## tack on lookup table for variant names
                  %>% left_join(variant_map, by = "strain")
                  %>% mutate(
                    label = ifelse(is.na(label), "other", label)
                    , date = as.Date(date)
                  )
)

variants_ts_all <- (variants_long
                    %>% group_by(date, province, label)
                    %>% summarise(simple_count = sum(count,na.rm=TRUE), .groups = "drop")
                    %>% group_by(date,province)
                    %>% mutate(simple_sum = sum(simple_count,na.rm=TRUE))
                    %>% filter(simple_sum != 0)
                    %>% mutate(inv_prop = simple_count/simple_sum)
                    %>% arrange(province, date, label)
                    %>% ungroup()
)

## filter each variant down to desired start and end dates
variants_ts <- (
  variants_ts_all
  %>% left_join(
    invader_properties %>% select(label, start_date, end_date),
    by = "label")
  %>% filter(!is.na(start_date))
  %>% mutate(is_invading =
               between(date,
                       start_date,
                       end_date))
  %>% filter(is_invading)
)

gg <- (
  ggplot(variants_ts_all,
         aes(x = date, y = inv_prop, color = label,
             group = label))
  + geom_line()
  + facet_wrap(~province)
  + scale_x_date(date_labels = "%Y")
  + theme(legend.position = "bottom")
)

print(gg)

print(gg %+% variants_ts)

## make params_timevar lines for invader proportion
## NOTE: there is no interpolation here! inv_prop will be piecewise constant
params_timevar_inv_prop <- (
  variants_ts
  %>% rename(Date = date,
             Value = inv_prop)
  %>% mutate(Symbol = "inv_prop")
  %>% select(Date, Symbol, Value)
  %>% as.data.frame()
)

# ---------------------------
# Variant parameter changes
#
# For each invasion, there is a resident and invader
# Several parameters have to change roles from invader to resident upon a new invasion
# That gets taken care of here
# ---------------------------

#' Prepare time-varying invasion parameters
#'
#' Subset the invader_properties df defined in pipeline_parameters.R to pull out a specific set of parameters, then attach resident parameters for each invasion and put in standard params_timevar format
#'
#' @param params_prefix prefix for a set of parameters (the non-invader version)
#'
#' @return a `data.frame` of time-varying parameter changes
#'
#' @examples params_prefix("vax_VE_trans")

prep_invasion_params <- function(
    params_prefix
){
  df <- (invader_properties
         %>% select(start_date, contains(params_prefix))
         ## attach corresponding resident VEs for each invasion
         ## (take VE from previous variant)
         %>% mutate(across(contains(params_prefix),
                           lag,
                           .names = "not_{.col}"))
  )
  ## fill in wild-type parameter values for first invasion (get from base parameters list)
  df[1, names(df)[grep("^not_inv",
                       names(df))]] <- c(
                         unname(params[grep(paste0("^", params_prefix), names(params))]))

  df <- (
    df
    %>% rename(Date = start_date)
    %>% pivot_longer(-Date,
                     names_to = "Symbol",
                     values_to = "Value")
    ## correct parameter names
    %>% mutate(Symbol = str_replace(Symbol,
                                    "not_inv_",
                                    ""))
  )

  return(df)
}

## prep all parameters that need to change upon an invasion

inv_params_list <- c("vax_VE_trans", "vax_VE_hosp",
                     "trans_adv")

params_timevar_inv_params <- bind_rows(
  lapply(inv_params_list,
         prep_invasion_params)
)

# ---------------------------
# Script output
# ---------------------------

params_timevar_invader <- bind_rows(
  params_timevar_inv_prop,
  params_timevar_inv_params
)

params_timevar <- bind_rows(
  params_timevar,
  params_timevar_invader
)
