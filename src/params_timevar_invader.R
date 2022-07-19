# ---------------------------
# Variant proportion
#
# load and tidy data on variant counts and frequencies
# ---------------------------

## established variants
## from covarnet
## --------------------------
variants_raw <- readRDS("data/covvarnet_voc.rds")

## This repo is for Ontario Only
## We will make a new repo later for other pts
region_name_long = c("Ontario")

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
                  %>% filter(province %in% region_name_long)
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

## for a newly invading variant (BA.4/5)
## ----------------------------

## read raw variant data
df <- bind_rows(
  read_csv(file.path("data", "ba4.csv")) %>% mutate(strain = "ba4"),
  read_csv(file.path("data", "ba5.csv")) %>% mutate(strain = "ba5"))

## tidy variant data
df <- (df
       %>% rename(date = x, percent = y)
       %>% separate(date, into = c("date", "time"),
                    sep = " ")
       %>% select(-time)
       %>% mutate(date = as.Date(date))
       ## combine ba4 and ba5 percentages and convert to proportion
       %>% group_by(date)
       %>% summarize(obs = sum(percent)/100, .groups = 'drop')
)
first_obs <- min(df$date)

## fill first obs date as start date for "Omicron5"
invader_properties[which(invader_properties$label == "Omicron5"), "start_date"] <- first_obs

## regression to project until the end of the calibration period
inv_prop_model <- glm(obs ~ date, family = "binomial", data = df)
save_obj("inv_prop_model", calib_end_date)
df <- (data.frame(date = seq(first_obs,
                             calib_end_date,
                             by = 1))
       %>% left_join(df, by = "date"))
df$pred <- predict(inv_prop_model,
                   newdata = df,
                   type = "response")

## diagnostic plot
p1 <- (ggplot(df,
              aes(x = date))
       + geom_point(aes(y = obs))
       + geom_line(aes(y = pred))
       + scale_y_continuous(limits = c(0, 1))
       + labs(title = "Proportion of BA.4 and BA.5 combined",
              subtitle = "(until calibration end date)"))

print(p1)

## convert to params_timevar format and bind to rows for established variants
params_timevar_inv_prop_new <- (
  df
    %>% select(-obs)
    %>% rename(Date = date, inv_prop = pred)
    %>% pivot_longer(-Date,
                     names_to = "Symbol",
                     values_to = "Value")
    %>% as.data.frame()
)
params_timevar_inv_prop <- rbind(
  params_timevar_inv_prop,
  params_timevar_inv_prop_new
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
    ## drop NA from the lag operation
    ## (wild-type values will be being used
    ## regardless for the first invasion because
    ## they're in the default parameters list)
    %>% drop_na()
  )

  return(df)
}

## prep all parameters that need to change upon an invasion
inv_params_list <- c("vax_VE_trans", "vax_VE_hosp")
params_timevar_inv_params <- bind_rows(
  lapply(inv_params_list,
         prep_invasion_params)
) %>% as.data.frame()

## attach transmission advantages, if specified
if("inv_trans_adv" %in% names(invader_properties)){
  params_timevar_inv_params <- rbind(
    params_timevar_inv_params,
    (invader_properties
    %>% filter(!is.na(inv_trans_adv))
    %>% select(start_date, inv_trans_adv)
    %>% rename(Date = start_date)
    %>% pivot_longer(-Date,
                     names_to = "Symbol",
                     values_to = "Value"))
    %>% mutate(Value = as.numeric(Value))
    %>% as.data.frame())
}

# ---------------------------
# Script output
# ---------------------------

params_timevar_data <- bind_rows(
  params_timevar_data,
  params_timevar_inv_prop,
  params_timevar_inv_params
)
