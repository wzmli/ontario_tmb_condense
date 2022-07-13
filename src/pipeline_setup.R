# ---------------------------
# Set up pipeline
# ---------------------------

## deal with package conflict rules
conflictRules("MASS", exclude = "select")

## load libraries right at the start of the pipeline
library(McMasterPandemic)
library(segmented)
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(jsonlite)
library(patchwork)
library(googlesheets4)

# ---------------------------
# Make output directories (not tracked by git)
# ---------------------------

if(!file.exists("figs")) dir.create("figs")
if(!file.exists("obj")) dir.create("obj")

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


## Macpan technical options (see manual for details)
options(MP_default_do_sim_constraint = TRUE)
options(MP_get_bbmle_init_from_nlminb = TRUE)

## load utils
source("src/utils.R")

