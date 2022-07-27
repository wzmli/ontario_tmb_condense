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
gs4_deauth() ## bypass authentication, not needed for public sheets

## macpan technical options (see manual for details)
options(MP_default_do_sim_constraint = TRUE)
options(MP_get_bbmle_init_from_nlminb = TRUE)

# ---------------------------
# Make output directories (not tracked by git)
# ---------------------------

if(!file.exists("figs")) dir.create("figs")
if(!file.exists("obj")) dir.create("obj")

# ---------------------------
# Load utils + plot settings
# ---------------------------

source(file.path("src", "utils.R"))
source(file.path("src", "plot_settings.R"))

