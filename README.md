# ontario_tmb_condense (MLi refactor)

## to run the pipeline

Simply run `source("run_pipeline.R")` from the main directory.

## to change pipeline parameters

All pipeline parameters should be stored in a Google Sheet, e.g. [this one](https://docs.google.com/spreadsheets/d/13GBes6A2PMXITwfkyYw7E3Lt3odpb3tbiFznpVy8VhU/edit?usp=sharing). This is the only file you need to edit to make changes to the calibration and forecast (unless you're doing something major like changing the model, in which case, see below). 

There are several sheets in the file, which follow a naming convention:
- `params_*`: these sheets should have every parameter actually used in the model definition. There must be a value specified for each symbol, even if the value is only ever calibrated from data. Any symbol and value specified in the eponymous columns will be pulled into the `params` list used to initialize the `flexmodel()`. A value specified in these sheets will be used throughout each simulation, unless it is either calibrated instead or changed with a time-varying parameter specification. Currently, we have the following `params_*` sheets:
  - `params_base`: parameters for the base model (without vaccines and variants)
  - `params_vax`: vaccine-related parameters (for the wild-type; variant-based changes in vaccine parameters will be handled via a different spreadsheet)
  - `params_waning`: parameters related to the waning of infection-related immunity
  - `params_variant`: parameters related to the invasion of a variant. The values specified here are just placeholders, provided `inv_prop` (the invading variant proportion) is also initially set to 0; parameters for each variant's invasion are specified in the `tv_variant_*` sheets (described below).
- `map_*`: these sheets give lookup tables for various purposes.
  - `map_variant` maps strain names (as in the variant incidence data) to labels we use internally in our pipeline
  - `map_condense` maps labels used for different types of observations to the model symbol name associated (e.g. a state or accumulator variable name)
- (COMING SOON) `tv_variant_*`: each one of these sheets contains the parameters associated with each variant invasion. The pipeline will detect all sheets named in this way, so if you want to include a new invasion, simply name it `tv_variant_label`, where `label` is the label associated with the new invading variant (must match at least one entry in the `map_variant` table in order to pull in the frequency series associated with the invasion). There must be one `tv_variant_*` sheet for each variant label in the `map_variant` sheet.
- (COMING SOON) `settings_*`: these sheets contain the settings for calibration and forecast.
  - `settings_calibration`: all things calibration, including the number of break dates for beta0 to autodetect from the report data, manual beta0 breaks for after reports drop out, priors for calibrated parameters, etc
  - `settings_forecast`: all things forecast, including the number of days for which to forecast
- (COMING SOON) `tv_forecast`: a table of time-varying parameter schedules for the forecast period. These can encode assumptions on beta0, booster schedules, and/or a timeseries of new invading variant frequency.

---

TODO: EVERYTHING UNDER HERE IS POTENTIALLY OUTDATED/NEEDS REVIEW

## to change the model definition

The model is defined in `define_model.R`. This includes state variables, the list of parameter names, flow rates, condensation of state variables across subcategories, and the calculation of report variables. If you're going to modify the model definition, be sure to update the model parameters (including any time-varying parameter ones) in `pipeline_parameters.R` and potentially `time_varying_params.R`.

### to change data to which the model is being calibrated

The observed data is initially loaded in via `prep_observations.R`. This script loads the data and processes it minimally into a standardized (long) form, but it is not filtered or trimmed here in any way. Filtering and trimming occur later, in `calibration_setup.R`, where info about the calibration is encoded. There, the data are at least trimmed to lie between the simulation start date and calibration end date (defined in `calibration_setup.R`), though the user may choose to additionally filter out some variables from the original observed data (_e.g._ ICU prevalence) as not to fit to them.

For any observed data that is being fitted, one must specify a distribution for the observation error. This is done in `calibration_settings.R` using the function `update_error_dist()` (and possibly `update_params()` if using a negative binomial distribution, because then one must also specify a dispersion parameter).

### to change time-varying parameters input into the calibration

There are several parameters that one may wish to change in a time-varying way during the calibration, but that are not being calibrated themselves. For instance, vaccine dosing can be changed on a daily basis to reflect the daily dosing as recorded by each province. These inputs are specified in `input_tv_params.R`.

Time-varying parameters should be defined in a data-frame with the following columns:

| `Date` | `Symbol` | `Value` |
| ------ | -------- | ------- |

where
- `Date` is the date of the change to the parameter (as a `Date` object, usually in `YYYY-MM-DD` format)
- `Symbol` is the name of the parameter symbol used in the model definition (specified in `define_model.R`)
- `Value` is the new value for the parameter on the given `Date`

The pipeline assumes that these time-varying parameters are specified in an absolute (not relative) way.

### to specify start and end dates for calibration

To perform a calibration, one must at least provide the start date for the simulation and an end date for the calibration. Note that the _simulation_ start date is not necessarily the calibration start date as the observed data can start after the simulation in order to include a burn-in period for each simulation, bypassing the start of the outbreak where reported data may be sparse and high-variance. These dates are specified in `calibration_settings.R`.

### to specify forecast period length

One can set a number of days for which to run the forecast past the calibration end date in `forecast_settings.R`.

### to specify parameters to be calibrated

To fit base parameters, one tells the software which ones to calibrate using `update_opt_params()` in `calibration_settings.R`. See [the `McMasterPandemic` manual](https://canmod.github.io/macpan-book/calibration.html) for more details.

To fit time-varying parameters, one must additionally specify the schedule upon which the parameter changes. This is done in `prep_opt_tv_params_schedule.R`. The schedule is specified in the same format as for the non-calibrated time-varying parameters, that is,

| `Date` | `Symbol` | `Value` |
| ------ | -------- | ------- |

except that the `Value` should be set to `NA` to tell the software this value should be calibrated.

Once the schedule is specified, one attaches the schedule and any initial values or priors for the fitting using `update_opt_tv_params()` in `calibration_settings.R`.

### to specify a forecast scenario

Forecast scenarios are set up by specifying time-varying parameters for the forecast period in `forecast_settings.R`. For instance, one could specify the following:

| `Date`       | `Symbol` | `Value`   | `Type`     |
| ------------ | -------- | --------- | ---------- |
| `2022-06-11` | `beta0`  | `1.2`     | `rel_prev` |

to have the transmission rate, `beta0`, increase by 20% on `2022-06-11` relative to its last value, presumably a calibrated value. The date specified here is usually chosen to be the day after the calibration end date.

### other

If you want to tweak output plots, you can access their shared settings in `plot_settings.R`.

## files that should not have to be edited regularly

The following files support the pipeline in a model/forecast agnostic way, and should not have to be edited regularly:

- `pipeline_setup.R`
- `plot_settings.R`
- `calibrate.R`
- `forecast.R`
