# ontario_tmb_condense (MLi refactor)

## to run the pipeline

Simply run `source("run_pipeline.R")` from the main directory.

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

### to change the model definition

The model is defined in `define_model.R`. This includes state variables, base parameters, flow rates, condensation of state variables across subcategories, and the calculation of report variables.

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
