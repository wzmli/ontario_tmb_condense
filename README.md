# ontario_tmb_condense

## `data.R`

Read in data and produce two outputs:

* `observed_dat`
  * long-format with three columns: date, var, value
  * will be passed _as is_ into a `flexmodel` using `add_observed`
  * all names in `var` therefore need to be columns returned by the 
    `simulation_history` function
  * all dates in `date` must be within the `start_date` and `end_date`
    of the `flexmodel`
* `params_timevar`
  * long-format with three columns: Date, Symbol, Value
  * will be passed _as is_ into a `flexmodel` using `add_piece_wise`
  * all names in `Symbol` must therefore refer to parameters in the
    `params` vector of the associated `flexmodel`
  * all dates in `Date` must be within the `start_date` and `end_date`
    of the `flexmodel`


