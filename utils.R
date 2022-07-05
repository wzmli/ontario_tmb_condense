#' Clean up environment
#'
#' @param env existing environment list
#' @param vars_to_add new variables to add to environment list
clean_env <- function(env, vars_to_add){
  rm(list=setdiff(ls(envir = .GlobalEnv), c(env, vars_to_add)),
     envir = .GlobalEnv)
  return(ls(envir = .GlobalEnv))
}

#' Load parameters into global environment
#'
#' @param sheet sheet name
#' @param params_url url to a Google Sheet
load_params <- function(sheet, params_url){
  pp <- (
    read_sheet(
      params_url,
      sheet = sheet,
      col_types = 'c' ## in order to parse fractional values later
    )
    %>% select(symbol, value)
  )

  invisible(map2(pp$symbol,
                 pp$value,
                 function(sym,val){
                   assign(sym,
                          ## parse and evaluate from character string
                          eval(parse(text = val)),
                          envir = .GlobalEnv)}))
}
