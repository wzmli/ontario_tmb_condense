## function to selectively update environment
## to keep the working environment clean
addEnvironment <- function(prevs,new){
  rm(list=setdiff(ls(), c(prevs,new)))
  return(ls())
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
