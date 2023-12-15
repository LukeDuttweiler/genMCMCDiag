#' Calculate Gelman-Rubin statistic for multi-chain samples of a univariate (or transformed univariate) MCMC algorith from an mcmcObj
#'
#' @param mhDraws Object of mcmcObj class
#' @param ... Catches unnecessary additional arguments
#'
#' @return Data.frame with 1 row and 2 columns. First entry gives estimated GR statistic, second gives upper 95% limit for GR statistic.
#'
sgelmanRubin <- function(mhDraws, ...){

  #Check for a val.2 column
  if(!is.null(mhDraws[[1]]$val.2)){
    warning('Did you enter a multivariate chain with the standard transform? If so, the G-R statistic will only reflect the first variable.')
  }

  #Strip out unecessary info from mhDraws
  mhMCMC <- lapply(mhDraws, function(df){
    return(coda::mcmc(df[grepl('val.1', names(df))]))
  })

  #Make into mcmcm.list
  mhMCMC <- coda::as.mcmc.list(mhMCMC)

  #Get gelman object
  gel <- coda::gelman.diag(mhMCMC, autoburnin = FALSE)

  #Return as presented dataframe
  df <- as.data.frame(gel$psrf)
  row.names(df) <- 'Gelman-Rubin Diagnostic'

  return(df)
}
