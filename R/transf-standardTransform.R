#' Transforms a list of mcmcObjs into a list of dataframes with no modifications to values
#'
#' @param mhDraws An list of mcmcObjs
#' @param ... Not used.
#'
#' @return A list of data.frames with rows that represent MCMC draws.
#'  Columns named 'val.1', 'val.2',... contain different dimensions of the draws,
#'  column named 'Posterior' contains (non-normalized) posterior value of the draw and column
#'  named 't' gives the within-chain ordering of the draws. Each separate data.frame is a
#'  different chain.
#'
#'  @export
standardTransform <- function(mhDraws, ...){

  #Will try to force into dataframe of the expected type
  mhRet <- lapply(mhDraws, function(mhObj){
    mhDF <- as.data.frame(do.call('rbind', mhObj$val))
    names(mhDF) <- paste0('val.', rep(1:ncol(mhDF)))
    mhDF <- cbind(mhDF, data.frame(Posterior = mhObj$Posterior, t = 1:nrow(mhDF)))
    return(mhDF)
  })

  return(mhRet)
}
