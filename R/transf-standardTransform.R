#' Transforms an mcmcObj into a dataframe with no modifications to values
#'
#' @param mhDraws An object of class mcmcObj
#' @param ... Not used.
#'
#' @return data.frame with rows that represent MCMC draws. Columns named val.1, val.2,...
#' contain different dimensions of the draws, column named Posterior contains (non-normalized)
#' posterior value of the draw
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
