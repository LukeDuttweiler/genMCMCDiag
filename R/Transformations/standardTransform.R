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