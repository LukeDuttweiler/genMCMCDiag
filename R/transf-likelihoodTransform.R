#' Transforms a list of mcmcObjs into a list of data.frames using the 'likelihood'
#' transformation
#'
#' @param mhDraws A list of mcmcObjs
#' @param ... Catches extra arguments. Not used.
#'
#' @return List of data.frames with columns 'val.1' which is non-normalized
#' posterior of each draw and 't' which gives the within-chain ordering of the MCMC draws.
#'  Each data.frame is a separate chain.
likelihoodTransform <- function(mhDraws, ...){
  #Throw error if no Posterior value
  if(is.null(mhDraws[[1]]$Posterior)){
    stop('The likelihood method requires posterior values for each chain.')
  }

  #Extract Posteriors
  likelihood <- lapply(mhDraws, function(df){
    return(data.frame(val.1 = df$Posterior, t = 1:length(df$Posterior)))
  })

  #Notice we name likelihood as val in the dataframe.
  #This is because standardTraceplot() will plot the variable
  #named val on the y-axis as desired
  return(likelihood)
}
