#' Transforms a list of mcmcChains into a list of data.frames using the 'likelihood'
#' transformation
#'
#' @param mhDraws A list of mcmcChains
#' @param ... Catches extra arguments. Not used.
#'
#' @return List of data.frames with columns 'val.1' which is non-normalized
#' posterior of each draw and 't' which gives the within-chain ordering of the MCMC draws.
#'  Each data.frame is a separate chain.
#'
#'  @export
likelihoodTransform <- function(mhDraws, ...){
  #Throw error if no Posterior value
  if(is.null(mhDraws[[1]]@Posterior)){
    stop('The likelihood method requires posterior values for each chain.')
  }

  #Extract Posteriors
  likelihood <- lapply(mhDraws, function(mhChain){
    return(data.frame(val.1 = mhChain@Posterior, t = 1:length(mhChain@Posterior)))
  })

  #Notice we name likelihood as val in the dataframe.
  #This is because standardTraceplot() will plot the variable
  #named val on the y-axis as desired
  return(likelihood)
}
