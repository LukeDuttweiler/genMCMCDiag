#' Transforms a list of mcmcChains into a list of data.frames using the 'sum' transformation
#'
#' @param mhDraws A list of mcmcChains
#' @param ... Catches extra arguments. Not used.
#'
#' @return List of data.frames with columns 'val.1' which is the sum of all dimensions of each MCMC
#' draw, 'Posterior' which is the (non-normalized) posterior value of each MCMC draw and 't'
#' which gives the within-chain ordering of the MCMC draws. Each data.frame is a separate chain.
#'
#' @export
sumTransform <- function(mhDraws, ...){
  #Turn each val value into sum of the parts
  mhSums <- lapply(mhDraws, function(mhChain){

    #Loop through val (as if it is a list, even if its only a vector) and calculate the distance between
    #the value and the reference for each
    sums <- sapply(mhChain@val, function(v){
      return(sum(v))
    })

    #Notice we name distance as val in the dataframe for future functions
    return(data.frame(val.1 = sums, t = 1:length(sums)))
  })

  return(mhSums)
}
