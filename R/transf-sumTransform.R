#' Transforms MCMC draws from an mcmcObj into a data.frame using the 'sum' transformation
#'
#' @param mhDraws An object of class mcmcObj
#' @param ... Catches extra arguments. Not used.
#'
#' @return data.frame with columns 'val.1' which is the sum of all dimensions of each MCMC
#' draw, and 'Posterior' which is the (non-normalized) posterior value of each MCMC draw.
sumTransform <- function(mhDraws, ...){
  #Turn each val value into sum of the parts
  mhSums <- lapply(mhDraws, function(mhList){

    #Loop through val (as if it is a list, even if its only a vector) and calculate the distance between
    #the value and the reference for each
    sums <- sapply(mhList$val, function(v){
      return(sum(v))
    })

    #Notice we name distance as val in the dataframe for future functions
    return(data.frame(val.1 = sums, t = 1:length(sums)))
  })

  return(mhSums)
}
