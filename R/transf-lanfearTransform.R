#' Transforms a list of mcmcObjs into a list of data.frames using the Lanfear transformation
#'
#' @param mhDraws A list of mcmcObjs
#' @param distance Distance function defined on the space of MCMC draws. See details.
#' @param reference Reference point (with exact same structure as each MCMC draw)
#'  for draw comparison. If left NULL a random point is selected from the given draws.
#'  See details.
#' @param ... Catches extra arguments. Not used.
#'
#' @details
#' The Lanfear transformation works by specifying a reference point and then comparing each
#' MCMC draw back to that reference point using a distance function. The function returns
#' this distance value as the Lanfear transformation of each draw.
#'
#' @return List of data.frames with columns 'val.1' which is the Lanfear transformation of each MCMC
#' draw, 'Posterior' which is the (non-normalized) posterior value of each MCMC draw and 't'
#' which gives the within-chain ordering of the MCMC draws. Each data.frame is a separate chain.
#'
#' @export
lanfearTransform <- function(mhDraws, distance, reference = NULL, ...){
  #if reference isn't specified, select a random reference
  if(is.null(reference)){
    #Select a random chain
    rChain <- sample(1:length(mhDraws), 1)

    #Select a random value from that chain
    mhChain <- mhDraws[[rChain]]
    mhChainVal <- mhChain[grepl('val', names(mhChain))]

    rNum <- sample(1:length(mhChainVal[[1]]), 1)
    reference <- mhChainVal[[1]][[rNum]]

  }

  #Turn each val value into the distance from the reference
  mhDists <- lapply(mhDraws, function(mhList){
    #Extract val, ignore Posterior
    mhList <- mhList$val

    #Loop through val (as if it is a list, even if its only a vector) and calculate the distance between
    #the value and the reference for each
    dist <- sapply(mhList, function(v){
      return(distance(v, reference))
    })

    #Notice we name distance as val in the dataframe for future functions
    return(data.frame(val.1 = dist, t = 1:length(dist)))
  })

  #Return transformed chains
  return(mhDists)
}
