lanfearTransform <- function(mhDraws, distance, reference = NULL){
  #if reference isn't specified, select a random reference
  if(is.null(reference)){
    #Select a random chain
    rChain <- sample(1:length(mhDraws), 1)
    
    #Select a random value from that chain
    mhChain <- mhDraws[[rChain]]
    mhChainVal <- mhChain[grepl('val', names(mhChain))]
    if(is.data.frame(mhChainVal)){
      rNum <- sample(1:nrow(mhChainVal), 1)
      reference <- as.numeric(mhChainVal[rNum,])
    }else{
      rNum <- sample(1:length(mhChainVal[[1]]), 1)
      reference <- mhChainVal[[1]][[rNum]]
    }
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