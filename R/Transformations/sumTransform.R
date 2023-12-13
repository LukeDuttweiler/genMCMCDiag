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