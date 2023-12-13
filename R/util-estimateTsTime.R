estimateTsTime <- function(distance, draw1, draw2, N){
  #Calculate time it takes to evaluate one instance of distance
  t1 <- Sys.time()
  mork <- distance(draw1, draw2)
  t2 <- Sys.time()
  
  oneTime <- t2 - t1
  
  #Standard estimate
  standardTime <- oneTime*(N)*(N-1)/2
  
  #Fuzzy Estimate
  fuzzyTime <- oneTime*(N*51 + (N^2)/200 - N/2 + 50*99)
  
  return(data.frame(Standard = round(standardTime/60), Fuzzy = round(fuzzyTime/60)))
}