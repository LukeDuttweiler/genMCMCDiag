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