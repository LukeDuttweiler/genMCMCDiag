as.mcmcObj.data.frame <- function(x){
  #Split into list of vectors and vector of posteriors
  val <- lapply(1:nrow(x), function(i){
    unlist(x[i,!(names(x) %in% 'Posterior')])
  })
  
  Posterior <- x$Posterior
  
  return(list(val = val, Posterior = Posterior))
}