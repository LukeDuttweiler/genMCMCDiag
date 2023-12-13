as.mcmcObj.list <- function(x){
  #Warn if names are wrong
  if(any(!(names(x) %in% c('val', 'Posterior')))){
    warning('names of lists in individual chains should be val or Posterior. Stuff might break.')
  }
  #Lists are all good, keep
  return(x)
}