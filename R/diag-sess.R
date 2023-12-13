sess <- function(mhDraws, ...){
  #Strip out unecessary info from mhDraws
  mhMCMC <- lapply(mhDraws, function(df){
    return(df[grepl('val', names(df))])
  })

  #Calculate effective sample size
  chainESS <- sapply(mhMCMC, multiESS)

  #Present as df and return
  chainESS <- as.data.frame(matrix(chainESS, nrow = 1))
  names(chainESS) <- paste0('Chain ', 1:length(mhDraws))
  chainESS$Sum <- rowSums(chainESS)
  row.names(chainESS) <- 'Effective Sample Size'

  return(chainESS)
}
