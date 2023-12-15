#' Generate a traceplot of MCMC draws from an mcmcObj
#'
#' @param mhDraws Object of class mcmcObj
#' @param method Character string - Name of method used to generate traceplot. Is used to generate the title of the traceplot.
#' @param ... Catches unused arguments
#'
#' @return ggplot2 plot object showing traceplot
#'
straceplot <- function(mhDraws, method = NULL, ...){
  #Check for a val.2 column
  if(!is.null(mhDraws[[1]]$val.2)){
    warning('Did you enter a multivariate chain with the standard transform? If so, the traceplot will only reflect the first variable.')
  }

  #Get number of chains
  nChain <- length(mhDraws)

  #Extract draws
  pltList <- lapply(1:nChain, function(i){
    chain <- mhDraws[[i]]
    chain$Chain <- i
    return(chain[,c('t', 'val.1', 'Chain')])
  })
  pltDF <- do.call('rbind', pltList)

  #Make sure label is a factor
  pltDF$Chain <- as.factor(pltDF$Chain)

  if(!is.null(method)){
    if(is.character(method)){
      title <- paste0('Traceplot - Method: ', method)
    }else{
      title <- paste0('Traceplot - Method: Custom')
    }
  }

  #make plot
  plt <- ggplot(data = pltDF) + geom_line(aes(x = t, y = val.1, col = Chain))
  plt <- plt + theme_bw() + ggtitle(title) + theme(legend.position = 'none')
  return(plt)
}
