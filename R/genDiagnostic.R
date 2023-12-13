genDiagnostic <- function(mhDraws,
                          method = c('standard', 'ts', 'lanfear', 'likelihood'),
                          diagnostics = c('traceplot', 'ess', 'gelmanRubin'),
                          distance = NULL,
                          verbose = TRUE,
                          ...){
  #Catch arguments, except for full draws
  argg <- as.list(environment())[-1]
  if(is.character(argg$method)){
    argg$method <- argg$method[1]
  }
  methodArgs <- list(...)
  
  #Make sure mhDraws is of the correct format
  if(!is.list(mhDraws)){
    stop('mhDraws must be a list with length equal to the number of chains')
  }
  mhDraws <- lapply(mhDraws, as.mcmcObj)
  
  #Make sure diagnostics is not empty
  if(length(diagnostics) == 0){
    stop('diagnostics must contain at least one character or function.')
  }
  
  #Make sure every entry in diagnostics is in the correct format, create names
  diagNames <- sapply(diagnostics, function(d){
    if(!is.function(d)){
      if(!(d %in% c('traceplot', 'ess', 'gelmanRubin'))){
        stop("each diagnostic must be a function or one of 'traceplot','ess','gelmanRubin'.")
      }
      #If not a function and a character, use d as name
      return(d)
    }else{
      return('custom')
    }
  })
  
  #Warnings regarding distance
  if(is.function(method) & !is.null(distance)){
    if(verbose){
      message('method is a custom function and may ignore the specified distance.')
    }else{
      warning('method is a custom function and may ignore the specified distance.')
    }
  }else if(is.character(method)){
    if(!(method[1] %in% c('ts', 'lanfear')) & !is.null(distance)){
      if(verbose){
        message('this method ignores the argument distance.')
      }else{
        warning('this method ignores the argument distance.')
      }
    }else if((method[1] %in% c('ts', 'lanfear')) & is.null(distance)){
      stop('distance must be specified for this method.')
    }
  }
  
  
  #Select Method
  if(is.character(method)){ #If method is character, must be one of the options
    #Make sure method is used correctly
    if(!(method[1] %in% c('standard', 'ts', 'lanfear', 'likelihood'))){
      stop("method must be a function or one of 'standard', 'ts', 'lanfear', 'likelihood'.")
    }
    
    methodF <- get(paste0(method[1], 'Transform'))
  }else if(is.function(method)){#If method is function change call name
    methodF <- method
  }else{
    stop("method must be a function or one of 'standard', 'ts', 'lanfear', 'likelihood'.")
  }
  
  #Get transformed data
  mhTransformed <- methodF(mhDraws = mhDraws, distance = distance, verbose = verbose, ...)
  
  #Select and evaluate diagnostics
  diagRet <- lapply(diagnostics, function(d){
    if(is.character(d)){#If diagnostic is character, fetch correct diagnostic
      diagF <- get(paste0('s', d))
    }else if(is.function(d)){#If diagnostic is function, change name
      diagF <- d
    }else{
      stop("each diagnostic must be a function or one of 'traceplot','ess','gelmanRubin'.")
    }
    
    #Return evaluated diagnostic
    return(diagF(mhTransformed, method = method))
  })
  names(diagRet) <- diagNames
  
  #Create return object
  retObj <- list(diagnostics = diagRet,
                 transformedDraws = mhTransformed,
                 call = list(arguments = argg,
                             methodArguments = methodArgs))
  class(retObj) <- c('mcmcDiag', 'list')
  
  return(retObj)
}