#' Generate Diagnostics for Markov Chain Monte Carlo Draws
#'
#' This function generates diagnostics for Markov Chain Monte Carlo (MCMC) draws, transforming the draws if specified, and evaluating selected diagnostics.
#'
#' @param mhDraws A list of MCMC draws, where each element is a list or numeric vector representing the output of a single MCMC chain.
#' @param method Method for transforming the MCMC draws. Options include 'standard', 'ts', 'lanfear', or a custom transformation function. See details.
#' @param diagnostics A character vector or list of diagnostic functions to be evaluated. Options include 'traceplot', 'ess', 'gelmanRubin', or custom functions. See details.
#' @param distance Function for evaluating distance between MCMC draws if required by 'method'. Note that the lanfear and ts methods ALWAYS require a distance function.
#' @param verbose If TRUE, informative messages are displayed.
#' @inheritDotParams tsTransform minDist fuzzy fuzzyDist
#' @inheritDotParams lanfearTransform reference
#'
#' @return An object of class 'mcmcDiag', containing evaluated diagnostics, transformed draws, and function call details.
#'
#' @details
#' Built-in transformation methods can be called with the appropriate character string in the 'method'
#' argument. For details on a particular method use ?lanfearTransform or ?tsTransform. Custom transform
#' functions may be added as well. A custom function must be written to accept a list of mcmcChain
#' type objects, and output a list of dataframes with columns val (the transformed draw) and t (the MCMC chain order).
#' Each element in the list is the transformed MCMC chain corresponding to the input.
#'
#' Built-in diagnostics can be called with the appropriate character string in the 'diagnostics'
#' argument. Additional custom diagnostic functions may be written. These functions should
#' act on a list of data.frames output from a transform function and should output as a relatively
#' small data.frame where the name of diagnostic is the first row.name.
#'
#'
#' @export
#'
#' @examples
#' #Example using standard Traceplot
#' tstS <- genDiagnostic(uniMCMCResults)
#' tstS
#'
#' #Example using 'lanfear' traceplot
#' tstL <- genDiagnostic(uniMCMCResults, method = 'lanfear', distance = eucDist,
#'                       reference = 0)
#' tstL
#'
#' #Example using 'ts' traceplot (not run here for sake of time during package build)
#' #tstTS <- genDiagnostic(uniMCMCResults, method = 'ts', distance = eucDist,
#' #                        fuzzy = TRUE, fuzzyDist = .5)
#' #tstTS
#'
#' #Examples using bayesian network sample data
#' tstBN1 <- genDiagnostic(bnMCMCResults, method = 'lanfear', distance = partitionDist)
#' tstBN1
#'
#' tstBN2 <- genDiagnostic(bnMCMCResults, method = 'ts', distance = partitionDist)
#' tstBN2
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
  #mhDraws <- lapply(mhDraws, as.list)

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

  retObj <- methods::new('mcmcDiag', diagnostics = diagRet, transformedDraws = mhTransformed,
                call = list(arguments = argg,
                            methodArguments = methodArgs))

  return(retObj)
}
