#' mcmcDiag Class Definition
#'
#' A class for storing and handling MCMC diagnostics.
#'
#' @slot diagnostics A list containing MCMC diagnostic information.
#' @slot transformedDraws A list containing transformed MCMC draws.
#' @slot call A list containing information about the function call.
#'
#' @docType class
#' @name mcmcDiag-class
#' @rdname mcmcDiag-class
#' @exportClass mcmcDiag
mcmcDiag <- setClass("mcmcDiag", slots = c(diagnostics="list", transformedDraws="list",
                                           call = 'list'))


#' Print Method for mcmcDiag Objects
#'
#' Prints MCMC diagnostics and relevant information.
#'
#' @param x An object of class \code{mcmcDiag}.
#'
#' @return Prints MCMC diagnostics to the console.
#'
#' @exportMethod print
#' @aliases print.mcmcDiag
methods::setMethod('print',
                   signature(x = 'mcmcDiag'),
                   function(x){
                     #Extract method
                     m <- x@call$arguments$method

                     #Change to 'Custom' if custom function was used
                     if(!is.character(m)){
                       m <- 'Custom'
                     }

                     #Extract diagnostics
                     diags <- x@diagnostics

                     #Special handling for traceplot
                     if('traceplot' %in% names(diags)){
                       print(diags$traceplot)
                       diags <- diags[names(diags) != 'traceplot']
                     }

                     if(length(diags) !=0){
                       catStr <- paste0('----------------------------------------------------\n',
                                        'Generalized MCMC Diagnostics using ', m,
                                        ' Method \n----------------------------------------------------\n\n')

                       #Other diagnostics printed
                       for(d in names(diags)){
                         nm <- row.names(diags[[d]])[1]
                         row.names(diags[[d]]) <- NULL
                         strAdd <- paste0('|',nm, ': \n|---------------------------\n',
                                          paste0(knitr::kable(round(diags[[d]], 3)),
                                                 collapse = '\n'),
                                          '\n\n')
                         catStr <- paste0(catStr, strAdd)
                       }
                       cat(catStr)
                     }
                   })

#' Show - Inherited from package 'methods'
#' @importFrom methods show
#' @inheritParams methods::show
#' @inherit methods::show
#' @export
show <- methods::show

#' Show Method for mcmcDiag Objects
#'
#' Displays the content of an object of class \code{mcmcDiag}.
#'
#' @param object An object of class \code{mcmcDiag}.
#'
#' @return Displays the content of the object.
#'
#' @exportMethod show
#' @aliases show.mcmcDiag
methods::setMethod('show',
                   signature = 'mcmcDiag',
                   definition = function(object){
                     print(object)
                   })
