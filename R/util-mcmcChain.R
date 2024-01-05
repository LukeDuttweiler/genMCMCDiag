#' mcmcChain Class Definition
#'
#' A class for storing MCMC results from a single chain.
#'
#' @slot val A list containing the MCMC draws from a single chain IN ORDER.
#' @slot Posterior A vector containing the Posterior values corresponding to
#' the draws in val
#'
#' @docType class
#' @name mcmcChain-class
#' @rdname mcmcChain-class
#' @exportClass mcmcChain
mcmcChain <- setClass("mcmcChain", slots = c(val = 'list', Posterior = 'vector'))


#' Turns a data.frame or list into an mcmcChain.
#'
#' @param x data.frame or list. Should have a specific format. See details.
#' @param ... Extra arguments passed to methods. Currently unused.
#'
#' @details
#' If x is a list, as.mcmcChain expects length(x) == 2 with x$val as a list (or vector) of
#' the draws from a single MCMC chain and x$Posterior as the corresponding Posterior values
#' of the draws in x$val. If these are fulfilled then x will be returned unchanged, otherwise
#' x is returned unchanged but with a warning.
#'
#' If x is a data.frame as.mcmcChain will create a list of vectors val, where val\[\[i\]\] is all
#' columns of x\[i,\] EXCEPT a column named 'Posterior', and a vector of posterior values where
#' Posterior\[i\] is x\[i, 'Posterior'\]. These are combined and returned as
#' list(val = val, Posterior = Posterior).
#'
#' @return Object of class 'mcmcChain'.
#' @export
#'
#' @examples
#' # Example with a data.frame
#' df <- data.frame(matrix(rnorm(20), ncol = 5), Posterior = rnorm(4))
#' as.mcmcChain(df)
#'
#' # Example with a list
#' my_list <- list(val = list(c(1, 2), c(3, 4)), Posterior = c(0.1, 0.2))
#' as.mcmcChain(my_list)
#'
as.mcmcChain <- function(x, ...){
  UseMethod('as.mcmcChain')
}

#' @rdname as.mcmcChain
#' @export
#' @method as.mcmcChain data.frame
methods::setMethod("as.mcmcChain",
          signature(x = "data.frame"),
          function (x)
          {
            #Split into list of vectors and vector of posteriors
            val <- lapply(1:nrow(x), function(i){
              unlist(x[i,!(names(x) %in% 'Posterior')])
            })

            Posterior <- x$Posterior

            ret <- mcmcChain(val = val, Posterior = Posterior)

            return(ret)
          }
)

#' @rdname as.mcmcChain
#' @export
#' @method as.mcmcChain list
methods::setMethod("as.mcmcChain",
          signature(x = "list"),
          function (x)
          {
            #Warn if names are wrong
            if(any(!(names(x) %in% c('val', 'Posterior')))){
              warning('names of lists in individual chains should be val or Posterior. Stuff might break.')
            }

            x <- mcmcChain(val = x$val, Posterior = x$Posterior)

            return(x)
          }
)
