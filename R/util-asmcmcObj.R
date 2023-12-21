#' Turns a data.frame or list into an mcmcObj.
#'
#' @param x data.frame or list. Should have a specific format. See details.
#' @param ... Extra arguments passed to methods. Currently unused.
#'
#' @details
#' If x is a list, as.mcmcObj expects length(x) == 2 with x$val as a list (or vector) of
#' the draws from a single MCMC chain and x$Posterior as the corresponding Posterior values
#' of the draws in x$val. If these are fulfilled then x will be returned unchanged, otherwise
#' x is returned unchanged but with a warning.
#'
#' If x is a data.frame as.mcmcObj will create a list of vectors val, where val\[\[i\]\] is all
#' columns of x\[i,\] EXCEPT a column named 'Posterior', and a vector of posterior values where
#' Posterior\[i\] is x\[i, 'Posterior'\]. These are combined and returned as
#' list(val = val, Posterior = Posterior).
#'
#' @return Object of class 'mcmcObj' and 'list'.
#' @export
#'
#' @examples
#' # Example with a data.frame
#' df <- data.frame(matrix(rnorm(20), ncol = 5), Posterior = rnorm(4))
#' as.mcmcObj(df)
#'
#' # Example with a list
#' my_list <- list(val = list(c(1, 2), c(3, 4)), Posterior = c(0.1, 0.2))
#' as.mcmcObj(my_list)
#'
as.mcmcObj <- function(x, ...){
  UseMethod('as.mcmcObj')
}

#' @rdname as.mcmcObj
#' @export
#' @method as.mcmcObj data.frame
methods::setMethod("as.mcmcObj",
          signature(x = "data.frame"),
          function (x)
          {
            #Split into list of vectors and vector of posteriors
            val <- lapply(1:nrow(x), function(i){
              unlist(x[i,!(names(x) %in% 'Posterior')])
            })

            Posterior <- x$Posterior

            ret <- list(val = val, Posterior = Posterior)
            class(ret) <- c('mcmcObj', 'list')

            return(ret)
          }
)

#' @rdname as.mcmcObj
#' @export
#' @method as.mcmcObj list
methods::setMethod("as.mcmcObj",
          signature(x = "list"),
          function (x)
          {
            #Warn if names are wrong
            if(any(!(names(x) %in% c('val', 'Posterior')))){
              warning('names of lists in individual chains should be val or Posterior. Stuff might break.')
            }
            #Lists are all good, keep
            class(x) <- c('mcmcObj', 'list')

            return(x)
          }
)
