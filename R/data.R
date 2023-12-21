#' Results from a univariate MCMC algorithm on a simulated posterior
#'
#' Results from a univariate Metropolis-Hastings algorithm run on a tri-modal posterior.
#' Although the standard traceplot and Gelman-Rubin diagnostic show good mixing, the results
#' are actually mixing poorly. Included for examples.
#'
#' @format ## `uniMCMCResults`
#' A list with 7 elements, each representing a different MCMC chain. Each element is
#' a data.frame with 2000 rows and 2 columns:
#' \describe{
#'   \item{val.1}{MCMC draw at current iteration}
#'   \item{Posterior}{Posterior value of val.1}
#' }
#' @source Luke Duttweiler
"uniMCMCResults"
