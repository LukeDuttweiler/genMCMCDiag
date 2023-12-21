#' Euclidean Distance
#'
#' Simple function to return the Euclidean distance between two objects. Acts elementwise.
#'
#' @param x Numeric vector or matrix.
#' @param y Numeric vector or matrix of same dimensions as x.
#'
#' @note
#' For speed, no error handling if x and y do not have the same dimensions, take care!
#'
#' @return Numeric, elementwise Euclidean distance between x and y.
#' @export
#'
#' @examples
#' eucDist(c(0,0), c(1,1))
eucDist <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}

#' Hamming Distance
#'
#' Simple function to return the Hamming distance between two objects. Acts elementwise.
#'
#' @param x Binary vector or matrix
#' @param y Binary vector or matrix of same dimensions as x.
#'
#' @note
#' For speed, no error handling if x and y do not have the same dimensions. Also,
#' does not test to make sure x,y are binary, take care!
#'
#' @return Numeric, elementwise Hamming distance between x and y.
#' @export
#'
#' @examples
#' x <- matrix(c(1,0,
#'               0,0), nrow = 2, byrow = TRUE)
#' y <- diag(1,2)
#' hammingDist(x, y)
hammingDist <- function(x,y){
  return(sum(abs(x-y)))
}

#' Partition Distance
#'
#' Function to return the 'Partition' distance between two objects.
#'
#' @param x Data.frame with columns node and partition
#' @param y Data.frame with columns node and partition. Same nrows as x.
#'
#' @note
#' For speed, no error handling if x and y do not have the same dimensions. Also,
#' does not test to make sure x,y are integers, take care!
#'
#' @return Numeric, Partition distance between x and y.
#' @export
#'
#' @examples
#' x <- bnMCMCResults[[1]]$val[[1]]
#' y <- bnMCMCResults[[1]]$val[[100]]
#' partitionDist(x, y)
partitionDist <- function(x, y){
  #Sort by nodes
  x <- x[order(x$node),]
  y <- y[order(y$node),]

  #Take absolute difference of partition levels
  return(sum(abs(x$partition - y$partition)))
}
