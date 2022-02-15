#' Computes the standard deviation of a vector
#' 
#' Computes the standard deviation of a vector.
#' 
#' @param x A numeric vector.
#'           
#' 
#' @section What it does:
#' The \emph{standard deviation} of a vector is just the square
#' root of the variance.
#' 
#' \deqn{\sqrt Var[X]}
#'
#' @return A scalar value representing the standard deviation of a vector.
#' 
#' @examples
#' # For the standard deviation of a term's distribution in a 
#' word-document matrix:
#' data(P)
#' a = P["king",]
#' standard_deviation(a)
#' @export
standard_deviation = function(x, mode = "biased") {
  results = sqrt(variance(x, mode = mode))
  return(results)
}