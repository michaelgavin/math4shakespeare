#' Computes the correlation between two vectors
#' 
#' Computes the correlation between two vectors.
#' 
#' @param x A numeric vector.
#' 
#' @param y A numeric vector.
#' 
#' @param num_results An integer.
#'
#' @section What it does:
#' 
#' 
#' \deqn{\frac{Cov[X, Y]}{\sqrt Var[X] \times Var[Y]}}
#'
#' @return A scalar value representing the correlation between
#' two vectors.
#' 
#' @examples
#' # For the correlation between two terms' distribution in a 
#' word-document matrix:
#' data(P)
#' a = P["king",]
#' most_correlated(P, a)
#' @export
most_correlated = function(m, a, num_results = 10) {
  results = apply(m, 1, correlation, a)
  return(sort(results, decreasing = T)[1:num_results])
}
