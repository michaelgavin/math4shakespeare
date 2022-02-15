#' Computes the correlation between two vectors
#' 
#' Computes the correlation between two vectors.
#' 
#' @param x A numeric vector.
#' 
#' @param y A numeric vector.
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
#' b = P["queen",]
#' correlation(a, b)
#' 
#' most_correlated(P, a)
#' @export
correlation = function(x, y) {
  results = covariance(x, y) / sqrt(variance(x) * variance(y))
  return(results)
}

most_correlated = function(m, word, num_results = 10) {
  results = apply(m, 1, correlation, m[word,])
  return(sort(results, decreasing = T)[1:num_results])
}
