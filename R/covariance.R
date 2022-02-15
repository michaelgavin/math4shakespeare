#' Computes the covariance between two vectors
#' 
#' Computes the covariance between two vectors.
#' 
#' @param x A numeric vector.
#'           
#' 
#' @section What it does:
#' The covariance is the dot product between the residuals
#' of two vectors, divided by their length.
#' 
#' \deqn{Cov[X] = \frac{\sum(x_i - \bar x)(y_i - \bar y)}{n}}
#'
#' @return A scalar value representing the covariance between
#' two vectors.
#' 
#' @examples
#' # For the covariance between two terms' distributions in a 
#' word-document matrix:
#' data(P)
#' a = P["king",]
#' b = P["queen",]
#' covariance(a)
#' @export
covariance = function(x, y) {
  n = length(x)
  residuals_x = x - avg_mean(x)
  residuals_y = y - avg_mean(y)
  results = dot_product(residuals_x, residuals_y) / n
  return(results)
}