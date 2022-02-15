#' Computes the variance of a vector
#' 
#' Computes the variance of a vector.
#' 
#' @param x A numeric vector.
#'           
#' 
#' @section What it does:
#' The variance measures the extent to which the elements
#' in a sequence of values deviate from their mean. The 
#' "residuals" represent each value \eqn{x_i - \bar x}. For
#' any given vector, the variance is just the dot product of
#' the residuals against themselves, divided by \eqn{n} or
#' \eqn{n+1}.
#' 
#' 
#' The \emph{standard deviation} of a vector is just the square
#' root of the variance.
#'
#' @return A scalar value representing the variance of a vector.
#' 
#' @examples
#' # For the variance of a term's distribution in a 
#' word-document matrix:
#' data(P)
#' a = P["king",]
#' variance(a)
#' @export
variance = function(x, mode = "biased") {
  n = length(x)
  residuals_x = x - avg_mean(x)
  if (mode == "biased") {
    results = dot_product(residuals_x, residuals_x) / n
  }
  if (mode == "unbiased") {
    results = dot_product(residuals_x, residuals_x) / (n-1)
  }
  return(results)
}