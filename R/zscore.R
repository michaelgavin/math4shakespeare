#' Computes the Z-score for each element of a vector
#' 
#' Computes the Z-score for each element of a vector.
#' 
#' @param x A numeric vector.
#'
#' @section What it does:
#' The \emph{Z}-score of a vector represents how many standard
#' deviations each value is above or below the mean of that
#' vector. It is the ratio between a vector's residuals and its
#' standard deviation.
#' 
#' \deqn{\frac{x_i - \bar x}{\sigma}}
#'
#' @return A numeric vector representing the Z-score for each element.
#' 
#' @examples
#' # For the z-score of a term's distribution in a 
#' word-document matrix:
#' data(P)
#' a = P["king",]
#' zscore(a)
#' @export
zscore = function(x) {
  results = (x - avg_mean(x)) / standard_deviation(x)
  return(results)
}