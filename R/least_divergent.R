#' Computes the relative entropy over a matrix
#' 
#' Computes the relative entropy over a matrix
#' 
#' @param m A numeric matrix.
#' 
#' @param y A numeric vector.
#' 
#' @param num_results An integer.
#'           
#' 
#' @examples
#' # For the relative entropy over a word-document matrix:
#' data(P)
#' a = P["king",]
#' least_divergent(P, a)
#' @export
least_divergent = function(m, a, num_results = 10) {
  results = apply(m, 1, relative_entropy, a)
  return(sort(results[results != 0], decreasing = F)[1:num_results])
}