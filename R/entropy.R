#' Computes the entropy of a vector
#' 
#' Computes the entropy of a vector.
#' 
#' @param x A numeric vector.
#'
#' @return A scalar value representing the entropy of a vector.
#' 
#' @examples
#' data(P)
#' a = P["king",]
#' entropy(a)
#' @export
entropy = function(x) {
  p = x / sum(x)
  results = -1 * sum(p * log(p), na.rm = T)
  return(results)
}
