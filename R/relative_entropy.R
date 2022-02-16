#' Computes the relative entropy between two vectors
#' 
#' Computes the relative entropy between two vectors.
#' 
#' @param x A numeric vector.
#' 
#' @param y A numeric vector.
#'           
#' 
#' @return A scalar value representing the relative entropy between
#' two vectors.
#' 
#' @examples
#' # For the relative entropy between two terms' distributions in a 
#' word-document matrix:
#' data(P)
#' a = P["king",]
#' b = P["queen",]
#' relative_entropy(a, b)
#' @export
relative_entropy = function(x, y) {
  p = x / sum(x)
  q = y / sum(y)
  p = p + .0000000001
  q = q + .0000000001
  results = sum(p * log(p / q), na.rm = T)
  return(results)
}

#' @rdname least_divergent
least_divergent = function(m, word, num_results = 10) {
  results = apply(m, 1, relative_entropy, m[word,])
  return(sort(results[results != 0], decreasing = F)[1:num_results])
}