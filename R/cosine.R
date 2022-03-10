#' Computes the cosine similarity between two vectors
#' 
#' Computes the cosine similarity between two vectors.
#' 
#' @param x A numeric vector.
#' 
#' @param y A numeric vector.
#'
#' @section What it does:
#' 
#' 
#' 
#'
#' @return A scalar value representing the cosine similarity between
#' two vectors.
#' 
#' @examples
#' # For the cosine between two terms' distribution in a 
#' word-document matrix:
#' data(P)
#' a = P["king",]
#' b = P["queen",]
#' cosine(a, b)
#' 
#' most_similar(P, a)
#' @export
cosine = function(x, y) {
  results = x %*% y / (x %*% x * y %*% y) 
  return(results)
}

#' @rdname most_similar
most_similar = function(m, word, num_results = 10) {
  results = apply(m, 1, cosine, m[word,])
  return(sort(results, decreasing = T)[1:num_results])
}
