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
#' cosine_similarity(a, b)
#' 
#' most_similar(P, a)
#' @export
cosine_similarity = function(x, y) {
  results = x %*% y/(sqrt(x %*% x) * sqrt(y %*% y))
  return(results)
}
