#' Computes the cosine similarity over a matrix
#' 
#' Computes the cosine similarity over a matrix.
#' 
#' @param m A numeric matrix.
#' 
#' @param a A numeric vector.
#'
#' @param num_results An integer.
#' 
#' @examples
#' # For the cosine similarity over a word-document matrix:
#' data(P)
#' a = P["king",]
#' most_similar(P, a)
#' @export
most_similar = function(m, a, num_results = 10) {
  results = apply(m, 1, cosine, a)
  return(sort(results, decreasing = T)[1:num_results])
}
