#' Computes the mean of a vector
#' 
#' Computes the mean of a vector.
#' 
#' @param x A numeric vector.
#'           
#' @param weights A numeric vector, the same length as x, representing
#' the weight of each element.
#' 
#' @section What it does:
#' This function is written to demonstrate a simple principle: 
#' the what we think of as the "average" is a special case of
#' the dot product. It's just the product of a vector and a
#' corresponding vector of weights. Here's the formula for
#' the dot product again:
#' 
#' \deqn{a \cdot b = a_1 b_1 + a_2 b_2 + ... + a_n b_n}
#' 
#' @return A scalar value representing the mean of a vector.
#' 
#' @examples
#' # For the dot product between two words in a word-document matrix
#' data(P)
#' a = P["king",]
#' avg_mean(a)
#' @export
avg_mean = function(x, weights = c()) {
  n = length(x)
  if (length(weights) == 0) {
    weights = rep(1/n, n)
  }
  results = dot_product(x, weights)
  return(results)
}