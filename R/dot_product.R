#' Computes the dot product between two vectors
#' 
#' Computes the dot product between two vectors.
#' 
#' @param x A numeric vector.
#'           
#' @param y A numeric vector, the same length as x.
#' 
#' @section What it does:
#' The dot product is a mathematical operation that allows you to
#' compare two vectors of numbers. The dot product is a mathematical
#' operation at the heart of many descriptive statistics. The dot
#' is calculated simply by multiply each corresponding element together,
#' then taking the sum of those elementwise products. 
#' 
#' 
#'
#' @return A scalar value representing the dot product between two vectors.
#' 
#' @examples
#' # For the dot product between two words in a word-document matrix
#' data(P)
#' a = P["king",]
#' b = P["queen",]
#' dot_product(a, b)
#' @export
dot_product = function(x, y) {
  results = sum(x * y, na.rm = T)
  return(results)
}
