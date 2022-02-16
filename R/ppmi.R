#' Computes the PPMI score for each element of a matrix
#' 
#' Computes the PPMI score for each element of a matrix.
#' 
#' @param m A numeric matrix.
#' 
#'           
#' 
#' @return A matrix representing the PPMI score of each value.
#' 
#' @examples
#' data(P)
#' P_ppmi = ppmi(P)
#' @export
ppmi = function(m) {
  pxy = m / sum(m)
  pxpy = rowSums(pxy) %*% t(colSums(pxy))
  PPMI = log(pxy / pxpy)
  PPMI[PPMI < 0] = 0
  PPMI[is.na(PPMI)] = 0
  return(PPMI)
}