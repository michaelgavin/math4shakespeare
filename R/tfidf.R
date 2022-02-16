#' Computes the TF-IDF score for each element of a matrix
#' 
#' Computes the TF-IDF score for each element of a matrix.
#' 
#' @param m A numeric matrix.
#' 
#'           
#' 
#' @return A matrix representing the TF-IDF score of each value.
#' 
#' @examples
#' data(P)
#' P_tfidf = tfidf(P)
#' @export
tfidf = function(m) {
  df = apply(m, 1, FUN = function(x) length(x[x>0]))
  N = ncol(m)
  idf = log(N / df)
  TFIDF = matrix(0,nrow(m),ncol(m))
  rownames(TFIDF) = rownames(m)
  colnames(TFIDF) = colnames(m)
  for (i in 1:nrow(m)) {
    tf = m[i,]
    TFIDF[i,] = tf * idf[i]
  }
  return(TFIDF)
}
