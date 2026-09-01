#' Compute a binary matrix product
#'
#' \code{binary_matrix_product} expects two binary matrices and computes their Boolean product.
#'
#' @param m Binary matrix
#' @param n Binary matrix
#'
#' @return Boolean matrix product of m and n
#'
#' @examples
#' binary_matrix_product(xpl$sr, kmsurmiserelation(kmmaximalspace(4)))
#' binary_matrix_product(xpl$sr, kmsurmiserelation(kmminimalspace(4)))
#'
#'
#' @family Utilities
#'
#' @export
binary_matrix_product <- function(m,n) {
  if (dim(m)[2] != dim(n)[1]) {
    stop(sprintf("%s and %s do not fit in size!", dQuote("m"), dQuote("n")))
  }
  res <- matrix(rep(0,dim(m)[2]*dim(n)[1]), nrow=dim(m)[2])
  lapply((1:dim(m)[2]), function(x) {
    lapply((1:dim(n)[1]), function(y) {
      res[x,y] <<- max(m[x,] & n[,y])
    })
  })
  res
}
