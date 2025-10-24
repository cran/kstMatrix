#' Determine the notions of a knowledge structure
#'
#' \code{kmnotions2} returns a matrix representing the
#' notions of a knowledge structure.
#'
#' @param x Binary matrix representing a knowledge structure
#' @return Binary matrix representing notions in the knowledge structure
#'
#' The matrix has a '1' in row 'i' and column 'j' if 'i' and 'j' belong
#' to the same notion (i.e. are equivalent). It is a symmetric matrix
#' with '1's in the main diagonal.
#'
#' @examples
#' kmnotions2(xpl$space)
#'
#' @family Properties of knowledge structures
#'
#' @export
kmnotions2 <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  if (is.null(colnames(x))) colnames(x) <- as.character(1:dim(x)[2])

  l <- unique(lapply(colnames(x), function(item) {
    which(apply(x, 2, function(i) {
      all(i == x[,item])
    }))
  }))
  print(l)

  red <- matrix(rep(0, length(l)*dim(x)[2]), nrow = length(l))
  sapply((1:length(l)), function(n) {
    sapply(l[[n]], function(i) {
      red[n,i] <<- 1
      })
  })

  class(red) <- unique(c("kmfamset", class(red)))
  colnames(red) <- colnames(x)
  red
}
