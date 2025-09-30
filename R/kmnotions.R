#' Determine the notions of a knowledge structure
#'
#' \code{kmnotions} returns a matrix representing the
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
#' kmnotions(xpl$space)
#'
#' @family Properties of knowledge structures
#'
#' @export
kmnotions <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  noi <- dim(x)[2]
  sr <- matrix(rep(0, noi*noi), nrow = noi, ncol = noi)
  for (i in 1:noi) {
    for (j in 1:i) {
      if (all(x[,i] == x[,j])) {
        sr[j,i] <- 1
        sr[i,j] <- 1
      }
    }
  }
  rownames(sr) <- colnames(x)
  colnames(sr) <- colnames(x)
  unique(sr)
}
