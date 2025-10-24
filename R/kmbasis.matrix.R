#' Compute the basis of a knowledge space
#'
#' \code{kmbasis.matrix} returns a matrix representing the basis of a
#' knowledge space. If \code{x} is a knowledge structure or an
#' arbitrary family of sets \code{kmreduction} returns the basis of
#' the smallest knowledge space containing \code{x}.
#'
#' @param x Binary matrix representing a knowledge space
#' @param ... Space or future, optiona parameters
#' @return Binary matrix representing the basis of the knowledge space.
#'
#' @examples
#' kmbasis(xpl$space)
#'
#' @family Different representations for knowledge spaces
#'
#' @export
kmbasis.matrix <- function(x, ...) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  if (dim(x)[1] == 1) {
    if (sum(x) > 0)
      return(x)
    else
      return(NULL)
  }

  noi <- as.integer(dim(x)[2])
  nos <- as.integer(dim(x)[1])
  storage.mode(x) <- "integer"
  nob <- as.integer(0)

  result <- .C("basis_reduction", noi, nos, t(x), nob, package="kstMatrix")
  nob <- result[[4]][1]

  basis <- matrix(1:(noi*nob), ncol=noi, nrow=nob, byrow = TRUE)
  storage.mode(basis) <- "integer"
  result2 <- .C("basis_results", basis, package="kstMatrix")
  b <- matrix(result2[[1]], ncol=noi, nrow=nob, byrow=TRUE)
  colnames(b) <- colnames(x)
  class(b) <- c("kmbasis", "kmfamset", class(basis))
  b
}
