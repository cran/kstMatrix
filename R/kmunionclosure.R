#' Close a family of sets under union
#'
#' \code{kmunionclosure} returns a matrix representing a knowledge space. Please note
#' that it may take quite some time for computing larger knowledge spaces.
#'
#' @param x Binary matrix representing a family of sets
#' @return Binary matrix representing the corresponding knowledge space, i.e. the
#'         closure of the family under union including the empty set and
#'         the full set.
#'
#' \code{kmunionclosure} implements the irredundant algorithm developed by Dowling (1993).
#'
#' @references Dowling, C. E. (1993). On the irredundant construction of knowledge spaces.
#' Journal of Mathematical Psychology, 37, 49–62.
#'
#' @examples
#' kmunionclosure(xpl$basis)
#'
#' @useDynLib kstMatrix
#'
#'
#' @keywords math
#' @family Different representations for knowledge spaces
#'
#' @export
kmunionclosure <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  x <- kmbasis(x)

  noi <- as.integer(dim(x)[2])
  nob <- as.integer(dim(x)[1])
  storage.mode(x) <- "integer"
  nos <- as.integer(0)
  result <- .C("constr", noi, nob, t(x), nos, package="kstMatrix")
  nos <- result[[4]][1]
  space <- matrix(1:(noi*nos), ncol=noi, nrow=nos, byrow = TRUE)
  storage.mode(space) <- "integer"
  result2 <- .C("constr_results", space, package="kstMatrix")
  result2[[1]]
}

