#' Check for wellgradedness of a knowledge structure
#'
#' \code{kmiswellgraded()} returns whether a knowledge space (or its basis)
#' is well-graded.
#'
#' @param x \code{kmspace} or \code{kmbasis}
#' @return Logical value specifying whether `x` is well-graded
#'
#' @references Doignon, J.-P. & Falmagne, J.-C. (1999). _Knowledge Spaces._
#' Springer–Verlag, Berlin.
#'
#' @examples
#' kmiswellgraded(xpl$space)
#'
#' @family Properties of knowledge structures
#' @family Functions for wellgradedness & learning paths
#'
#' @export
kmiswellgraded <- function(x) {
  if (!inherits(x, "kmspace") && !inherits(x, "kmbasis"))
    stop("'x' must be a 'kmspace' or 'kmbasis' object.")
  sf <- kmsurmisefunction(x)[,-1]
  usf <- unique(sf, MARGIN=1)
  if (dim(sf)[1] != dim(usf)[1]) return(FALSE)
  else return(TRUE)
}

