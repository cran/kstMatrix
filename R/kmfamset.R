#' Convert a binary matrix to a \code{kmfamset} object (family of sets)
#'
#' \code{kmfamset} returns a \code{kmfamset} object after checking that
#' the passed object is a binary matrix with all different rows. If the
#' passe object inherits the \code{kmfamset} property, nothing else is
#' changed.
#'
#' @param x Binary matrix representing a family of sets
#' @return Distance distribution vector
#'
#' @examples
#' m <- as.matrix(c(1,0,0,0,1,0,1,1,1), nrow=3, byrow=TRUE)
#' kmfamset(m)
#'
#' @family Constructors
#'
#' @export
kmfamset <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("data"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("data")))
  }
  res <- unique(x, MARGIN = 1)
  if (!inherits(x, "kmfamset")) {
    class(res) <- c('kmfamset', class(x))
  } else {
    class(res) <- class(x)
  }
  x
}
