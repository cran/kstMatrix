#' Convert a binary matrix to a \code{kmstructure} object
#'
#' \code{kmstructure()} returns a \code{kmstructure} object after checking that
#' the passed object is a binary matrix without double rows. The empty set
#' and the full item set are added if missing.
#'
#' @param x Binary matrix representing a family of sets
#' @return Distance distribution vector
#'
#' @examples
#' m <- as.matrix(c(1,0,0,0,1,0,1,1,1), nrow=3, byrow=TRUE)
#' kmstructure(m)
#'
#' @family Constructors
#'
#' @export
kmstructure <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("data"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("data")))
  }

  t <- kmminimalspace(dim(x)[2])
  r <- rbind(x,t)
  res <- kmfamset(r)
  class(res) <- unique(c("kmstructure", class(x)))
  res
}
