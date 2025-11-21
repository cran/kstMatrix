#' Convert a binary matrix to a \code{kmspace} object
#'
#' \code{kmspace()} returns a \code{kmspace} object for a binary matrix.
#'
#' @param x Binary matrix representing a family of sets
#' @return \code{kmspace} object
#'
#' @examples
#' m <- as.matrix(c(1,0,0,0,1,0,1,1,1), nrow=3, byrow=TRUE)
#' kmspace(m)
#'
#' @family Constructors
#'
#' @export
kmspace <- function(x) {
  kmunionclosure(kmstructure(x))
}
