#' Obtain a quasi-ordinal knowledge space
#'
#' \code{kmqspace()} computes the  quasi-ordinal space
#' corresponding to a given KST object.
#'
#' @param x KST object
#' @return Corresponding \code{kmqspace} object
#'
#' @examples
#' m <- kmfamset(matrix(c(1,0,0,0,1,0,1,1,1), nrow=3, byrow=TRUE))
#' kmqspace(m)
#'
#' @family Constructors
#'
#' @export
kmqspace <- function(x) {
  UseMethod("kmqspace")
}
