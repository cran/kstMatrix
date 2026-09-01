#' \code{kmspace} constructor method
#'
#' \code{kmspace()} determines the knowledge space corresponding to a KST object
#'
#' @param x KST object
#' @return Corresponding knowledge space
#'
#' @examples
#' kmspace(xpl$basis)
#' kmspace(xpl$sf)
#'
#' @family Constructors
#' @export
kmspace <- function(x) {
  UseMethod("kmspace")
}
