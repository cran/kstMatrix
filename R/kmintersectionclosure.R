#' Close a knowledge space under intersection
#'
#' @param x Knowledge space
#' @returns Quasi--ordinal knowledge space
#'
#' @examples
#' kmintersectionclosure(xpl$space)
#'
#' @family Closure operators
#'
#' @export
kmintersectionclosure <- function(x) {
  if (!(inherits(x, "kmspace")))
    stop("'x' must be a 'kmspace' object.")
  kmqspace(x)
}
