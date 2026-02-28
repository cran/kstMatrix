#' Compute the basis of a knowledge space
#'
#' \code{kmbasis.matrix} returns a matrix representing the basis of a
#' knowledge space. If \code{x} is a knowledge structure or an
#' arbitrary family of sets \code{kmbasis} returns the basis of
#' the smallest knowledge space containing \code{x}.
#'
#' @param x Binary matrix representing a knowledge space
#' @return Binary matrix representing the basis of the knowledge space.
#'
#' @examples
#' kmbasis(xpl$space)
#'
#' @family Different representations for knowledge spaces
#'
#' @export
kmbasis <- function(x) {
  UseMethod("kmbasis")
}
