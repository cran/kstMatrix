#' Generic \code{kmbasis()} function
#'
#' @param x Family of sets or surmise relation in matrix representation
#' @param ... Optional additional parameters
#' @return Basis for a knowledge space
#'
#' @export
kmbasis <- function(x, ...) {
  UseMethod("kmbasis")
}
