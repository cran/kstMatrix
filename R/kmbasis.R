#' Compute the basis of a knowledge space
#'
#' @param x KST object
#' @return Binary matrix representing the basis of the corresponding knowledge space.
#'
#' @family Different representations for knowledge structures
#'
#' @export
kmbasis <- function(x) {
  UseMethod("kmbasis")
}
