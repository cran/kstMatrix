#' Compute the "intersection" of two KST objects
#'
#' Both objects must be of the same kstMatrix class, the result will be
#' of the same class, i.e. a respective closure operator will be executed
#' automatically.
#'
#' @param x First KST object
#' @param y Second KST object
#' @returns _Intersection_ of \code{x} and \code{y}
#'
#' @useDynLib kstMatrix
#'
#' @family Functions for working with knowledge structures
#'
#' @export
kmintersection <- function(x, y) {
  UseMethod("kmintersection")
}
