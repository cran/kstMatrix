#' Determine the closure of KST objects
#'
#' \code{kmclosure()} is an S3 method closing \code{kstMatrix} objects.
#'
#' @param x KST object
#' @param ... Optional additional parameters
#' @return Corresponding closed KST object
#'
#' An attribution relation or
#' attribution function is closed to the corresponding surmise relation
#' or surmise function, respectively.
#'
#' A family of sets (including bases, knowledge structure, and knowledge space)
#' is closed to a knowledge space or quasi-ordinal knowledge space.
#'
#' @family Closure operators
#'
#' @export
kmclosure <- function(x, ...) {
  UseMethod("kmclosure")
}
