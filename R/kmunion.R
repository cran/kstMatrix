#' Compute the "union" of two KST objects
#'
#' Both objects must be of the same kstMatrix class, the result will be
#' of the same class, i.e. a respective closure operator will be executed
#' automatically.
#'
#' For surmise functions, the algorithm by Dowling & Hockemeyer (1998) is used.
#'
#' Please note that, for families of sets, there are the functions
#' \code{kmunionclosure()} and \code{kmintersectionclosure()}.
#'
#' @references Dowling CE, Hockemeyer C (1998). “Computing the Intersection
#'    of Knowledge Spaces Using only their Basis.” In Dowling CE, Roberts FS,
#'    Theuns P (eds.), _Recent Progress in Mathematical Psychology,_ 133–141.
#'    Lawrence Erlbaum Associates Ltd., Mahwah, NJ.
#'
#' @param x First object
#' @param y Second object
#'
#' @return _Union_ of \code{x} and \code{y}
#'
#' @useDynLib kstMatrix
#'
#' @family Functions for working with knowledge structures
#'
#' @export
kmunion <- function(x, y) {
  UseMethod("kmunion")
}

