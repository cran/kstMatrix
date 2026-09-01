#' Determine the knowledge space delineated by a surmise function
#'
#' @family Constructors
#' @family Different representations for knowledge structures
#' @method kmspace kmsurmisefunction
#' @rdname kmspace
#' @export
kmspace.kmsurmisefunction <- function(x)  {
  b <- kmbasis(x)
  kmunionclosure(b)
}
