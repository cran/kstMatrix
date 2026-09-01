#' Determine the smallest knowledge space containing a family of sets/states.
#'
#' @rdname kmspace
#' @family Constructors
#' @method kmspace kmfamset
#' @export
kmspace.kmfamset <- function(x) {
  kmunionclosure(x)
}
