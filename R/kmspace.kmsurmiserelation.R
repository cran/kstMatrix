#' Determine the knowledge space delineated by a surmise relation
#'
#' Please note that a knowledge space delineated by a surmise relation is
#' quasi-ordinal. Therefore, the result is automatically marked as
#' \code{kmqspace} object.
#'
#' @family Constructors
#' @family Different representations for knowledge structures
#' @method kmspace kmsurmiserelation
#' @rdname kmspace
#' @export
kmspace.kmsurmiserelation <- function(x) {
  b <- kmbasis(x)
  sp <- kmunionclosure(b)
  class(sp) <- unique(c("kmqspace", class(sp)))
  sp
}
