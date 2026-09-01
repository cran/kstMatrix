#' Determine the quasi-ordinal knowledge space for a surmise relation
#'
#' \code{kmqspace.kmsurmiserelation()} computes the quasi-ordinal
#' knowledge space delineated by a surmise relation
#'
#' @examples
#' sr <- kmsurmiserelation(xpl$space)
#' kmqspace(sr)
#'
#' @family Constructors
#' @rdname kmqspace
#' @method kmqspace kmsurmiserelation
#'
#' @export
kmqspace.kmsurmiserelation <- function(x) {
  b <- kmbasis(x)
  s <- kmunionclosure(b)
  class(s) <- unique(c("kmqspace", class(s)))
  s
}
