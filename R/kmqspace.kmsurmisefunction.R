#' Determine the quasi-ordinal knowledge space containing the knowledge space delineated by a surmise function
#'
#' \code{kmqspace.kmsurmisefunction()} takes a surmise function and
#' determines the quasi-ordinal knowledge space which is the closure
#' under intersection of the knowledge space delineated by this surmise
#' function.
#'
#' @family Constructors
#' @rdname kmqspace
#' @method kmqspace kmsurmisefunction
#' @export
kmqspace.kmsurmisefunction <- function(x) {
  b <- kmbasis(x)
  sr <- kmsurmiserelation(b)
  b <- kmbasis(sr)
  s <- kmunionclosure(b)
  class(s) <- unique(c("kmqspace", class(s)))
  s
}
