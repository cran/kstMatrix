#' Close a family of sets to a quasi-ordinal knowledge space
#'
#' \code{kmqspace()} computes the smallest quasi-ordinal space
#' containing a given family of sets.
#'
#' @examples
#' m <- kmfamset(matrix(c(1,0,0,0,1,0,1,1,1), nrow=3, byrow=TRUE))
#' kmqspace(m)
#'
#' @family Constructors
#' @rdname kmqspace
#' @method kmqspace kmfamset
#'
#' @export
kmqspace.kmfamset <- function(x) {
  f <- kmfamset(x)
  sr <- kmsurmiserelation(f)
  b <- kmbasis(sr)
  sp <- kmspace(b)
  class(sp) <- unique(c("kmqspace", class(sp)))
  sp
}
