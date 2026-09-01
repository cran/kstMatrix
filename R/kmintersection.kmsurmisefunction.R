#' Determine the "intersection" of two surmise functions.
#'
#' @examples
#' sf <- kmsurmisefunction(kmbasis(xpl$sr))
#' kmintersection(xpl$sf, sf)
#'
#' @family Functions for working with knowledge structures
#' @method kmintersection kmsurmisefunction
#' @rdname kmintersection
#' @export
kmintersection.kmsurmisefunction <- function(x,y) {
  if (!(inherits(y, "kmsurmisefunction")))
    stop("'y' must also be a surmise function.")
  basx <- kmbasis(x)
  basy <- kmbasis(y)
  basu <- kmunion(basx, basy)
  kmsurmisefunction(basu)
}
