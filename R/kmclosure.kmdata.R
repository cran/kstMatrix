#' @method kmclosure kmdata
#' @family Closure operators
#' @rdname kmclosure
#' @export
kmclosure.kmdata <- function(x, ...) {

  x <- kmbasis(kmfamset(x))

  kmunionclosure(x)
}

