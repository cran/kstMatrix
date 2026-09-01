#' @method kmspace kmdata
#' @family Constructors
#' @rdname kmspace
#' @export
kmspace.kmdata <- function(x) {

  x <- kmbasis(kmfamset(x))

  kmunionclosure(x)
}

