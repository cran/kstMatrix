#' @method kmclosure kmfamset
#' @param closure Type of closure
#'
#' For families of sets (\code{kmfamset}), kmclosure is basically a wrapper
#' for \code{kmunionclosure()} and \code{kmintersectionclosure}.
#' \code{closure()} may take the values "union" (default) or "intersection".
#' The former requests a closure under union, the latter a closure under
#' union **and** intersection.
#'
#' @rdname kmclosure
#'
#' @family Closure operators
#' @export
kmclosure.kmfamset <- function(x, ..., closure="union") {
  if (closure == "union")
    kmunionclosure(x)
  else if (closure == "intersection")
    kmqspace(x)
  else stop("Unknown type of closure.")
}
