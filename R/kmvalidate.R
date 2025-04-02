#' Validate a knowledge structure against a data set
#'
#' \code{kmvalidate} returns a list with three elements,
#' a named vector (dist) with the frequencies of distances
#' between a set of response patterns and a knowledge structure, the
#'  Discrepancy Index (DI), and the Distance Agreement Coefficient (DA).
#'
#' @param data Binary matrix representing a set of response patterns
#' @param struct Binary matrix representing a knowledge structure
#' @return A list with three elements:
#' \describe{
#'   \item{dist}{Distance distribution vector}
#'   \item{DI}{Discrepancy Index}
#'   \item{DA}{Distance Agreement Coefficient}
#' }
#'
#' @section Warning:
#' The DA computation can take quite some time for larger item sets as the
#' power set has to be computed. For item sets with around 30 items or more,
#' it may even crash the system due to huge memory requests.
#'
#' @examples
#' kmvalidate(xpl$data, xpl$space)
#'
#' @keywords math
#' @family Validating knowledge spaces
#'
#' @export
kmvalidate <- function(data, struct) {
  if (dim(data)[2] != dim(struct)[2])
    stop("data and struct do not match in item number!")
  distvector <- kmdist(data, struct)
  ddat <- sum(as.integer(names(distvector)) * distvector) / sum(distvector)
  di <- kmdist(kmmaximalspace(dim(struct)[2]), struct)
  dpot <- sum(as.integer(names(di))*di)/sum(di)
  list("dist"=distvector, "DI"=ddat, "DA"=ddat/dpot)
}
