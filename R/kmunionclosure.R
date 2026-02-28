#' Close a family of sets under union
#'
#' \code{kmunionclosure} returns a matrix representing a knowledge space. Please note
#' that it may take quite some time for computing larger knowledge spaces.
#'
#' @param x Binary matrix representing a family of sets
#' @return Binary matrix representing the corresponding knowledge space, i.e. the
#'         closure of the family under union including the empty set and
#'         the full set.
#'
#' \code{kmunionclosure} implements the irredundant algorithm developed by Dowling (1993).
#'
#' @references Dowling, C. E. (1993). On the irredundant construction of knowledge spaces.
#' _Journal of Mathematical Psychology, 37,_ 49–62.
#'
#' @examples
#' kmunionclosure(xpl$basis)
#'
#' @useDynLib kstMatrix
#'
#'
#' @family Different representations for knowledge spaces
#'
#' @rdname kmunionclosure
#' @export
kmunionclosure <- function(x) {
  UseMethod("kmunionclosure")
}
