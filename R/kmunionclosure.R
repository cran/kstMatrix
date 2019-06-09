#' Close a family of sets under union
#'
#' \code{kmunionclosure} returns a matrix representing a knowledge space. Please note
#' that it takes quite some time for computing larger knowledge spaces.
#'
#' @param x Binary matrix representing a family of sets
#' @return Binary matrix representing the corresponding knowledge space, i.e. the
#'         closure of the family under union including the empty set and
#'         the full set.
#'
#' \code{kmconstrDowling} implements the irredundant algorithm deveoped by Dowling (1993).
#'
#' @references Dowling, C. E. (1993). On the irredundant construction of knowledge spaces.
#' Journal of Mathematical Psychology, 37, 49–62.
#'
#' @examples
#' kmunionclosure(xpl$basis)
#'
#' @keywords math
#'
#' @export
kmunionclosure <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  noi <- dim(x)[2]
  nob <- dim(x)[1]
  f <- matrix(rep(0L, noi), nrow = 1, ncol = noi)

  lapply(as.list(1:nob), function(i) {
    f2 <- t(apply(f, 1, function(s) {1L*(s | x[i,])}))
    f <<- unique(rbind(f, f2))
  })
  storage.mode(f) <- "integer"
  f
}

