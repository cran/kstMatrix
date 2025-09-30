#' Generate a knowledge structure from a set of response patterns
#'
#' \code{kmgenerate} returns a matrix representing a knowledge structure generated
#' from data. It uses a simplistic approach: patterns with a frequency above a
#' specified threshold are considered as knowledge states.
#' If the specified threshold is 0 (default) a real threshold is computed as
#' N (number of  response patterns) divided by 2^|Q|.
#' Please note that the number of response patterns should be much higher than the
#' size of the power set of the item set Q. A factor of art least 10 is recommended.
#' Currently, the number of items is limited to the number of bits in a C long
#' minus one (i.e. 31 under Windows and 63 otherwise). But we would probably run into
#' memory problems way earlier anyway.
#'
#' @param x Binary matrix representing a data set
#' @param threshold Threshold for taking response patterns as knowledge states (default 0)
#' @return Binary matrix representing the generated knowledge structure
#'
#'
#' @examples
#' kmgenerate(xpl$sim, 15)
#'
#' @useDynLib kstMatrix
#'
#'
#' @family Generating knolwedge spaces
#'
#' @export
kmgenerate <- function(x, threshold = 0) {

  gen_errmsg <- c(
    "Too many items! The number of items is generally limited to 64, on Windows systems it is probalby limited to 32.",
    "Not enough memory!"
  )
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  noi <- as.integer(dim(x)[2])
  nod <- as.integer(dim(x)[1])
  if (threshold == 0) {
    threshold <- ceiling(nod / (2^noi));
  }
  storage.mode(x) <- "integer"
  nos <- as.integer(0)
  rc <- as.integer(0)
  result <- .C("generate", noi, nod, t(x), as.integer(threshold), rc, nos, package="kstMatrix")
  rc <- result[[5]][1]
  if (rc != 0) {
    stop(gen_errmsg[rc])
  }
  nos <- result[[6]][1]
  kstruct <- matrix(1:(noi*nos), ncol=noi, nrow=nos, byrow = TRUE)
  storage.mode(kstruct) <- "integer"
  result2 <- .C("generate_results", kstruct, package="kstMatrix")
  s <- result2[[1]]
  colnames(s) <- NULL
  s
}
