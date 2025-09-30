#' Compute the surmise relation of a quasi-ordinal knowledge space
#'
#' \code{kmsurmiserelation} returns a matrix representing the
#' surmise relation of a quasi-ordinal knowledge space. If \code{x}
#' is a general knowledge space, a knowledge structure or an
#' arbitrary family of sets, \code{kmsurmiserelation} returns the
#' surmise relation of the smallest quasi-ordinal knowledge space
#' containing \code{x}.
#'
#' @param x Binary matrix representing a quasi-ordinal knowledge space
#' @return Binary matrix representing the surmise relation of the
#' corresponding quasi-ordinal knowledge space
#'
#' Note: The columns of the surmise relation matrix describe the
#' minimal state for the respective item in the quasi-ordinal
#' knowledge space.
#'
#' @examples
#' kmsurmiserelation(xpl$space)
#'
#' @family Different representations for knowledge spaces
#'
#' @export
kmsurmiserelation <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1L*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  noi <- dim(x)[2]
  sr <- matrix(rep(0, noi*noi), nrow = noi, ncol = noi)
  lapply(as.list(1:dim(x)[2]), function(i) {
    lapply(as.list(1:dim(x)[2]), function(j) {
      if (all(x[,i] <= x[,j])) {
        sr[j,i] <<- 1
      }
    })
  })
  # for (i in 1:noi) {
  #   for (j in 1:noi) {
  #     if (all(x[,i] <= x[,j])) {
  #       sr[j,i] <- 1
  #     }
  #   }
  # }
  storage.mode(sr) <- "integer"
  rownames(sr) <- colnames(x)
  colnames(sr) <- colnames(x)
  sr
}
