#' Compute the surmise function for a knowledge space or basis
#'
#' \code{kmsurmisefunction} returns a data frame representing the
#' surmise function for a knowledge space or basis. The rows of the
#' data frame are ordered by item name.
#'
#' @param x Binary matrix representing a knowledge space or basis
#' @return Data frame  representing the surmise unction of \code{x}.
#'
#' @examples
#' kmsurmisefunction(xpl$space)
#'
#' @family Different representations for knowledge spaces
#'
#' @importFrom sets as.set set_is_subset set %e%
#' @importFrom pks as.pattern as.binmat
#'
#' @export
kmsurmisefunction <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  if (dim(x)[1] == 1) {
    return(NULL)
  }

  noi <- as.integer(dim(x)[2])
  nos <- as.integer(dim(x)[1])
  storage.mode(x) <- "integer"
  nob <- as.integer(0)

  result <- .C("basis_reduction", noi, nos, t(x), nob, package="kstMatrix")
  nob <- result[[4]][1]

  basis <- matrix(1:(noi*nob), ncol=noi, nrow=nob, byrow = TRUE)
  storage.mode(basis) <- "integer"
  mins <- matrix(1:(noi*nob), ncol=noi, nrow=nob, byrow = TRUE)
  storage.mode(mins) <- "integer"
  result2 <- .C("sf_results", basis, mins, package="kstMatrix")
  b <- matrix(result2[[1]], ncol=noi, nrow=nob, byrow=TRUE)
  colnames(b) <- colnames(x)
  m <- matrix(result2[[2]], ncol=noi, nrow=nob, byrow=TRUE)
  colnames(m) <- colnames(x)

  df <- data.frame(NULL)
  sapply(colnames(x), function(item) {
    hm <- matrix(b[m[,item]==1,], ncol=noi, byrow=FALSE)
    hr <- dim(hm)[1]
    hdf <- data.frame(cbind(t(t(rep(item, hr))), hm))
    df <<- rbind(df, hdf)
  })
  colnames(df) <- c("Item", colnames(x))
  df
}
