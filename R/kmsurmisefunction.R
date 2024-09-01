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
#' @keywords math
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

  # First determine the basis of x
  mat <- kmbasis(x)
  x <- as.pattern(mat, as.set = TRUE)
  class(x) <- unique(c("kbase", class(x)))
  mat <- as.binmat(x)

  rownames(mat) <- NULL
  # colnames(mat) <- NULL
  mat <- 2*mat

  dom <- as.set(unique(unlist(as.list(x))))
  ### compute atoms
  y <- as.list(x)
  atoms <- list()
  items <- as.set(lapply(dom, as.character))
  for (i in items) {
    states <- y[which(sapply(y, function(j) grep(i,j))!=0)]
    atom <- set()
    for (j in seq_along(states)) {
      subsets <- lapply(states[-j],set_is_subset, states[[j]])
      if (!any(unlist(subsets))) {
        atom <- c(atom, set(as.set(states[[j]])))
      }
    }
    atoms[[i]] <- atom
  }
  names(atoms) <- unlist(items)
  sind <- 1
  for (s in x) {
    qind <- 1
    for (q in dom) {
      if (s %e% atoms[[qind]])
        mat[sind,qind] <- 1
      qind <- qind + 1
    }
    sind <- sind + 1
  }
  itemnames <- colnames(mat)

  sf <- as.data.frame(t(as.data.frame(apply(mat, MARGIN=1, function(x) {
    items <- which(x == 1)
    prereqs <- which(x == 2)
    x[prereqs] <- 1
    y <- t(rbind(
      as.data.frame(matrix(names(items), nrow=1)),
      as.data.frame(matrix(rep(x, length(items)), ncol = length(items), byrow = FALSE))
    ))
    # print("printing y")
    # print(y)
    t(y)
  }))))
  # print("printing sf")
  # print(sf)
  noc <- dim(sf)[2]
  sf[,2:(noc)] <- sapply(sf[,2:(noc)], as.numeric)
  colnames(sf)[1] <- "Item"
  colnames(sf)[2:noc] <- itemnames
  rownames(sf) <- NULL
  sf[order(sf$Item),]

}
