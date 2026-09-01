#' Reduce a KST object to a subset of items
#'
#' @param x KST object
#' @param items Selection of items (vector) to which the KST object shall be reduced
#' @returns Sub structure KST object (the kstMatrix class and its properties
#'          are preserved)
#'
#' \code{kmsubstructure()} takes a KST object and determines a sub-structure
#' based on the list of items.
#'
#' The items can be specified through their item names or through the respective
#' column numbers. At least two items must be specified.
#'
#' @examples
#' phsg$basis
#' kmsubstructure(phsg$basis, c("I1", "I2", "I5"))
#'
#' @family Functions for working with knowledge structures
#'
#' @export
kmsubstructure <- function(x, items) {
  UseMethod("kmsubstructure")
}


#' @rdname kmsubstructure
#' @method kmsubstructure kmdata
#' @export
kmsubstructure.kmdata <- function(x, items) {
  if (length(items) < 2)
    stop("The vector of items must contain at least two elements.")
  m <- x[, items]
  class(m) <- unique(c("kmdata", class(m)))
  m
}

#' @rdname kmsubstructure
#' @method kmsubstructure kmbasis
#' @export
kmsubstructure.kmbasis <- function(x, items) {
  if (length(items) < 2)
    stop("The vector of items must contain at least two elements.")
  m <- x[, items]
  class(m) <- unique(c("kmfamset", class(m)))
  kmbasis(m)
}

#' @rdname kmsubstructure
#' @method kmsubstructure kmstructure
#' @export
kmsubstructure.kmstructure <- function(x, items) {
  if (length(items) < 2)
    stop("The vector of items must contain at least two elements.")
  m <- unique(x[, items], MARGIN=1)
  class(m) <- unique(c("kmstructure", "kmfamset", class(m)))
  m
}

#' @rdname kmsubstructure
#' @method kmsubstructure kmspace
#' @export
kmsubstructure.kmspace <- function(x, items) {
  if (length(items) < 2)
    stop("The vector of items must contain at least two elements.")
  m <- unique(x[, items], MARGIN=1)
  class(m) <- unique(c("kmspace", "kmstructure", "kmfamset", class(m)))
  m
}

#' @rdname kmsubstructure
#' @method kmsubstructure kmsurmiserelation
#' @export
kmsubstructure.kmsurmiserelation <- function(x, items) {
  if (length(items) < 2)
    stop("The vector of items must contain at least two elements.")
  m <- x[, items]
  m <- m[items,]
  class(m) <- unique(c("kmsurmiserelation", "kmattributionrelation", class(m)))
  m
}

#' @rdname kmsubstructure
#' @method kmsubstructure kmsurmisefunction
#' @export
kmsubstructure.kmsurmisefunction <- function(x, items) {
  if (length(items) < 2)
    stop("The vector of items must contain at least two elements.")
  if (is.numeric(items))
    items = colnames(x)[c(1, items+1)]
  else {
    items <- c(colnames(x)[1], items)
  }
  df <- unique(x[, items], MARGIN=1)
  l <- which(df[,1] %in% items)
  df <- df[l,]
  class(df) <- unique(c("kmattributionfunction", class(df)))
  # kmclosure(df)
  df
}
