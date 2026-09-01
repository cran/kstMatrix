#' @rdname kmunion
#'
#' @method kmunion kmsurmisefunction
#' @family Functions for working with knowledge structures
#' @export
kmunion.kmsurmisefunction <- function(x, y) {
  if (!(inherits(y, "kmsurmisefunction")))
    stop("'y' must be a kmbasis object.")
  noix <- dim(x)[2] - 1
  noiy <- dim(y)[2] - 1
  if (noix != noiy)
    stop("Objects do not fit in number of items.")
  if (is.null(colnames(x))) {
    if (!(is.null(colnames(y))))
      stop("Item names do not fit.")
  } else if (is.null(colnames(y)))
    stop("Item names do not fit.")
  else {
    cnx <- colnames(x)[-1]
    cny <- colnames(y)[-1]
    if (any(cnx != cny))
      stop("Item names do not fit")
  }

  res <- data.frame("ignore", t(rep(0,noix)))
  colnames(res) <- colnames(x)
  apply(x, MARGIN=1, function(c) {
    item <- c[1]
    clause <- as.numeric(c[-1])
    clist = which(y[,1] == item)
    apply(y[clist,], MARGIN=1, function(d) {
      u <- data.frame(item, t(as.numeric(clause | as.numeric(d[-1]))))
      colnames(u) <- colnames(res)
      rownames(u) <- NULL
      res <<- rbind(res, u)
    })
  })
  res <- res[-1,]
  class(res) <- unique(c("kmattributionfunction", class(res)))
  kmclosure(res)
}
