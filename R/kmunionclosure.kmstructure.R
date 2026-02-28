#' @rdname kmunionclosure
#' @export
kmunionclosure.kmstructure <- function(x) {
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  x <- kmbasis(x)

  noi <- as.integer(dim(x)[2])
  nob <- as.integer(dim(x)[1])
  storage.mode(x) <- "integer"
  nos <- as.integer(0)
  result <- .C("constr", noi, nob, t(x), nos, package="kstMatrix")
  nos <- result[[4]][1]
  space <- matrix(1:(noi*nos), ncol=noi, nrow=nos, byrow = TRUE)
  storage.mode(space) <- "integer"
  result2 <- .C("constr_results", space, package="kstMatrix")
  s <- result2[[1]]
  colnames(s) <- colnames(x)
  class(s) <- unique(c("kmspace", "kmstructure", "kmfamset", class(s)))
  s
}

