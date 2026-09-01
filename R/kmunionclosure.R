#' Close a family of sets under union, i.e. determine the smallest knowledge space containing it
#'
#' @param x Family of set
#' @returns Knowledge space
#'
#' \code{kmunionclosure} implements the irredundant algorithm developed by Dowling (1993).
#'
#' @references Dowling, C. E. (1993). On the irredundant construction of knowledge spaces.
#' _Journal of Mathematical Psychology, 37,_ 49–62.
#'
#' @examples
#' kmunionclosure(xpl$basis)
#'
#'
#' @family Closure operators
#' @export
kmunionclosure <- function(x) {
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

