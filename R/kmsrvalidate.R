concordance <- function(data, sr) {
  colnames(data) <- colnames(sr)
  sum(sapply(1:dim(sr)[1], function(x) {
    sum(sapply(1:dim(sr)[1], function(y) {
      if (data[x,y] == 1) {
        ind <- which(data[,x]==1 & data[,y]==0)
        length(ind)
      } else {
        0
      }
    }))
  }))
}

discordance <- function(data, sr) {
  colnames(data) <- colnames(sr)
  sum(sapply(1:dim(sr)[1], function(x) {
    sum(sapply(1:dim(sr)[1], function(y) {
      if (data[x,y] == 1) {
        ind <- which(data[,x]==0 & data[,y]==1)
        length(ind)
      } else {
        0
      }
    }))
  }))
}


#' Validate a surmise relation against a data set
#'
#' \code{kmSRvalidate} returns a list with two elements,
#' Goodman & Kruskal's gamma value and the violational
#' coefficient (VC).
#'
#' @param data Binary matrix representing a set of response patterns
#' @param sr Binary matrix representing a surmise relation
#' @return A list with two elements:
#' \describe{
#'   \item{gamma}{Goodman & Kruskal's gamma index}
#'   \item{VC}{Violational Coefficient}
#' }
#'
#' @examples
#' kmSRvalidate(xpl$data, xpl$sr)
#'
#' @family Validating knowledge spaces
#'
#' @export

kmSRvalidate <- function(data, sr) {
  nc <- concordance(data, sr)
  nd <- discordance(data, sr)
  gamma <- (nc - nd) / (nc + nd)

  vc = nd / (dim(data)[1] * (sum(sr) - dim(data)[2]))

  list(gamma=gamma, vc=vc)
}
