#' Compute the fringe of a state within a knowledge structure using its basis
#'
#' \code{kmbasisfringe} computes the fringe of a state
#' within a knowledge structure, i.e. the set of items by which
#' the state differs from its neighbours.
#'
#' @param state Binary vector representing a knowledge state
#' @param basis \code{kmbasis} object
#' @return Binary vector representing the fringe
#'
#' @references Hockemeyer C (1997). Using the Basis of a Knowledge Space for
#' Determining the Fringe of a Knowledge State.
#' _Journal of Mathematical Psychology, 41,_ 275–279.
#'
#' @examples
#' kmbasisfringe(c(1,0,0,0), xpl$basis)
#'
#' @family Fringes & learning paths
#'
#' @export
kmbasisfringe <- function(state, basis) {
  as.integer(kmbasisinnerfringe(state, basis) |
               kmbasisouterfringe(state, basis))
}


#' @rdname kmbasisfringe
#' @export
kmbasisinnerfringe <- function(state, basis) {
  noi <- length(state)
  nob <- dim(basis)[1]
  if (!inherits(basis, "kmbasis"))
    stop("Basis must be a kmbasis object")
  if (noi != dim(basis)[2])
    stop("State and basis do not match in size")

  cs <- rep(0, noi)
  inner <- cs

  apply(basis, MARGIN=1, function(x) {
    if (all(as.integer(x & state) == x))
      cs <<- as.integer(cs | x)
  })
  if (any(cs != state))
    stop("The state is no element of the space represented by the basis.")

  bkvec <- c()
  sapply(1:nob, function(b) {
    if (all(basis[b,] <= state))
      bkvec <<- c(bkvec, b)
  })
  bk <- matrix(basis[bkvec,], ncol=noi, byrow=TRUE)
  print(bk)

  inner <- rep(0, noi)
  sapply(1:noi, function(it) {
    if (state[it] == 1) {
      bknqlist <- which(bk[,it] == 0)
      bknq <- matrix(bk[bknqlist,], nrow=length(bknqlist), ncol=noi, byrow=FALSE)
      cand <- rep(0, noi)
      apply(bknq, MARGIN=1, function(b) {
        cand <<- as.integer(cand | b)
      })
      if (sum(state - cand) == 1)
        inner[it] <<- 1
    }
  })

  names(inner) <- colnames(basis)
  inner
}


#' @rdname kmbasisfringe
#' @export
kmbasisouterfringe <- function(state, basis) {
  noi <- length(state)
  if (!inherits(basis, "kmbasis"))
    stop("Basis must be a kmbasis object")
  if (noi != dim(basis)[2])
    stop("State and basis do not match in size")

  cs <- rep(0, noi)
  of <- cs
  apply(basis, MARGIN=1, function(x) {
    if (all(as.integer(x & state) == x))
      cs <<- as.integer(cs | x)
    else if (sum(as.integer(x & !state)) == 1)
      of <<- as.integer(of | x)
  })
  if (any(cs != state))
    stop("The state is no element of the space represented by the basis.")
  of <- as.integer(of & !state)
  names(of) <- colnames(basis)
  of
}
