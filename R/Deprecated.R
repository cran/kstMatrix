#' DEPRECATED: \code{kmSR2basis()} has been replaced by \code{kmbasis()}.
#' @param x Surmise relation
#' @return Corresponding basis
#'
#' @family Deprecated functions and methods
#' @export
kmSR2basis <- function(x) {
  .Deprecated("kmbasis")
  kmbasis(x)
}

#' DEPRECATED: \code{kmSF2basis()} has been replaced by \code{kmbasis()}.
#' @param x Surmise function
#' @return Corresponding basis
#'
#' @family Deprecated functions and methods
#' @export
kmSF2basis <- function(x) {
  .Deprecated("kmbasis")
  kmbasis(x)
}

#' DEPRECATED: \code{kmhasse()} has been replaced by a \code{plot()} method.
#' @param x Knowledge structure
#' @param horizontal Boolean whether the diagram should be drawn horizontally or vertically (default TRUE)
#' @param colors Color vector (default NULL)
#'
#' @family Deprecated functions and methods
#' @export
kmhasse <- function(x, horizontal=TRUE, colors=NULL) {
  .Deprecated("plot")
  if (horizontal)
    plot(x, horizontal=horizontal, colors=colors, arrowtail="normal")
  else
    plot(x, horizontal=horizontal, colors=colors)
}

#' DEPRECATED: \code{kmbasisdiagram()} has been replaced by a \code{plot()} method.
#' @param x Basis
#' @param horizontal Boolean whether the diagram should be drawn horizontally or vertically (default TRUE)
#' @param colors Color vector (default NULL)
#'
#' @family Deprecated functions and methods
#' @export
kmbasisdiagram <- function(x, horizontal=TRUE, colors=NULL) {
  .Deprecated("plot")
  if (horizontal)
    plot(x, horizontal=horizontal, colors=colors, arrowtail="normal")
  else
    plot(x, horizontal=horizontal, colors=colors)
}

#' DEPRECATED: \code{kmSRdiagram()} has been replaced by a \code{plot()} method.
#' @param x Surmise relation
#' @param horizontal Boolean whether the diagram should be drawn horizontally or vertically (default FALSE)
#' @param colors Color vector (default NULL)
#'
#' @family Deprecated functions and methods
#' @export
kmSRdiagram <- function(x, horizontal=FALSE, colors=NULL) {
  .Deprecated("plot")
  if (horizontal)
    plot(x, horizontal=horizontal, colors=colors, arrowtail="normal")
  else
    plot(x, horizontal=horizontal, colors=colors)
}
