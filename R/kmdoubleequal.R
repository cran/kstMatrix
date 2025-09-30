#' Test two double numbers on equity with a certain tolerance
#'
#' @param x First double to compare
#' @param y Second double to compare
#' @param tol Tolerance optional)
#' @return Boolean for (approximate) equity
#'
#' @examples
#' kmdoubleequal(0.5+0.5, 1)
#'
#' @family Utilities
#'
#' @export
kmdoubleequal <- function(x, y, tol = sqrt(.Machine$double.eps)){
  abs(x - y) < tol
}
