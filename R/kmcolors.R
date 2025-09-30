#' Determine a color vector based on probabilities
#'
#' \code{kmcolors} takes a probabilty vector and a color palette and
#' creates a color vector to be used with \code{kmhasse}.
#'
#' @param prob Probability vector
#' @param palette Color palette (default = cm.colors)
#'
#' @family Plotting knowledge structures
#'
#' @importFrom grDevices cm.colors
#'
#' @export
kmcolors <- function(prob, palette=cm.colors) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(sprintf("Plotting requires package 'igraph'."))
  }
  palette(1001)[1001-1000*prob]
}
