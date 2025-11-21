#' Determine a color vector based on probabilities
#'
#' \code{kmcolors} takes a probabilty vector and a color palette and
#' creates a color vector to be used with \code{kstMatrix::plot}.
#'
#' @param prob Probability vector
#' @param palette Color palette (default = cm.colors)
#'
#' @family Plotting knowledge structures
#' @family Utilities
#'
#' @importFrom grDevices cm.colors
#'
#' @export
kmcolors <- function(prob, palette=cm.colors) {
  palette(1001)[1001-1000*prob]
}
