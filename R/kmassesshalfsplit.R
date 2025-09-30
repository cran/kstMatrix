#' Determine next question for probabilistic knowledge assessment
#'
#' \code{kmassesshalfsplit} determines the next question in a probabilistic
#' assessment according to the halfsplit rule.
#'
#' @param probs Probability distribution over the knowledge structure (vector)
#' @param ks Binary matrix of the knowledge structure
#' @return Number of the selected question
#'
#' @examples
#' kmassesshalfsplit(c(0.02, 0.1, 0.07, 0.01, 0.4, 0.17, 0.07, 0.08, 0.08),
#'                   xpl$space)
#'
#' @family Knowledge assessment
#'
#' @export
kmassesshalfsplit <- function(probs, ks) {
  debug <- FALSE
  set.seed(NULL)

  if (debug) print(sprintf("Prob dist: [%s]", paste(probs, collapse = ", ")))
  storage.mode(ks) <- "integer"
  noi <- dim(ks)[2]

  if ((min(ks) < 0) | (max(ks) > 1))
    stop("Knowledge structure must be a binary matrix.")
  if ((min(probs) < 0) | (max(probs) > 1))
    stop("State probabilities must be between 0 and 1.")
  if (!(kmdoubleequal(sum(probs), 1.0)))
    stop("State probabilities must sum up to 1.")

  ip <- sapply(1:noi, function(x) {
    sum(probs[which(ks[,x]==1)])
  })
  hd <- abs(ip - 0.5)
  if (debug) print(hd)
  hs <- which(hd == min(hd))
  if (debug) print(sprintf("Possible questions: %s", paste(hs, collapse=", ")))
  if (length(hs)> 1)
    hs <- sample(hs, 1)
  if (debug) print(sprintf("Question selected: %d", hs))
  hs
}
