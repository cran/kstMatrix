#' Simulate assessments for a set of response patterns
#'
#' \code{kmassessmentsimulation} does a probabilistic knowledge assessment
#' for each response pattern in a data matrix and stores information about
#' the assessment.
#'
#' @param respdata Data matrix
#' @param ks Knowledge structure
#' @param questioning Question rule
#' @param update Updating rule
#' @param beta Careless error probability
#' @param eta Lucky guess probability
#' @param zeta0 Update parameter for wrong responses
#' @param zeta1 Update parameter for correct responses
#' @param threshold Stopping criterion
#' @return Assessment data as data frame
#'
#' @details
#' \code{kmassessmentsimulation} applies the \code{kmsassess} function.
#'
#' @examples
#' kmassessmentsimulation(
#'   xpl$data,
#'   xpl$space,
#'   "halfsplit",
#'   "multiplicative",
#'   NULL,
#'   NULL,
#'   5,
#'   5,
#'   0.55
#' )
#'
#' @family Knowledge assessment
#'
#' @importFrom tidyr unnest
#'
#' @export
kmassessmentsimulation <- function(respdata,
                                   ks,
                                   questioning,
                                   update,
                                   beta,
                                   eta,
                                   zeta0,
                                   zeta1,
                                   threshold) {
  columnnames <- c(
    "No items",
    "No States",
    "Questioning",
    "Update",
    "Beta",
    "Eta",
    "Zeta_0",
    "Zeta_1",
    "Threshold",
    "Pattern no.",
    "Assessment error",
    "Distance",
    "Net assessm. error",
    "Qestions asked",
    "Computation time",
    "Avg. questioning time",
    "Avg. updating time",
    "State",
    "Pattern"
  )
  cnt <- 0
  ars <- apply(respdata, 1, function(rp) {
    cnt <<- cnt+1
    # print(cnt)
    # print(rp)
    dist <- min(unlist(apply(ks, 1, function(state) {kmsetdistance(rp, state)})))
    t <- system.time(ar <- kmsassess(rp, ks, questioning, update, beta, eta, zeta0, zeta1, threshold))
    if (!is.null(ar)) {
      list(dim(ks)[2], dim(ks)[1], questioning, update, beta, eta, zeta0, zeta1, threshold, cnt,
           kmsetdistance(rp, ar$state), dist, kmsetdistance(rp, ar$state) - dist, length(ar$queried), t[[3]],
           ar$qtime, ar$utime, paste(ar$state, collapse = ", "), paste(rp, collapse = ", ")
      )
    } else {
      warning(sprintf("NULL assessment result for pattern # %s.", cnt))
      NULL
    }
  })
  # print(length(ars))
  arsdf <- unnest(as.data.frame(do.call(rbind, ars)), 1:19)
  colnames(arsdf) <- columnnames
  arsdf
}
