#' @method kmclosure kmattributionfunction
#' @family Closure operators
#' @rdname kmclosure
#' @export
kmclosure.kmattributionfunction <- function(x, ...) {
  af <- data.frame(x)
  # We start with reflexivity
  sapply((1:dim(af)[1]), function(i) {
    j <- af[i,1]
    af[i, j] <<- 1
  })
  # Next step: Transitivity
  if (dim(af)[2] > 3) {
    chgd <- TRUE
    while (chgd) {
      chgd <- FALSE
      noclauses <- dim(af)[1]
      sapply((1:noclauses), function(r) {
        if (!chgd) {
          curritem <- af[r,1]
          currclause = af[r,]
          currclause[curritem] <- 0
          prereqs <- colnames(af)[which(currclause==1)]
          clause <- as.numeric(af[r,-1])
          if (length(prereqs)>0) {
            sapply(prereqs, function(p) {
              if (!chgd) {
                cprimes <- which(af[,1] == p)
                if (length(cprimes) == 0)
                  stop(sprintf("I found no clause for item %s", p))
                foundtrans <- FALSE
                sapply(cprimes, function(cp)  {
                  clauseprime <- as.numeric(af[cp,-1])
                  if (length(clause) != length(clauseprime)) print(af)
                  if (all((clause & clauseprime) == clauseprime))
                    foundtrans <<- TRUE
                })
                if (!foundtrans) {
                  sapply(cprimes, function(cp) {
                    hf <- data.frame(curritem, t(as.numeric(af[cp,-1]|af[r,-1])))
                    names(hf) <- colnames(af)
                    af <<- rbind(af, hf)
                  })
                  af <<- af[-r,]
                  chgd <<- TRUE
                }
              }
            })
          }
        }
      })
    }
  }
  # Pre-Final step: Incomparability
  items <- dim(af)[2]-1
  chgd <- TRUE
  af[,2:(items+1)] <- sapply(af[,2:(items+1)], as.numeric)
  while (chgd) {
    chgd <- FALSE
    lapply(unique(af[,1]), function(s) {
      if (!chgd) {
        sel <- which(af[,1] == s)
        lapply(sel, function(x) {
          if (!chgd) {
            lapply(sel, function(y) {
              if (!chgd) {
                if (x != y) {
                  if (all((af[x,-1] & af[y,-1]) == af[x,-1])) {
                    af <<- af[-y,]
                    chgd <<- TRUE
                  }
                }
              }
            })
          }
        })
      }
    })
  }
  # Very final step: make sure, every item has a clause
  items <- colnames(af)[-1]
  clauseditems <- unique(af[,1])
  exists <- items %in% clauseditems
  if (!(all(exists))) {
    missing <- items[which(!(exists))]
    hmat <- matrix(rep(0, length(missing)*length(items)), nrow=length(missing))
    hdf <- cbind(data.frame((missing)), data.frame(hmat))
    colnames(hdf) <- colnames(af)
    sapply((1:length(missing)), function(x) {
      index <- which(hdf[,1] == missing[x])
      hdf[index,missing[x]] <<- 1
    })
    af <- rbind(af, hdf)
  }
  class(af) <- unique(c("kmsurmisefunction", class(af)))
  af
}
