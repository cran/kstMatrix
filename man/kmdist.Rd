% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmdist.R
\name{kmdist}
\alias{kmdist}
\title{Compute the distance between a data set and a knowledge structure}
\usage{
kmdist(data, struct)
}
\arguments{
\item{data}{Binary matrix representing a set of response patterns}

\item{struct}{Binary matrix representing a knowledge structure}
}
\value{
Distance distribution vector
}
\description{
\code{kmdist} returns a named vector with the frequencies of distances
between a set of response patterns and a knowledge structure. This vector
can be used to compute, e.g., the Discrepancy Index (DI) or the
Distance Agreement Coefficient (DA).
}
\examples{
kmdist(xpl$data, xpl$space)

}
\seealso{
Other Validating knowledge spaces: 
\code{\link{kmvalidate}()}
}
\concept{Validating knowledge spaces}
\keyword{math}
