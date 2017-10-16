\name{rank_wt}
\alias{rank_wt}
\title{Ranks a numeric variable taking weights into account. Mimics a Stata function}
\description{
This function calculates the weighted fractional ranks of a numeric variable.
}
\usage{
rank_wt(x, wt)
}
\arguments{
  \item{x}{numeric vector}
  \item{wt}{weights}
}
\value{a numeric vector containing weighted fractional ranks of the elements of \code{x}.}
\references{Kakwani \emph{et al}., 1997.
}
\author{Peter Konings}
\examples{
x <- sample(1:10, size = 10, replace = TRUE)
x.weight <- seq(0, 1, length.out = 10)
rank_wt(x, wt = x.weight)
}
\keyword{univar}
\keyword{utilities}
