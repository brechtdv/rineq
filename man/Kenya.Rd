\name{Kenya}
\Rdversion{1.1}
\alias{kenya}
\docType{data}
\title{Sample dataset}
\description{
A sample dataset, based on a DHS household survey of Kenya (2003).
}
\usage{kenya}
\format{
  A data frame with 79440 observations on the following 6 variables.
  \describe{
    \item{\code{stunting}}{a numeric vector; a measure of the subject's stunting}
    \item{\code{wealth}}{a numeric vector; a measure of the household's wealth}
    \item{\code{popweight}}{a numeric vector, containing sample weight}
    \item{\code{meducY}}{a numeric vector; the subject's mother's education in years}
    \item{\code{sex}}{a factor with levels \code{male} \code{female}}
    \item{\code{urban}}{a factor with levels \code{0} \code{1}; with \code{0}: rural and \code{1}: urban}
  }
}
\source{
\url{http://www.measuredhs.com}
}
\examples{
data(Kenya)
str(Kenya)
}
\keyword{datasets}
