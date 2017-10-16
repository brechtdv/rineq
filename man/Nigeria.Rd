\name{Nigeria}
\alias{nigeria}
\docType{data}
\title{Sample dataset}
\description{A sample dataset, based on a DHS household survey of Nigeria (2013).}
\usage{data(nigeria)}
\format{
  A data frame with 999 observations on the following 15 variables.
  \describe{
    \item{\code{wealth}}{a numeric vector; a measure of the household's wealth}
    \item{\code{quintile}}{a factor ; it gives the wealth quintiles}
    \item{\code{agechild}}{a integer vector; a measure of the child's age in months}
    \item{\code{bord}}{a integer vector; it gives the birth order}
    \item{\code{agemother}}{a integer vector; a measure of the mother's age in years}
    \item{\code{ed}}{a factor with levels "\code{education}" and "\code{no education}"}
    \item{\code{rural}}{a factor with levels "\code{N}": urban and "\code{Y}":rural}
    \item{\code{region}}{a factor with levels "\code{a}","\code{b}","\code{c}","\code{d}","\code{e}","\code{f}"; it gives the region of residence}
    \item{\code{reg}}{...}
    \item{\code{PSU}}{...}
    \item{\code{weight}}{a numeric vector; it gives the sample weight}
    \item{\code{male}}{...}
    \item{\code{zscore}}{a numeric vector; it gives the height-for-age standard deviation (an indicator of malnutrition)}
    \item{\code{zscore1}}{...}
    \item{\code{zscore2}}{...}  
  }
}

\source{\url{http://www.measuredhs.com}}
\examples{
data(nigeria)
names(nigeria)
}
\keyword{datasets}
