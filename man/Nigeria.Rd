\name{Nigeria}
\alias{nigeria}
\docType{data}
\title{Sample dataset}
\description{A sample dataset, based on a DHS household survey of Nigeria (2013).}
\usage{data(nigeria)}
\format{
  A data frame with 999 observations on the following 15 variables.
  \describe{
    \item{\code{wealth}}{a numeric vector; the household's wealth}
    \item{\code{quintile}}{a factor; the household's wealth quintile}
    \item{\code{agechild}}{a integer vector; the child's age in months}
    \item{\code{bord}}{a integer vector; the birth order}
    \item{\code{agemother}}{a integer vector; the mother's age in years}
    \item{\code{ed}}{a factor with levels "\code{education}" and "\code{no education}"}
    \item{\code{rural}}{a factor with levels "\code{N}": urban and "\code{Y}":rural}
    \item{\code{region}}{a factor; region of residence}
    \item{\code{reg}}{a factor; region in which the respondent was interviewed}
    \item{\code{PSU}}{a numeric vector; cluster number (enumeration areas drawn from census files)}
    \item{\code{weight}}{a numeric vector; the sample weight}
    \item{\code{male}}{a factor; sex of the child (1= male, 0= female)}
    \item{\code{zscore}}{a numeric vector; the height-for-age standard deviation (an indicator of malnutrition)}
    \item{\code{zscore1}}{a numeric vector; transformed z-score: the minimum value was subtracted to all observations to have only positive values}
  }
}

\source{\url{http://www.measuredhs.com}}
\examples{
data(nigeria)
names(nigeria)
}
\keyword{datasets}
