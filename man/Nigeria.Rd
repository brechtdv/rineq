\name{Nigeria}
\alias{Nigeria}
\docType{data}
\title{Sample dataset}
\description{
A sample dataset, based on a DHS household survey of Nigeria (2013).
}
\usage{data("Nigeria")}
\format{
  A data frame with 999 observations on the following 14 variables.
  \describe{
    \item{\code{quintile}}{a factor ; it gives the wealth quintiles}
    \item{\code{wealth}}{a numeric vector; a measure of the household's wealth}
    \item{\code{zscore}}{a numeric vector; it gives the height-for-age standard deviation (an indicator of malnutrition)}
    \item{\code{bord}}{a integer vector; it gives the birth order}
    \item{\code{agemother}}{a integer vector; a measure of the mother's age in years}
    \item{\code{weight}}{a numeric vector; it gives the sample weight}
    \item{\code{agechild}}{a integer vector; a measure of the child's age in months}
    \item{\code{region}}{a factor with levels "\code{a}","\code{b}","\code{c}","\code{d}","\code{e}","\code{f}"; it gives the region of residence}
    \item{\code{religion}}{a factor with levels "\code{a}","\code{b}","\code{c}","\code{d}"; it gives the household's religion}
    \item{\code{rural}}{a factor with levels "\code{N}": urban and "\code{Y}":rural}
    \item{\code{mocc}}{a factor with levels "\code{not working}", "\code{prof, techn, manag, cler, sales}", "\code{agriculture}" and "\code{manual, domestic}"}
    \item{\code{ed}}{a factor with levels "\code{education}" and "\code{no education}"}
    \item{\code{skilled}}{a factor with levels "\code{N}": unassisted delivery and "\code{Y}": assisted delivery}
    \item{\code{married}}{a factor with levels "\code{N}": unmarried respondent and "\code{Y}": married respondent}
        
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\url{http://www.measuredhs.com}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Nigeria)
names(Nigeria)
}
\keyword{datasets}
