\name{Nigeria}
\alias{Nigeria}
\docType{data}
\title{Sample dataset}
\description{
A sample dataset, based on a DHS household survey of Nigeria (2013).
}
\usage{data("Nigeria")}
\format{
  A data frame with 999 observations on the following 15 variables.
  \describe{
    \item{\code{vmeasles}}{a integer vector; with \code{0}: unvaccined and \code{1}: vaccined against measles}
    \item{\code{skilled}}{a integer vector; with \code{0}: unassisted delivery and \code{1}: assisted delivery}
    \item{\code{wealth}}{a numeric vector; a measure of the household's wealth}
    \item{\code{quintile}}{a factor vector; it gives the wealth quintiles}
    \item{\code{agechild}}{a integer vector; a measure of the child's age in months}
    \item{\code{male}}{a integer vector; with \code{0}: female and \code{1}: male}
    \item{\code{bord}}{a integer vector; it gives the birth order}
    \item{\code{agemother}}{a integer vector; a measure of the mother's age in years}
    \item{\code{married}}{a integer vector; with \code{0}: unmarried respondent and \code{1}: married respondent}
    \item{\code{ed}}{a factor with levels "\code{education}" and "\code{no education}"}
    \item{\code{mocc}}{a factor with levels "\code{not working}", "\code{prof, techn, manag, cler, sales}", "\code{agriculture}" and "\code{manual, domestic}"}
    \item{\code{rural}}{{a integer vector; with \code{0}: urban and \code{1}:rural}}
    \item{\code{region}}{a integer vector; it gives the region of residence}
    \item{\code{religion}}{a integer vector; it gives the household's religion}
    \item{\code{weight}}{a numeric vector; it gives the sample weight}
    
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
