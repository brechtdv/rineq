\name{contribution}
\alias{contribution}
\alias{contribution.coxph}
\alias{contribution.glm}
\alias{contribution.lm}
\alias{contribution.svyglm}
\alias{decomposition}
\alias{plot.decomposition}
\alias{print.decomposition}
\alias{summary.decomposition}

\title{
Functions to decompose the Relative Concentration Index into its components
}
\description{
These functions decompose the Relative Concentration Index into its components using a (generalized) linear model, optionally using a survey design, or a Cox Proportional Hazards model. Print, summary and plot methods have been defined for the results.
}
\usage{
contribution(object, ranker, correction = TRUE)
\method{contribution}{coxph}(object, ranker, correction = TRUE)
\method{contribution}{glm}(object, ranker, correction = TRUE)
\method{contribution}{lm}(object, ranker, correction = TRUE)
\method{contribution}{svyglm}(object, ranker, correction = TRUE)
}

\arguments{
  \item{object}{an object of class \code{coxph}, \code{glm}, \code{lm} or \code{svyglm}. The outcome should be the health variable and the predictors the components.
}
  \item{ranker}{a numeric vector containing the wealth variable, from the same dataframe as the outcome.
}
  \item{correction}{a logical indicating whether the global and partial RCIs should be corrected for negative values using imputation.
}
}
\details{
If \code{correction} is \code{TRUE} negative values of components or outcome are corrected using \code{\link{correct_sign}} with option \code{shift = FALSE}.
}
\value{
An object of class \code{decomposition} containing the following components
\item{betas}{a numeric vector containing regression coefficients}
\item{partialcis}{a numeric vector containing partial RCIs}
\item{confints}{a numeric vector contaning 95\% confience intervals for the partial concentration indices}
}
\references{
Konings \emph{et al.}, 2009
Speybroeck \emph{et al.}, 2009
}
\author{Peter Konings
}
\section{Warning}{
\code{ranker} should be chosen with care. Ideally, it is a variable from the same dataframe as the other variables. If not, redefine the row names in the model. 
}

\examples{
data(kenya)
simple_model <- lm(stunting ~ meduY + sex +  wealth + urban, data = kenya)
simple_contrib <- contribution(simple_model, kenya$wealth)
print(simple_contrib)
plot(simple_contrib)
}
