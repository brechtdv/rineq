\name{rpart_ci}
\alias{rpart_ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recursive Partitioning and Regression Trees Using the Concentration Index }
\description{ Fit a \code{\link{rpart}} model using the concentration index as a measure of deviance.}
\usage{
rpart_ci(formula, data, weights, type = c("CI", "CIg", "CIc"), 
    subset, na.action = na.rpart, method, model = FALSE, x = FALSE, 
    y = TRUE, control, cost, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a \code{\link{formula}}, with y as a two columns object (the first one must be the wealth variable and the second one the health outcome variable) and no interaction terms. If it's a data frame that is taken as the model frame, see \code{\link{model.frame}}.}
  \item{data}{an optional data frame in which to interpret the variables named in the formula.}
  \item{weights}{optional case weight}
  \item{type}{one of \code{"CI"}, \code{"CIg"}, \code{"CIc"}. If type is missing, "CI" is chosen as the default action. \code{"CI"} corresponds to the Concentration Index, \code{"CIg"} to Generalized Concentration Index (see Clarke and al, 2002) and \code{"CIc"} to Corrected Concentration Index (see Erreygers, 2009).}
  \item{subset}{optional expression saying that only a subset of the rows of the data should be used in the fit.}
  \item{na.action}{the default action deletes all observations for which y is missing, but keeps those in which one or more predictors are missing.}
  \item{method}{by default, the concentration index' method is used according to the \code{type} argument. If no \code{type} argument is specified, the \code{"CI"} method is used}
  \item{model}{if logical: keep a copy of the model frame in the result? If the input value for model is a model frame (likely from an earlier call to the rpart function), then this frame is used rather than constructing new data.}
  \item{x}{keep a copy of the x matrix in the result.}
  \item{y}{keep a copy of the dependent variable in the result. If missing and model is supplied this defaults to FALSE.}
  \item{control}{optional case weight}
  \item{cost}{a vector of non-negative costs, one for each variable in the model. Defaults to one for all variables. These are scalings to be applied when considering splits, so the improvement on splitting on a variable is divided by its cost in deciding which split to choose.}
  \item{...}{arguments to \code{rpart.control} may also be specified in the call to \code{rpart_cil}. They are checked against the list of valid arguments.}
}
\details{

}
\value{
An object of class \code{rpart}. See \code{\link{rpart.object}}.
}
\references{
 Breiman L., Friedman J. H., Olshen R. A., and Stone, C. J. (1984)
  \emph{Classification and Regression Trees.}
  Wadsworth.
}
\author{
Saveria Willimès and Brecht Devleesschauwer, adapting from the \code{\link{rpart}}
  package by Terry M.Therneau and Beth Atkinson.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{imp}},
  \code{\link{rpart.control}}, \code{\link{rpart.object}},
  \code{\link{summary.rpart}}, \code{\link{print.rpart}}
}
\examples{
data(Nigeria)
example <- rpart_ci(
  cbind(wealth, skilled) ~  quintile + bord + agemother + agechild 
                            + region + ed + religion + rural + mocc,
 Nigeria ,weight, type = "CI")
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(example)
text(example, use.n = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
