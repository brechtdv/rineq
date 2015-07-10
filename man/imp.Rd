\name{imp}
\alias{imp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluating variable importance in \code{rpart_ci()} trees
}
\description{
This function measures the variable importance for objects produced by rpart_ci()
}
\usage{
imp(object, surrogates = TRUE, competes = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{\link{rpart}}.}
  \item{surrogates}{optional. Should surrogate splits contribute to the importance calculation (logical)? If missing, \code{TRUE} is set by default.}
  \item{competes}{optional. Should competing splits contribute to the importance calculation (logical)? If missing, \code{FALSE} is set by default.}
  \item{...}{not used.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data.frame with one row for each predictor variable (ordered by decreasing importance). The first column \code{overall.importance} is the variable importance measure (the total decrease in node impurities) and the second column \code{relative.importance} is the relative importance variable (sum to 100). 
}
\references{
Breiman, L., Friedman, J., Stone, C. J., & Olshen, R. A. (1984). Classification and regression trees. CRC press.
}
\author{
Saveria Willimès
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Nigeria)
example <- rpart_ci(
  cbind(wealth, skilled) ~  quintile + bord + agemother + agechild 
                            + region + ed + religion + rural + mocc,
 Nigeria ,weight, type = "CI")
imp(example, surrogates = FALSE, competes = TRUE )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
