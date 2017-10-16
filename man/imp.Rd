\name{imp}
\alias{imp}

\title{
Evaluating variable importance in \code{rpart_ci()} trees
}
\description{
This function measures the variable importance for objects produced by rpart_ci()
}
\usage{
imp(object, surrogates = TRUE, competes = FALSE, ...)
}

\arguments{
  \item{object}{an object of class \code{\link{rpart}}.}
  \item{surrogates}{optional. Should surrogate splits contribute to the importance calculation (logical)? If missing, \code{TRUE} is set by default.}
  \item{competes}{optional. Should competing splits contribute to the importance calculation (logical)? If missing, \code{FALSE} is set by default.}
  \item{...}{not used.}
}

\value{
A data.frame with one row for each predictor variable (ordered by decreasing importance). The first column \code{overall.importance} is the variable importance measure (the total decrease in node impurities) and the second column \code{relative.importance} is the relative importance variable. 
}
\references{
Breiman, L., Friedman, J., Stone, C. J., & Olshen, R. A. (1984). Classification and regression trees. CRC press.
}
\author{
Saveria Willimès
}

\examples{
data(Nigeria)
example <- rpart_ci(
  cbind(wealth, zscore) ~ quintile + bord + agemother + agechild 
                            + region + ed + religion + rural + mocc,
 nigeria, nigeria$weight, type = "CI")
imp(example, surrogates = FALSE, competes = TRUE )
}
