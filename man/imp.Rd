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
  \item{surrogates}{should surrogate splits contribute to the importance calculation (logical)? Defaults to \code{TRUE}.}
  \item{competes}{should competing splits contribute to the importance calculation (logical)? Defaults to \code{FALSE}.}
  \item{...}{currently ignored.}
}

\value{
A data.frame with one row for each predictor variable (ordered by decreasing importance). The first column \code{overall.importance} is the variable importance measure (the total decrease in node impurities) and the second column \code{relative.importance} is the relative importance variable. 
}

\references{
Breiman, L., Friedman, J., Stone, C. J., & Olshen, R. A. (1984). Classification and regression trees. CRC press.
}

\author{
Saveria Willimes
}

\examples{
data(nigeria)

tree <-
rpart_ci(
  cbind(wealth, zscore) ~
    quintile + ed + rural + region + male + bord + agechild + agemother,
  data = nigeria,
  weigths = nigeria$weight,
  type = "CI")

imp(tree, surrogates = FALSE, competes = TRUE)
}
