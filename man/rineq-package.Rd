\name{rineq-package}
\alias{rineq-package}
\alias{rineq}

\docType{package}

\title{Statistical Analysis of Health Inequalities}

\description{
The \pkg{rineq} package provides functions to  calculate the relative, generalized, and Erreygers corrected concentration index; plot Lorenz curves; and decompose health inequalities using (generalized) linear models, survival models, and regression and binary trees.
}

\details{
  \tabular{ll}{
    Package:    \tab  rineq\cr
    Type:       \tab  Package\cr
    Version:    \tab  0.0.1\cr
    Date:       \tab  2017-10-16\cr
    Authors:    \tab  Brecht Devleesschauwer, Saveria Willimes, Carine Van Malderen, Peter Konings, Niko Speybroeck\cr
    Maintainer: \tab  Brecht Devleesschauwer <\email{brechtdv@gmail.com}>\cr
    Depends:    \tab  R (>= 3.4.0), rpart\cr
    License:    \tab  GPL (>= 2)\cr
    LazyData:   \tab  true\cr
}

Available functions in the \pkg{rineq} package:
  \tabular{ll}{
    \strong{Concentration index}\cr
    \code{\link{ci}}              \tab Calculate the concentration index.\cr
    \code{\link{correct_sign}}    \tab Correct negative values in the health variable.\cr\cr

    \strong{Decomposition}\cr
    \code{\link{contribution}}    \tab Decompose the Relative Concentration Index into its components.\cr
    \code{\link{rpart_ci}}        \tab Recursive partitioning and regression trees using the concentration index.\cr
    \code{\link{imp}}             \tab Evaluate variable importance in rpart_ci() trees.\cr\cr
  }
}

\keyword{package}
