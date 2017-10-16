\name{correctSign}
\Rdversion{1.1}
\alias{correct_sign}
\alias{corrected_value}
\alias{is_corrected}

\title{Corrects negative values in the health variable
}
\description{The Relative Concentration Index is not bonded between [-1,1] if the health variable contains both negative and positive values. This function corrects for this either by imputing a value of 0 for all negative values or by subtracting the minimum value.
}
\usage{
correct_sign(x, shift = TRUE)
}

\arguments{
  \item{x}{a numeric vector, typically representing health.
}
  \item{shift}{logical. If \code{FALSE} (the default), 0 is imputed for all negative values in \code{x}. If \code{TRUE} the minimum value of \code{x} is subtracted from it.
}
}

\value{
Returns a list with 2 components:
  \item{corrected}{corrected version of \code{x}}
  \item{modified}{logical, \code{TRUE} when any of the elements of \code{x} have been changed}
These components can be extracted with the functions \code{correctedValue} and \code{isCorrected}.
}

\author{Peter Konings}

\examples{
data(nigeria)

nigeria$zscore.shifted <- corrected_value(correct_sign(nigeria$zscore, shift = TRUE))
nigeria$zscore.imputed <- corrected_value(correct_sign(nigeria$zscore, shift = FALSE))

## compare the effect of both methods
plot(density(nigeria$zscore, na.rm = TRUE))
points(density(nigeria$zscore.shifted, na.rm = TRUE), col = 'blue')
points(density(nigeria$zscore.imputed, na.rm = TRUE), col = 'green')
}

\keyword{utilities}
\keyword{methods}
