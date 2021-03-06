\name{ci}
\alias{ci}
\alias{plot.hci}
\alias{confint.hci}
\alias{default.hci}
\alias{print.hci}
\alias{summary.hci}
\alias{concentration_index}
\alias{variance_concentration_index}
\title{A function to calculate the concentration index.}

\description{
This function calculates the relative concentration index (Kakwani et al), the generalized concentration index (Clarke et al, 2002) and the concentration index with Erreygers' correction (Erreygers et al, 2009).  It returns an object of class hci for which confidence intervals, summaries and plots are defined.
}

\usage{
ci(x, y, wt = NULL, type = c("CI", "CIg", "CIc"))

concentration_index(object)

\method{plot}{hci}(x, ...)
\method{print}{hci}(x, ...)
\method{summary}{hci}(object, ...)
\method{confint}{hci}(object, parm = NULL, level = 0.95, ...)
variance_concentration_index(object)
}

\arguments{
 \item{x}{For \code{ci}, a numeric variable containing the health measure. For the \code{print} and \code{plot} methods: an object of class \code{hci}.
}
  \item{y}{
a numeric variable containing the wealth measure.
}
  \item{wt}{
a numeric variable containing the sample weights. Defaults to \code{NULL}, which is equal to a constant sample weight for all cases.
}
  \item{type}{
the type of concentration index to be calculated: relative concentration index (\code{CI}), generalized concentration index (\code{CIg}), or concentration index with Erreygers Correction \code{CIc}.
}

  \item{object}{
an object of class \code{hci} () the result of a call to \code{RCI}).
}
  \item{parm}{
always set to \code{NULL}; provided for consistency with the generic \code{confint}.  
}
  \item{level}{
confidence level at which confidence intervals should be calculated. Defaults to 0.95.
}
  \item{...}{
arguments to pass on to \code{plot}, \code{print} and \code{summary}.
}
}
\details{
\code{ci} returns the concentration index in an object of class \code{hci}.
A number of extractor functions have been defined. The \code{plot} function returns a plot of the Lorenz curve.
}

\value{For \code{ci}, an object of class \code{hci}; this is a list containing the following components:
\item{concentration_index}{concentration index. The type of concentration index depends on the chosen \code{type} argument.}
\item{varC}{variance of the concentration index}
\item{fractional_rank}{weighted fractional rank for each case}
\item{outcome}{health measure for each case}
\item{n}{number of cases}
\item{call}{the original call to \code{RCI}}
For \code{plot.hci}, a concentration plot.
}
\references{
Clarke, P. M., Gerdtham, U. G., Johannesson, M., Bingefors, K., & Smith, L. (2002). On the measurement of relative and absolute income-related health inequality. Social Science & Medicine, 55(11), 1923-1928

Erreygers, G. (2009). Correcting the concentration index. Journal of health economics, 28(2), 504-515

Kakwani, N., Wagstaff, A., & Van Doorslaer, E. (1997). Socioeconomic inequalities in health: measurement, computation, and statistical inference.Journal of econometrics, 77(1), 87-103.

}
\author{
Saveria Willimes, Peter Konings and Niko Speybroeck
}

\examples{
data(nigeria)
example <- with(nigeria, ci(x = wealth, y = zscore, wt = weight, type = "CIg"))
plot(example)
summary(example)
}
