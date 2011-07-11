\name{glmpathcr-package}
\alias{glmpathcr-package}
\alias{glmpathcr}
\docType{package}
\title{
Penalized L1 continuation Ratio Models for Ordinal Response Prediction in High-dimensional Data Settings
}
\description{
This package provides a function glmpath.cr for fitting a penalized L1 continuation ratio model for predicting an ordinal response and associated methods for plotting, getting predicted values, estimating coefficients for selected models.}
\details{
\tabular{ll}{
Package: \tab glmpathcr\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2011-06-23\cr
License: \tab GPL2.0\cr
LazyLoad: \tab yes\cr
}
This package contains functions for fitting a penalized continuation ratio model and extracting estimated coefficients, predicted class, and fitted probabilities. The model and methods can be used when the response to be predicted is ordinal, and is particularly relevant when there are more covariates than observations.
}
\author{
Kellie J. Archer

Maintainer: Kellie J. Archer <kjarcher@vcu.edu>
}
\references{
Kellie J. Archer and Andre A.A. Williams (2010) Technical Report.
}
\keyword{ package }
\keyword{ models }
\keyword{ classif }

\seealso{
   See also \code{\link[glmpath:glmpath]{glmpath}}
}
\examples{
data(diabetes)
x <- diabetes[, 2:dim(diabetes)[2]]
y <- diabetes$y
fit <- glmpath.cr(x, y)
}
