\name{glmpath.cr-internal}
\alias{glmpath.cr-internal}
\alias{print.glmpath.cr}
\alias{cr.backward}
\alias{cr.forward}
\title{
Functions called by \code{glmpath.cr} functions, not by the user
}
\description{
These functions are called my other \code{glmpath.cr} functions and are not intended to be directly called by the user.
}
\usage{
cr.backward(x,y)
cr.forward(x,y)
\method{print}{glmpath.cr}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix of predictor variables}
  \item{y}{the ordinal response to be predicted}
  \item{fit}{a fitted \code{glmpath.cr} object}
}
\details{
The \code{cr.backward} and \code{cr.forward} functions restructure the original dataset to represent the K-1 conditionally independent datasets needed for the full likelihood.
}
\references{
Bender & Benner and O'Connell for backward CR programming
}
\author{
Kellie J. Archer, \email{kjarcher@vcu.edu}
}

\seealso{
See Also as \code{\link{glmpath.cr}}.
}

\keyword{ internal }