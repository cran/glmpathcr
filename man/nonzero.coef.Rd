\name{nonzero.coef}
\alias{nonzero.coef}
\title{
Extract Non-Zero Model Coefficients
}
\description{
The \code{nonzero.coef} function returns only those non-zero coefficient estimates for a selected model.    
}
\usage{
nonzero.coef(object, s)
}

\arguments{
  \item{object}{a \code{glmpath.cr} object}
  \item{s}{the step at which the non-zero coefficient estimates are desired}
}

\value{
A vector of estimated coefficients is returned.  The \code{nonzero.coef} function returns only those non-zero coefficient estimates for a selected model.
}

\author{
Kellie J. Archer, \email{kjarcher@vcu.edu}
}

\seealso{
See also as \code{\link{glmpath.cr}}, \code{\link{coef.glmpath.cr}}
}

\examples{
data(data)
fit <- glmpath.cr(x = data[,1:1000], y = data$class)
step <- model.select(fit, which="BIC")
coef(fit, s = step)
nonzero.coef(fit, s = step)
}
\keyword{ misc }
