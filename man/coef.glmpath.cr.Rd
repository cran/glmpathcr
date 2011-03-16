\name{coef.glmpath.cr}
\alias{coef.glmpath.cr}
\title{
Extract All Model Coefficients
}
\description{
The \code{coef.glmpath.cr} function returns all estimated coefficients for a \code{glmpath.cr} fitted model, where the model selected is indicated by step s.  
}
\usage{
\method{coef}{glmpath.cr}(object, s, weight = NULL, eps = .Machine$double.eps, ...)
}

\arguments{
  \item{object}{a \code{glmpath.cr} object}
  \item{s}{the step at which the coefficient estimates are desired}
  \item{weight}{weights, if any}
  \item{eps}{effective zero}
  \item{\dots}{additional arguments}
}

\value{
A vector of estimated coefficients is returned. The \code{coef.glmpath.cr} function returns all estimated coefficients for a glmpath.cr fitted model, where the model selected is indicated by step s. 
}

\author{
Kellie J. Archer, \email{kjarcher@vcu.edu}
}

\seealso{
See also as \code{\link{glmpath.cr}}, \code{\link{nonzero.coef}}
}

\examples{
data(data)
fit <- glmpath.cr(x = data[,1:1000], y = data$class)
step <- model.select(fit, which="BIC")
coef(fit, s = step)
nonzero.coef(fit, s = step)
}
\keyword{ methods }
