\name{summary.glmpath.cr}
\alias{summary.glmpath.cr}
\title{
Summarize a glmpath.cr Object
}
\description{
This function summarizes the fits of \code{glmpath.cr} over a path and returns a matrix that includes the degrees of freedom, deviance, AIC, and BIC associated with each model fit.
}
\usage{
\method{summary}{glmpath.cr}(object, ...)
}
\arguments{
  \item{object}{a glmpath.cr object}
  \item{\dots}{additional arguments}
}
\value{
A matrix is returned with the following columns corresponding to each model fit (rows):
\item{Df}{degrees of freedom at each step}
\item{Deviance}{deviance computed at each step}
\item{AIC}{AIC value at each step}
\item{BIC}{BIC value at each step}
}
\references{
Mee Young Park and Trevor Hastie (2007) L1 regularization path algorithm for generalized linear models. J. R. Statist. Soc. B, 69, 659-677.
}
\author{
Kellie J. Archer \email{kjarcher@vcu.edu}
}
\note{
This function is essentially the same as \code{summary.glmpath} from the \pkg{glmpath} package by Park and Hastie but was designed to operate on a returned \code{glmpath.cr} object.
}

\seealso{
See Also as \code{\link{predict.glmpath.cr}}, \code{\link{coef.glmpath.cr}}
}
\examples{
data(data)
fit <- glmpath.cr(x = data[,1:1000], y = data$class)
summary(fit)
}
\keyword{ methods }