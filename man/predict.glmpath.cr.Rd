\name{predict.glmpath.cr}
\alias{predict.glmpath.cr}
\alias{fitted.glmpath.cr}

\title{
Predicted Class and Fitted Probabilities from glmpath.cr Object
}

\description{
Returns either the predicted class or the fitted probabilities from the penalized continuation ratio model from a \code{glmpath.cr} object.
}

\usage{
\method{predict}{glmpath.cr}(object, newx, newy, method="backward", which = "BIC", type = "class", ...)
\method{fitted}{glmpath.cr}(object, newx, newy, method="backward", which = "BIC", type = "class", ...)
}

\arguments{
  \item{object}{a \code{glmpath.cr} object}
  \item{newx}{a data matrix containing the predictor variables}
  \item{newy}{the ordinal response vector}
  \item{method}{whether \code{backward} (default) or \code{forward} continuation ratio model was fit for \code{object}}
  \item{which}{used for model selection, either the default \code{which}="BIC" or \code{which}="AIC"}
  \item{type}{used for selecting desired output, either the default \code{type}="class" or \code{type}="probes" for the fitted probabilities}
  \item{\dots}{additional optional arguments}
}

\value{
Either a vector of the predicted class labels or a matrix containing the fitted probabilities is returned.
}
\references{
Kellie J. Archer and Andre A.A. Williams (2010) Technical Report.
}
\author{
Kellie J. Archer, \email{kjarcher@vcu.edu}
}

\seealso{
See also as \code{\link{glmpath.cr}}, \code{\link{coef.glmpath.cr}}.
}
\examples{
data(data)
x <- data[,1:1000]
y <- data$class
fit <- glmpath.cr(x=x, y=y)
pred <- predict(fit, newx=x, newy=y)
table(pred, y)
pred <- predict(fit, newx=x, newy=y, which="AIC", type="probs")
}
\keyword{ methods }