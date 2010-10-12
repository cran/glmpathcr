\name{model.select}
\alias{model.select}
\title{
Step of Optimal Fitted AIC or BIC CR Model.
}
\description{
This function returns the step number associated with the minimum AIC and BIC models for a \code{glmpath.cr} object and so can be used in selecting the step number for calls to functions such as \code{coef.glmpath.cr}, \code{effect.glmpath.cr}, and \code{predict.glmpath.cr}.
}
\usage{
model.select(object, which = "BIC")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a \code{glmpath.cr} object}
  \item{which}{determines whether the step is associated with the optimal \code{which="BIC"} model or \code{which="AIC"} model}
}

\value{
A numeric value of length one representing the step number
}
\references{
Kellie J. Archer and Andre A.A. Williams (2010) Technical Report.
}
\author{
Kellie J. Archer, \email{kjarcher@vcu.edu}
}

\seealso{
See also as \code{\link{coef.glmpath.cr}}, \code{\link{predict.glmpath.cr}}
}
\examples{
data(data)
fit <- glmpath.cr(x = data[,1:1000], y = data$class)
model.select(fit, which="AIC")
}
\keyword{ methods }