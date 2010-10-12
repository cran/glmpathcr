\name{plot.glmpath.cr}
\alias{plot.glmpath.cr}
\title{
Plots the Regularization Path Computed from glmpath.cr
}
\description{
This function takes a \code{glmpath.cr} object and visualizes the regularization path. The horizontal axis can be \code{norm}, \code{lambda} or \code{step}. The vertical axis can be \code{coefficients}, \code{aic} or \code{bid}.
}
\usage{
\method{plot}{glmpath.cr}(x, xvar = c("norm", "lambda", "step"), type = c("coefficients", "aic", "bic"), plot.all.steps = FALSE, xlimit = NULL, predictor = FALSE, omit.zero = TRUE, breaks = TRUE, mar = NULL, eps = .Machine$double.eps, main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{glmpath.cr} object}
  \item{xvar}{horizontal axis. \code{xvar=norm} plots against the L1 norm of the coefficients (to which L1 norm penalty was applied); \code{xvar=lambda} plots against \eqn{\lambda}; and \code{xvar=step} plots against the number of steps taken. Default is \code{norm}.}
  \item{type}{type of the plot, or the vertical axis. Default is \code{coefficients}.}
  \item{plot.all.steps}{If \code{TRUE}, all the steps taken along the path are marked on the plot. If \code{FALSE}, which is the default, only the steps at which the active set changed are shown on the plot.}
  \item{xlimit}{When the user wants to visualize a (beginning) sub-part of the plot, \code{limit} sets an upper limit to the L1 norm or the number of steps, or a lower limit to \eqn{\lambda}.}
  \item{predictor}{If \code{TRUE} and \code{type=coefficients}, the predictor step estimates are connected with dotted lines. If FALSE, only the corrector step estimates are connected with solid lines}
  \item{omit.zero}{If \code{TRUE} and \code{type=coefficients}, the predictors that were never in the active set are omitted.}
  \item{breaks}{If \code{TRUE}, vertical lines are drawn at the points where the active set changes and numbered with the degrees of freedom.}
  \item{mar}{margin relative to the current font size}
  \item{eps}{an effective zero}
  \item{main}{title of the plot}
  \item{\dots}{other options for the plot}
}
\references{
Mee Young Park and Trevor Hastie (2007) L1 regularization path algorithm for generalized linear models. J. R. Statist. Soc. B, 69, 659-677.
}
\note{
This function is essentially the same as \code{plot.glmpath} from the \pkg{glmpath} package by Park and Hastie but was designed to operate on a returned \code{glmpath.cr} object.
}
\author{
Mee Young Park and Trevor Hastie; Kellie J. Archer \email{kjarcher@vcu.edu} used here for new \code{glmpath.cr} class
}

\seealso{
See also \code{\link{glmpath.cr}}, \code{\link{predict.glmpath.cr}}
}
\examples{
data(data)
fit <- glmpath.cr(x = data[,1:1000], y = data$class)
plot(fit, xvar="step", plot.all.steps = TRUE)
}
\keyword{ methods }

