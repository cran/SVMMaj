\name{isb}
\alias{isb}
\title{ I-spline basis of each column of a given matrix }
\description{
  Create a  I-spline basis for an array. \ will equally distribute the knots over the value range using quantiles.
}
\usage{
isb(x, spline.knots = 0 , knots=NULL, spline.degree = 1)
}

\arguments{
  \item{x}{The predictor variable, which will be tranformed into I-spline basis.}
  \item{spline.knots}{Number of inner knots to use. \code{isb} will equally distribute the knots over the value
  						range using quantiles. \code{spline.knots} will only be used if \code{knots} is not given.}
  \item{knots}{An array consisting all knots (boundary knots as well as the interior knots) to be used to 
  				create the spline basis.}
  \item{spline.degree}{The polynomial degree of the spline basis.}
}

\value{
  Returns the I-spline with the used spline settings as attribute. The spline settings
                 attribute can transform the same attribute of any other objects using the same knots.  }

\references{
P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008) \emph{SVM-Maj: a majorization approah to linear support
vector machines with different hinge errors.}
J.O. Ramsay (1988) \emph{Monotone regression splines in action.} Statistical Science, 3(4):425-461
 }
\author{ Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov}
\seealso{
  \code{\link[SVMMaj]{svmmaj}}
}

\examples{

## create I-spline basis for the first 50 observations
x  <- iris$Sepal.Length
B1  <- isb(x[1:50],spline.knots=4,spline.degree=3)
## extracting the spline transformation settings
spline.param <- attr(B1,'splineInterval')
## use the same settings to apply to the next 50 observations
B2 <- isb(x[-(1:50)],spline.degree=3,knots=spline.param)

}

