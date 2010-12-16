\name{normalize}
\alias{normalize}
\title{ Normalize/standardize the colums of a matrix}
\description{
  Standardize the columns of an attribute matrix \code{X} to zscores, to the range \code{[0 1]} or a prespecified scale.
}                                           
\usage{
normalize(x,standardize = 'zscore')
}

\arguments{
  \item{x}{An attribute variable which will be scaled.}
  \item{standardize}{Either a string value denoting a predefined scaling, or a list with values \code{a} and
                    \code{b} corresponding with the numeric centering and scaling, that is, using the function 
                    \code{x * standardize$b - standardize$a}.}
}

\value{
  The standardized matrix. The numeric centering and scalings used 
  are returned as attribute \code{"standardize"}.
  }

\references{
P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008) \emph{SVM-Maj: a majorization approah to linear support
vector machines with different hinge errors.}
}
\author{ Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov}
\seealso{
  \code{\link[SVMMaj]{svmmaj}}
}

\examples{
## standardize the first 50 objects to zscores
x  <- iris$Sepal.Length
x1 <- normalize(x[1:50],standardize='zscore')
## use the same settings to apply to the next 100 observations
x2 <- normalize(x[-(1:50)],standardize=attr(x1,'standardization'))

}

