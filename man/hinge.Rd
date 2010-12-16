\name{hinge}
\alias{getHinge}
\alias{print.hinge}
\alias{plot.hinge}
\title{ Hinge error function of SVM-Maj }
\description{
  This function creates a function to compute the hinge error, given its predicted value \code{q} and
  its class \code{y}, according to the loss term of the Support Vector machine loss function.
}
\usage{
getHinge(hinge = 'quadratic', hingek = 3, eps= 1e-8)
\method{plot}{hinge}(x, y=1, z=NULL,...)
}

\arguments{
  \item{hinge}{ Hinge error function to be used, possible values are \code{'absolute'},
                \code{'quadratic'} and \code{'huber'}  }
  \item{hingek}{ The parameter of the huber hinge (only if \code{hinge = 'huber'}).}
  \item{eps}{ Specifies the maximum steepness of the quadratic majorization function
              \code{m(q) = a*q^2 -2*b*q + c}, where \code{a <= .25* eps^-1}.}
  \item{x}{ The hinge object returned from \code{getHinge}.}
  \item{y}{ Specifies the class (\code{-1} or \code{1}) to be plotted for the hinge error.}
  \item{z}{ If specified, the majorization function with the supporting point \code{z} will also be plotted.}
  \item{...}{Other arguments passed to plot method. }
}

\value{
  The hinge error function with arguments \code{q} and \code{y} to compute the hinge error.
  The function returns a list with the parameters of the majorization function SVM-Maj
  (\code{a}, \code{b} and \code{c}) and the loss error of each object (\code{loss}).
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
hingefunction <- getHinge()
## plot hinge function value and, if specified, the majorization function at z
plot(hingefunction,z=3)
## generate loss function value
loss <- hingefunction(q = -10:10 ,y = 1)$loss
loss

}

