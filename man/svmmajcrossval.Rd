\name{svmmajcrossval}
\alias{svmmajcrossval}
\title{ k-fold Cross-Validation of SVM-Maj  }
\description{
  This function performs a gridsearch of k-fold crossvalidations using SVM-Maj and returns
  the combination of input values which has the best forecasting performance.
}
\usage{
svmmajcrossval(X,y,search.grid=list(lambda=2^seq(5,-5,length.out=19)),...,convergence=1e-4, 
                  weights.obs=1, check.positive = TRUE, print.progress=FALSE, ngroup = 5, groups = NULL)
}

\arguments{
  \item{X}{ A data frame (or object coercible by \code{\link[base]{as.data.frame}}
                      to a data frame)  consisting the attribues. }
  \item{y}{ A factor (or object coercible by \code{\link[base]{factor}} to a factor) consisting the
                      class labels. }
  \item{search.grid}{ A list with for each factor the range of values to search for.}
  \item{...}{ Other arguments to be passed through \code{svmmaj}.}
  \item{convergence}{ Specifies the convergence criterion for \code{svmmaj}. Default is \code{1e-08}. }
  \item{weights.obs}{ Weigths for the classes.}
  \item{check.positive}{ Specifies whether a check should be performed for positive
                         \code{lambda} and \code{weights.obs}.}
  \item{print.progress}{ \code{=TRUE} shows the progress of the cross-validation.} 
  \item{ngroup}{ The number of groups to be divided into.}
  \item{groups}{ A predetermined group division for performing the cross validation.}

}

\value{
  \item{missclass.opt}{ The minimum (weighted) missclassification rate found in out-of-sample training along the search grid. }
  \item{param.opt}{ The level of the factors which gives the minimum loss term value.}
  \item{param.grid}{ The matrix of all gridpoints which has been performed during the cross-validation,
                     with its corresponding weighted out-of-sample missclassification rate.}
  }

\references{ 
P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008) \emph{SVM-Maj: a majorization approach to linear support
vector machines with different hinge errors.}
}
\author{ Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov}
\seealso{ 
  \code{\link[SVMMaj]{svmmaj}}
}

\examples{
require(MASS)
Xt <- subset(Pima.tr,select=-type)
yt <- Pima.tr$type

## setting grid range
search.grid= list(lambda=10^seq(1,-1) ,kernel.sigma=2^seq(0,2) )

## performing gridsearch with k-fold crossvalidation
results<-svmmajcrossval(Xt,yt,search.grid = search.grid,
standardize = 'interval',kernel = rbfdot,hinge='quadratic')
results

}

