\name{predict.svmmaj}
\alias{predict.svmmaj}
\alias{print.q.svmmaj}
\title{ Out-of-Sample Prediction from Unseen Data.  }
\description{
  This function predicts the predicted value (including intercept), given a previous trained
  model which has been returned by \code{\link[SVMMaj]{svmmaj}}.
}
\usage{
\method{predict}{svmmaj}(object, X.new , y=NULL, show.plot=FALSE,...)
}
                              
\arguments{
  \item{object}{ Model which has been trained beforehand using \code{\link[SVMMaj]{svmmaj}}.}
  \item{X.new}{ Attribute matrix of the objects to be predicted, which has the same number 
                of attributes as the untransformed attribute matrix in \code{model}.}
  \item{y}{ The actual class labels (only if \code{show.plot==TRUE}).}
  \item{show.plot}{ If \code{show.plot=TRUE}, it plots the density of the predicted value for
                    both class labels, if \code{y} is not specified, the density of all objects
                    will be plotted.}
  \item{...}{ Arguments to be passed to methods.}
}

\value{
  The predicted value (including intercept) of class \code{q.svmmaj}, with attributes:
  \item{y}{The observed class labels of each object.}
  \item{yhat}{ he predicted class labels of each object.}
  \item{classes}{The class labels.}
}
  
\references{ 
P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008) \emph{SVM-Maj: a majorization approah to linear support
vector machines with different hinge errors.}
}
\author{ Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov}
\seealso{ \code{\link[SVMMaj]{svmmaj}} }

\examples{
attach(AusCredit)

## model training
model <- svmmaj(X[1:400,],y[1:400],hinge='quadratic', lambda=1)
## model prediction
q4     <- predict(model,X[-(1:400),],y[-(1:400)],show.plot=TRUE)
q4
}

