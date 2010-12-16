\name{svmmaj}
\alias{svmmaj}
\alias{svmmaj.default}
\alias{plot.svmmaj}
\alias{plotWeights}
\alias{print.svmmaj}
\alias{print.summary.svmmaj}
\alias{summary.svmmaj}
\title{ SVM-Maj Algorithm  }
\description{                                        
  SVM-Maj is an algorithm to compute a support vector machine (SVM) solution. 
  In its most simple form, it aims at finding hyperplane that optimally separates two given classes.
  This objective is equivalent to finding a linear combination of \code{k} predictor variables 
  to predict the two classes for \code{n} observations. 
  SVM-Maj minimizes the standard support vector machine (SVM) loss function.
  The algorithm uses three efficient updates for three different situations: 
  primal method which is efficient in the case of \code{n>k}, 
  the decomposition method, used when the matrix of predictor variables is not of full rank, 
  and a dual method, that is efficient when \code{n<k}. 
  Apart from the standard absolute hinge error, SVM-Maj can also handle the
  quadratic and the Huber hinge.
}
\usage{
svmmaj(X,y,lambda=1,...)
\method{svmmaj}{default}(X, y, lambda=1, weights.obs = 1, weights.var= 1, scale = 'none',
spline.knots=0L, spline.degree=1L, kernel=vanilladot, kernel.sigma=1, kernel.degree=1,
kernel.scale=1, kernel.offset=0, hinge='absolute', hinge.k=5, convergence=1e-8,
print.step=FALSE, initial.point=NULL, increase.step = 20, eps=1e-8,
check.positive=TRUE,na.action=na.omit,...)

plotWeights(object,plotdim=c(3,3))
\method{plot}{svmmaj}(x,titletext=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{           A data frame (or object coercible by \code{\link[base]{as.data.frame}}
                      to a data frame)  consisting the attribues, the class of each attribute can be either \code{numeric}, 
                      \code{logical} or \code{factor}. }
  \item{y}{           A factor (or object coercible by \code{\link[base]{factor}} to a factor) consisting the
                      class labels. }
% \item{model}{       An object of class "\code{\link[stats]{formula}}" (or one that can
%                     be coerced to that class): a symbolic description of the model
%                     to be fitted. The details of model specification are given
%                     under 'Details'.}
% \item{data}{        An optional data frame, list or environment (or object coercible
%                     by \code{\link[base]{as.data.frame}} to a data frame) containing
%                     the variables in the model. If not found in \code{data}, the variables
%                     are taken from \code{environment(svmmaj)}, typically the
%                     environment from which \code{svmmaj.formula} is called.}
  \item{lambda}{      Regularization parameter of the penalty term. }
  \item{weights.obs}{ Vector of length \code{n} with the nonnegative weight for the 
                      residual of each object (with length \code{n}).
                      If the length is \code{2}, then it specifies the weight per class. }
  \item{weights.var}{ Vector of length \code{k} with weights for each attribute. }
  \item{scale}{       Specifies whether the columns of attribute matrix \code{X}
                      needs to be standardized into zscores or to the interval \code{[0 1]}. 
                      Possible values are:
                      \code{none}, \code{zscore} and \code{interval}.
                      Moreover, the standardization parameters can be given instead. }
  \item{spline.knots}{ Number of internal knots of the spline basis. When the number of knots exceeds the number of
                       (categorical) values of an explanatory variable, the duplicate knots will be removed using
                       \code{link[base]{unique}}. \code{spline.knots = 0} and \code{spline.degree=1}
                       in case of no splines.}
  \item{spline.degree}{ The polynomial degree of the splines, for no splines: 
                       \code{spline.knots = 0} and \code{spline.degree=1}.}
  \item{kernel}{ Specifies which kernel function to be used (see \code{\link[kernlab]{dots}} of
                  package \pkg{kernlab}). Default kernel is the linear kernel .}
  \item{kernel.sigma}{ The kernel parameter \code{sigma} for the RBF kernel (see \code{\link[kernlab]{rbfdot}}).
						Default value is 1.}
  \item{kernel.degree}{ The degree parameter of the polynomial kernel (see \code{\link[kernlab]{polydot}}).}
  \item{kernel.scale}{ The scale parameter of the polynomial kernel (see \code{\link[kernlab]{polydot}}).}
  \item{kernel.offset}{ The offset used in a polynomial kernel (\code{\link[kernlab]{polydot}}).}
  \item{hinge}{ Specifies which hinge term to be used. Possible values are:
                \code{absolute}, \code{quadratic}, \code{huber}. }
  \item{hinge.k}{ The parameter of the huber hinge, if the huber hinge is used. }
  \item{convergence}{ Specifies the convergence criterion of the algorithm. Default is \code{1e-08}. }
  \item{print.step}{ \code{print.step=TRUE} shows the progress of the iteration.  }
  \item{initial.point}{ Initial solution.}
  \item{increase.step}{ The iteration number from which relaxed update will be used.}
  \item{eps}{ The relaxation of the majorization function for absolute hinge: \code{.25 * eps^-1} is the
  maximum steepness of the majorization function.}
  \item{check.positive}{ Specifies whether a check has to be made for positive input values.}
  \item{na.action}{ Generic function for handling NA values.}
  
  \item{object}{ The model returned from \code{svmmaj}. }
  \item{x}{ The model returned from \code{svmmaj} used to plot the distribution of the
            objects per class using \code{plot.svmmaj}.}
  \item{titletext}{ An optional title for \code{plot.svmmaj}.}
  \item{plotdim}{ A vector of the form \code{c(nr, nc)}. Subsequent figures will be drawn 
              in an \code{nr}-by-\code{nc} array on the device.}
  \item{...}{Other arguments passed to methods.}
}

\value{
  Returns a \code{svmmaj}-class, of which the methods \code{plot}, \code{plotWeights}, \code{summary} and \code{predict} can be applied.
  (see also \code{\link[SVMMaj]{predict.svmmaj}})
  The returning object consist of the following values:
  \item{call}{ The function specifications which has been called.}
  \item{lambda}{ The regularization parameter of the penalty term which has been used.}
  \item{loss}{ The corresponding loss function value of the final solution.}
  \item{iteration}{ Number of iterations needed to evaluate the algorithm.}  

  \item{X}{ The attribute matrix of \code{dim(X) = c(n,k)}.}
  \item{y}{ The vector of length \code{n} with the actual class labels. These labels can be numeric \code{[0 1]} or
          two strings.}
  \item{classes}{ A vector of length \code{n} with the predicted class labels of each object, derived from q.tilde}

  \item{Xtrans}{ The attribute matrix \code{X} after standardization and (if specified) spline transformation.}
  \item{norm.param}{ The applied normalization parameters (see \code{\link[SVMMaj]{normalize}}).}
  \item{splineInterval}{ The spline knots which has been used (see \code{\link[SVMMaj]{isb}}).}
  \item{splineLength}{Denotes the number of spline basis of each explanatory variable in \code{X}.}
  
  \item{method}{ The decomposition matrices used in estimating the model.}
  \item{hinge}{ The hinge function which has been used (see \code{\link[SVMMaj]{hinge}}).}

  \item{beta }{ If identified, the beta parameters for the linear combination (only available
                for linear kernel).}

  \item{q}{ A vector of length \code{n} with predicted values of each object including the intercept.}
  \item{nSV}{ Number of support vectors.}
  \code{plotWeights} shows, one graph per attribute, the weights of all attributes. The type of graph depends on the
  type of the attribute: the spline line of the corresponding attribute in case a spline has been used, a bar plot for
  categorical and logical values, and a linear line for all other type of the attribute values. This function cannot be
  used in a model with a non-linear kernel.
}
\references{
P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008) \emph{SVM-Maj: a majorization approah to linear support
vector machines with different hinge errors.}
 }
\author{ Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov}

\seealso{ \code{\link[kernlab]{dots}} for the computations of the kernels.
\code{\link[SVMMaj]{predict.svmmaj}}
\code{\link[SVMMaj]{normalize}}
\code{\link[SVMMaj]{isb}}
\code{\link[SVMMaj]{hinge}}
}
\examples{
attach(diabetes)

## using default settings
model1 <- model <- svmmaj(X,y,hinge='quadratic', lambda=1)
summary(model1)

weights.obs = list(positive=2,negative=1)
## using radial basis kernel
model2 <- svmmaj(X,y,hinge='quadratic', lambda=1, weights.obs=weights.obs,
              standardize='interval',kernel=rbfdot,
              kernel.sigma=1)
summary(model2)
## I-spline basis
model3 <- svmmaj(X,y,,weights.obs=weights.obs,spline.knots=3,spline.degree=2)
plotWeights(model3,plotdim=c(2,4))
}

