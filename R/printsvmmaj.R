#' Print Svmmaj class
#' 
#' Trained SVM model as output from \code{\link[SVMMaj]{svmmaj}}. 
#' The returning object consist of the following values: 
#' \describe{
#'   \item{call}{ The function specifications which has been called.}
#'   \item{lambda}{ The regularization parameter of the penalty 
#'        term which has been used.} 
#'   \item{loss}{ The corresponding loss function
#'        value of the final solution.} 
#'   \item{iteration}{ Number of iterations needed
#'        to evaluate the algorithm.}
#'   \item{X}{ The attribute matrix of \code{dim(X) = c(n,k)}.} 
#'   \item{y}{ The vector of length \code{n} with the actual class labels. 
#'        These labels can be numeric \code{[0 1]} or two strings.} 
#'   \item{classes}{ A vector of length \code{n} with the predicted 
#'        class labels of each object, derived from q.tilde}
#'    \item{Xtrans}{ The attribute matrix \code{X} after standardization and 
#'        (if specified) spline transformation.} 
#'    \item{norm.param}{ The applied normalization parameters 
#'        (see \code{\link[SVMMaj]{normalize}}).}
#'    \item{splineInterval}{ The spline knots which has been used 
#'        (see \code{\link[SVMMaj]{isb}}).} 
#'    \item{splineLength}{Denotes the number of spline basis of
#'        each explanatory variable in \code{X}.}
#'    \item{method}{The decomposition matrices used in estimating the model.}
#'    \item{hinge}{ The hinge function which has been used 
#'        (see \code{\link[SVMMaj]{getHinge}}).}
#'    \item{beta }{If identified, the beta parameters for the linear combination
#'        (only available for linear kernel).}
#'    \item{q}{ A vector of length \code{n} with predicted values of 
#'        each object including the intercept.} 
#'    \item{nSV}{ Number of support vectors.}
#'    }
#' @param x the \code{svmmaj} object as result of \code{\link[SVMMaj]{svmmaj}}
#' @param ... further arguments passed to or from other methods.
#' @export
print.svmmaj <- function(x, ...)
  with(x,cat('Model:\n',
             '   update method                     ', method$type, '\n',
             '   attribute dimension               ', dim(method$X), '\n',
             '   degrees of freedom                ', method$x, '\n',
             '   number of iterations              ', iteration, '\n',
             '   loss value                        ', loss, '\n',
             '   number of support vectors         ', nSV, '\n'
  ))       

#' @rdname print.svmmaj
#' @param object the \code{svmmaj} object as result of \code{\link[SVMMaj]{svmmaj}}
#' @export
summary.svmmaj <- function(object,...)
  structure(object, class = 'summary.svmmaj')

#' @rdname print.svmmaj
#' @export
print.summary.svmmaj <- function(x,...) {
  object <- x
  cat('Call:\n   ')
  print(object$call)
  #spline info
  splineb <- 'no'
  if(!with(object, splineKnots == 0 && splineDegree == 1)) 
    splineb <- with(object, paste(
      'knots =' , splineKnots , 
      ',degree =', splineDegree
    ))
  #kernel info     
  typek  <- attr(object$method$kernel, 'class')[1]
  if(typek == 'vanillakernel')
    typek <- 'linear'
  #print settings   
  cat('\nSettings:\n',
      '   lambda                            ', object$lambda, '\n',
      '   hinge error                       ', attr(object$hinge,'hinge'), '\n',
      '   spline basis                      ', splineb, '\n',
      '   type of kernel                    ', typek, '\n')
  #print kernel          
  if(typek!='linear'){
    cat('    parameters of kernel               ') 
    kernel.param <- attr(object$method$kernel, 'kpar')     
    sapply(1:length(kernel.param), function(i) 
      cat(names(kernel.param)[i], '=', kernel.param[[i]], ' ')
    )}
  #print data
  cat('\nData:\n',
      '   class labels                      ', object$classes, '\n',
      '   rank of X                         ', object$method$x, '\n',
      '   number of predictor variables     ', NCOL(object$data) - 1, '\n',
      '   number of objects                 ', NROW(object$data), '\n',
      '   omitted objects                   ', length(object$na.output), '\n\n'
  )
  #print result
  cat('Model:\n',
      '   update method                     ', object$method$type, '\n',
      '   number of iterations              ', object$iteration, '\n',
      '   loss value                        ', object$loss, '\n',
      '   number of support vectors         ', object$nSV, '\n'
  )   
  prints <- with(object, classification(
    q = q, y = na.omit(data)[, 1], 
    classes = classes, weights = weights.obs
  ))
  
  #print prediction results 
  cat('\nConfusion matrix:\n')
  print(prints$matrix)  
  cat('\nClassification Measures:\n')
  print(prints$overall, digits = 3)      
  cat('\n')                          
  print(prints$measures, digits = 3)
}

#' @rdname svmmaj
#' @param x the \code{svmmaj} object as result of \code{\link[SVMMaj]{svmmaj}}
#' @export
print.q.svmmaj <- function(x, ...) {
  q <- x
  classnames   <- attr(q, 'classes')
  classcount  <- matrix(
    data = c(sum(q < 0), sum(q >= 0)), nrow = 1,
    dimnames = list(list('frequency'), as.list(classnames))
  )
  
  cat('Prediction frequencies:\n')
  print(classcount)
  
  if(!is.null(attr(q,'y'))){
    prints <- classification(
      q = q, y = attr(q,'y'),
      classes = classnames, weights = attr(q,'weights')
    )
    
    cat('\nConfusion matrix:\n')
    print(prints$matrix)
    cat('\nClassification Measures:\n')
    print(prints$overall, digits = 3)
    cat('\n')                                    
    print(prints$measures, digits = 3)
  }
}

#' @rdname print.svmmaj
#' @method plot svmmaj
#' @export
plot.svmmaj <- function(x, ...) 
  {
  object <- x
  classnames <- object$classes
  
  obj  <- data.frame(q = unclass(object$q), Class = object$y)
  splitted <- split(obj$q, obj$Class)
  hitrate <- (sum(splitted[[1]] < 0) + sum(splitted[[2]] > 0)) / 
    sum(sapply(splitted, length))
  
  obj$Class <- factor(obj$Class)
  levels(obj$Class) <- paste0(classnames, " (", levels(obj$Class), ")")
  
  ggplot(obj, aes(
    x = .data$q, y = after_stat(count), color = .data$Class, fill = .data$Class)) + 
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = 'longdash', color = '#8A8A8A', size = 1.0) +
    theme_light() + 
    xlab(expression(widetilde(q))) +
    ggtitle(
      sprintf("Density of predicted values\n hit rate = %4.2f%%", 
              hitrate * 100))
}
