#' Hinge error function of SVM-Maj
#' 
#' This function creates a function to compute the hinge error, 
#' given its predicted value \code{q} and its class \code{y}, 
#' according to the loss term of the Support Vector machine loss function.
#' @param hinge Hinge error function to be used, possible values are 
#' \code{'absolute'}, \code{'quadratic'} and \code{'huber'} 
#' @param delta The parameter of the huber hinge 
#'   (only if \code{hinge = 'huber'}).
#' @param eps  Specifies the maximum steepness of the quadratic majorization 
#'   function \code{m(q) = a * q ^ 2 -2 * b * q + c}, where 
#'   \code{a <= .25 * eps ^ -1}.
#' @return The hinge error function with arguments \code{q} and \code{y} to 
#'   compute the hinge error. The function returns a list with the parameters 
#'   of the majorization function SVM-Maj (\code{a}, \code{b} and \code{c}) 
#'   and the loss error of each object (\code{loss}).
#' @references P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008) 
#'    \emph{SVM-Maj: a majorization approach to linear support
#'    vector machines with different hinge errors.}
#' @aliases getHinge print.hinge
#' @seealso \code{\link{svmmaj}}
#' @examples
#' hingefunction <- getHinge()
#' ## plot hinge function value and, if specified, 
#' ## the majorization function at z
#' ## plot(hingefunction, z = 3)
#' ## generate loss function value
#' loss  <- hingefunction(q = -10:10, y = 1)$loss
#' print(loss)
#' plot(hingefunction, z = 3)
#' @export
#'
getHinge <- function(hinge = "quadratic", delta = 3, eps=1e-8) {
#=========================================================
#GENERATE HINGE ERROR FUNCTION

#DETERMINE THE HINGE TERM AND MAJOR PARAMETERS
  hingechoice <- switch(hinge,
    absolute = function(q, y) {
      z    <- q * y
      m    <- abs( 1 - z )
      m    <- m * ( m > eps ) + eps * ( m <= eps )
      a    <- .25 * m ^ -1
      b    <- y * ( a + .25 )
      c    <- a + .5 + .25 * m
      loss <- (1 > z) * ( 1 - z )
      list(a = a, b = b, c = c, loss = loss, call = match.call())     
    },

    quadratic = function(q, y) {
      z    <- q * y
      m    <- z * (z > 1) + (1 >= z)
      a    <- 1
      b    <- y * m
      c    <- m ^ 2
      loss <- (1 > z) * (1 - z) ^ 2
      list(a = a, b = b, c = c, loss = loss, call = match.call())
    },

    huber = function(q, y) {
      z    <- q * y
      m    <- (z < -delta) * (z + delta)
      o    <- (z >  1) * (z - 1)
      a    <- .5 * (delta + 1) ^ -1
      b    <- y * a * (1 + m + o)
      c    <- y * b * (1 + m + o) - m
      loss <- a * q ^ 2 - 2 * b * q + c
      list(a = a, b = b, c = c, loss = loss, call = match.call())
    },

    logit = function (q, y) {
      z    <- q * y
      m    <- (1 + exp(z)) ^ -1
      l    <- log(1 + exp(-z))
          l.inf    <- is.infinite(l)
          l[l.inf] <- log(1 + exp(z[l.inf])) - z[l.inf]
      a    <- .125
      b    <- y * a * (4 * m + z)
      c    <- z * a * (8 * m + z)  + l
      list(a = a, b = b, c = c, loss = l, call = match.call())   
    }
      
#    probit = function(q,y) {
#     	z    <- q * y
#     	M    <- pnorm(z)
#     	l    <- -log(M)
#     	mr   <- dnorm(z)/M
#      	mr[M==0] <- -z[M==0]
#      	l[M==0]  <- .5*z[M==0]^2
#      	a    <- .5
#      	b    <- y*(z + .5*mr)
#      	c    <- z*(2*b*y - z) + l
#      	list( a=a, b=b, c=c, loss=l, call=match.call())   },

#   regression =	function(q,y){
#       qa   <- q - y
#       m    <- pmax( abs( abs(qa) - hingek ) , eps )
#       z    <- abs( qa ) - 2 * hingek
#		    a    <- .5 / ( (z > 0) * abs(qa)  +  (z <= 0) * 2 * m )
#		    b    <- (z < 0) * a * abs(hingek - m) * sign(qa)
#		    c    <- (z < 0) * a * ( hingek - m )^2 + 
#               (z > 0) * ( .5 * abs(qa) - hingek )
#		    loss <- pmax( abs( qa ) - hingek, 0 )
#		    list( a=a , b=b , c=c , loss=loss , call=match.call() ) }
    )
  class(hingechoice)           <- "hinge"
  attr(hingechoice, "hinge")   <- hinge
  attr(hingechoice, "fixed.a") <- !(hinge %in% c("absolute", "regression"))

  if (hinge %in% c("huber",   "regression")) 
    attr(hingechoice, "delta") <- delta
  if (hinge %in% c("absolute", "regression"))
    attr(hingechoice, "eps")    <- eps

  return (hingechoice)
}

#' @export
print.hinge <- function(x, ...) {
	cat("Hinge function\n", "  Hinge type:", attr(x,"hinge"), "\n")
	if (!is.null(attr(x, "delta"))) {
	  cat("  Hinge parameters:\n", "k = ", attr(x, "delta"), "\n")
	}
}


#' Plot the hinge function
#' 
#' This function plots the hinge object created by \code{getHinge}.
#' 
#' @param x The hinge object returned from \code{getHinge}.
#' @param y Specifies the class (\code{-1} or \code{1}) 
#'    to be plotted for the hinge error.
#' @param z If specified, the majorization function with the supporting point 
#'          \code{z} will also be plotted.
#' @param ... Other arguments passed to plot method.
#' @examples
#' hingefunction <- getHinge()
#' ## plot hinge function value
#' plot(hingefunction, z = 3)
#' @importFrom graphics lines
#' @importFrom graphics points
#' @method plot hinge
#' @importFrom graphics plot
#' @export
plot.hinge <- function(x, y = 1, z = NULL, ...) {
	object <- x
	#DEFINE PLOT RANGE
	if (!is.null(z))  scale <- max(abs(z) * 1.2, 5)
	else              scale <- 5
	#CALCULATE LOSS FUNCTION
	xs    <- seq(-1, 1, by = .02) * scale
	loss <- object(xs, y)$loss
	plot(
    x = xs, y = loss, type = "l", 
    main = "Plot of hinge error", xlab = "q", ylab = "hinge value", ...
  )
	#CALCULATE, IF SPECIFIED MAJORIZATION FUNCTION
	if (!is.null(z)){
	  hingechoice <- object(z,y)
	  hingeVal    <- 
      hingechoice$a * xs ^ 2 - 2 * hingechoice$b * xs + hingechoice$c
	  lines(xs, hingeVal, col = "blue")
	  points(z, hingechoice$loss)
	}
}

