#' I-spline basis of each column of a given matrix
#'
#' Create a I-spline basis for an array. \code{isb} will equally distribute the
#' knots over the value range using quantiles.
#'
#' @param x The predictor variable, which will be transformed into I-spline
#' basis.
#' @param spline.knots Number of inner knots to use. \code{isb} will equally
#' distribute the knots over the value range using quantiles.
#' \code{spline.knots} will only be used if \code{knots} is not given.
#' @param knots An array consisting all knots (boundary knots as well as the
#' interior knots) to be used to create the spline basis.
#' @param spline.degree The polynomial degree of the spline basis.
#' @return The I-spline with the used spline settings as attribute. The
#' spline settings attribute can transform the same attribute of any other
#' objects using the same knots.
#' @author Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov
#' @seealso \code{\link[SVMMaj]{svmmaj}}
#' @references
#' P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008)
#' \emph{SVM-Maj: a majorization approach to linear support vector machines with
#' different hinge errors.}
#'
#' J.O. Ramsay (1988) \emph{Monotone regression
#' splines in action.} Statistical Science, 3(4):425-461
#'
#' @examples
#' ## plot the spline transformation given a monotone sequence
#' B0 <- isb(0:100, spline.knots = 2, spline.degree = 3)
#' plot(NULL, xlim = c(0, 140), ylim = c(0, 1), xlab = "x", ylab = "I-spline")
#' for (i in 1:ncol(B0)) {
#'   lines(B0[, i], col = i, lwd = 3)
#' }
#' legend("bottomright",
#'   legend = 1:ncol(B0), col = 1:ncol(B0),
#'   lty = 1, lwd = 3, title = "Spline Columns"
#' )
#' ## create I-spline basis for the first 50 observations
#' x <- iris$Sepal.Length
#' B1 <- isb(x[1:50], spline.knots = 4, spline.degree = 3)
#' ## extracting the spline transformation settings
#' spline.param <- attr(B1, "splineInterval")
#' ## use the same settings to apply to the next 50 observations
#' B2 <- isb(x[-(1:50)], spline.degree = 3, knots = spline.param)
#' @export
#'
isb <- function(x, spline.knots = 0, knots = NULL, spline.degree = 1) {
  # DEFINES THE KNOTS (AND REMOVE DUPLICATE VALUES)
  if (is.null(knots)) {
    quantiles <- seq(0, 1, length = spline.knots + 2)
    if (length(x[x == 0]) != 0) {
      interval <- quantile(c(0, x[x != 0]), probs = quantiles, names = FALSE)
    } else {
      interval <- quantile(x, probs = quantiles, names = FALSE)
    }

    interval <- quantile(c(0, x[x != 0]), probs = quantiles, names = FALSE)
    knots <- unique(interval)
  }

  if (length(knots) <= 2) {
    # NORMALIZE X IN CASE OF NO INTERIOR KNOTS
    I <- structure(normalize(x, standardize = "interval"),
      dim = c(length(x), 1)
    )
    return(I)
  } else {
    # COMPUTE SPLINE BASIS
    spline.degree <- min(length(knots) - 1, spline.degree)
    I <- structure(isplinebasis(x, knots, spline.degree),
      splineInterval = knots, splineDegree = spline.degree
    )
    return(I)
  }
}

#' Transform a given data into I-splines
#'
#' Inner function call to create I-splines based on the
#' user defined \code{knots}  and polynomial degree \code{d} of the splines
#'
#' @param x a scalar or vector of values which will be transformed into splines
#' @param knots a vector of knot values of the splines
#' @param d the polynomial degree of the splines
#' @return a matrix with for each value of x the corresponding spline values.
isplinebasis <- function(x, knots, d) {
  if (
    is.null(knots) || any(is.na(knots)) ||
      any(diff(knots) == 0) || length(knots) <= 2
  ) {
    return(x)
  }
  m <- length(knots)
  n <- length(x)

  interval <- findInterval(x, knots, all.inside = TRUE)
  M <- sapply(sequence(m - 1), `==`, interval)

  for (i in 2:(d + 1)) {
    # CREATE M-SPLINE
    tik <- c(knots[-1], rep(knots[m], i - 2))
    ti <- c(rep(knots[1], i - 2), knots[-m])
    M <- M %*% diag(1 / (tik - ti))

    Dx <- Dt <- array(0, dim = c(m + i - 3, m + i - 2))
    Dx[1L + 0L:(m + i - 4L) * (m + i - 2L)] <- -1
    Dx[1L:(m + i - 3L) * (m + i - 2L)] <- 1

    Dt[1L + 0L:(m + i - 4L) * (m + i - 2L)] <- tik
    Dt[1L:(m + i - 3L) * (m + i - 2L)] <- -ti

    M <- (M * x) %*% Dx + M %*% Dt
  }

  # REMOVE INTERCEPT
  M <- M[, -1]

  # CREATE I-SPLINE
  S <- array(1, dim = rep(NCOL(M), 2))
  S[upper.tri(S)] <- 0
  I <- M %*% S

  return(I)
}
