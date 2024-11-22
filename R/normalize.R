#' Normalize/standardize the columns of a matrix
#'
#' Standardize the columns of an attribute matrix \code{X} to zscores, to the
#' range \code{[0 1]} or a prespecified scale.
#'
#'
#' @param x An attribute variable which will be scaled.
#' @param standardize Either a string value denoting a predefined scaling, or a
#' list with values \code{a} and \code{b} corresponding with the numeric
#' centering and scaling, that is, using the function \code{x * standardize$b -
#' standardize$a}.
#' @return The standardized matrix. The numeric centering and scalings used are
#' returned as attribute \code{"standardize"}.
#' @author Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov
#' @seealso \code{\link[SVMMaj]{svmmaj}}
#' @references P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008)
#' \emph{SVM-Maj: a majorization approach to linear support vector machines with
#' different hinge errors.}
#' @examples
#'
#' ## standardize the first 50 objects to zscores
#' x <- iris$Sepal.Length
#' x1 <- normalize(x[1:50], standardize = "zscore")
#' ## use the same settings to apply to the next 100 observations
#' x2 <- normalize(x[-(1:50)], standardize = attr(x1, "standardization"))
#' @importFrom stats sd
#' @export
normalize <- function(x, standardize = "zscore") {
  # ===================================================================
  # STANDARDIZE THE ATTRIBUTE MATRIX X
  #-------------------------------------------------------------------
  if (is.list(standardize)) {
    stand.a <- standardize$a
    stand.b <- standardize$b
  } else if (standardize == "zscore") {
    stand.a <- mean(x)
    stand.b <- sd(x)
    stand.b[stand.b == 0] <- 1
  } else if (standardize == "interval") {
    stand.a <- min(x)
    stand.b <- max(x) - stand.a
  } else {
    stand.a <- 0
    stand.b <- 1
  }
  if (is.na(stand.b) || stand.b == 0) {
    stand.a <- mean(x)
    stand.b <- 1
  }

  X <- (x - stand.a) / stand.b
  attr(X, "standardization") <- list(a = stand.a, b = stand.b)
  return(X)
}


#' Transform the data with normalization and/or spline basis
#'
#' Performs subsequently a normalization of the input data and
#' creating spline basis based on the user defined input
#'
#' @param x a single column of values as input for the data
#' transformation
#' @param standardize Either a string value denoting a predefined scaling, or a
#' list with values \code{a} and \code{b} corresponding with the numeric
#' centering and scaling, that is, using the function
#' \code{x * standardize$b - standardize$a}.
#' @param spline.knots Number of inner knots to use. \code{isb} will equally
#' distribute the knots over the value range using quantiles.
#' \code{spline.knots} will only be used if \code{knots} is not given.
#' @param spline.degree The polynomial degree of the spline basis.
#' @importFrom stats model.matrix
#' @return transformed data in spline basis or (in case of no spline)
#'   a normalized vector
transformdata <- function(
    x,
    standardize = c("interval", "zscore", "none"),
    spline.knots = 0, spline.degree = 1) {
  standardize <- match.arg(
    standardize,
    c("interval", "zscore", "none"),
    several.ok = TRUE
  )[1]

  # nominal values --> binary columns
  if (is.factor(x)) {
    return(structure(model.matrix(~ x - 1), values = levels(x)))
  }
  # numeric values --> splines or normalized values
  if (is.numeric(x)) {
    if (spline.knots != 0 || spline.degree > 1) {
      return(isb(x, spline.knots = spline.knots, spline.degree = spline.degree))
    } else {
      return(structure(normalize(x, standardize),
        dim = c(length(x), 1)
      ))
    }
  }

  # logical values --> binary values
  if (is.logical(x)) {
    return(structure(as.numeric(x),
      type = "logical",
      dim = c(length(x), 1)
    ))
  }

  # other values --> try to coerce into numeric values
  return(structure(as.numeric(x),
    dim = c(length(x), 1)
  ))
}

#' Perform the transformation based on predefined settings
#'
#' Given the input parameters, which are generated from
#' \code{transformdata}, it performs the same transformation
#' with the same settings to the given input
#' @param x a (new) vector of numerics to be transformed
#' @param attrib either a list of settings, or \code{NULL} in case
#'         the attributes are given as separate input
#' @param values a vector of levels in case \code{x} is a factor
#' @param standardization the standardization rules from \code{normalize}
#' @param splineInterval the knots to be used for spline basis
#' @param splineDegree the polynomial degree of the splines
#' @importFrom stats model.matrix
#' @return a transformed data based on the user defined settings
predict.transDat <- function(
    x,
    attrib = NULL, values = NULL, standardization = NULL,
    splineInterval = NULL, splineDegree = NULL) {
  if (is.list(attrib)) {
    values <- attrib$values
    standardization <- attrib$standardization
    splineInterval <- attrib$splineInterval
    splineDegree <- attrib$splineDegree
  }
  # TRANSFORM AN ARRAY WITH PRESPECIFIED SETTINGS,
  # IN PARTICULAR FROM TRANSFORMDATA
  if (!is.null(splineInterval)) {
    return(isb(x, knots = splineInterval, spline.degree = splineDegree))
  } else if (!is.null(standardization)) {
    I <- normalize(x, standardize = standardization)
    attr(I, "dim") <- c(length(x), 1)
    return(I)
  } else if (!is.null(values)) {
    x <- factor(x, levels = values)
    I <- model.matrix(~ x - 1)
    attr(I, "values") <- values
    return(I)
  } else {
    I <- as.numeric(x)
    attr(I, "dim") <- c(length(x), 1)
    return(I)
  }
}
