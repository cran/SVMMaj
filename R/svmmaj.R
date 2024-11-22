#' SVM-Maj Algorithm
#'
#' SVM-Maj is an algorithm to compute a support vector machine (SVM) solution.
#' In its most simple form, it aims at finding hyperplane that optimally
#' separates two given classes.  This objective is equivalent to finding a
#' linear combination of \code{k} predictor variables to predict the two
#' classes for \code{n} observations.  SVM-Maj minimizes the standard support
#' vector machine (SVM) loss function.  The algorithm uses three efficient
#' updates for three different situations: primal method which is efficient in
#' the case of \code{n > k}, the decomposition method, used when the matrix of
#' predictor variables is not of full rank, and a dual method, that is
#' efficient when \code{n < k}.  Apart from the standard absolute hinge error,
#' SVM-Maj can also handle the quadratic and the Huber hinge.
#'
#' @param X A data frame (or object coercible by
#' \code{\link[base]{as.data.frame}} to a data frame) consisting the attributes,
#' the class of each attribute can be either \code{numeric}, \code{logical} or
#' \code{factor}.
#' @param y A factor (or object coercible by \code{\link[base]{factor}} to a
#' factor) consisting the class labels.
#' @param lambda Regularization parameter of the penalty term.
#' @param weights.obs a vector of length \code{n} with the nonnegative weight
#'  for the residual of each object (with length \code{n}).  If the length is
#'   \code{2}, then it specifies the weight per class.
#' @param weights.var a vector of length \code{k} with weights for each
#'   attribute.
#' @param scale Specifies whether the columns of attribute matrix \code{X}
#' needs to be standardized into zscores or to the interval \code{[0 1]}.
#' Possible values are: \code{none}, \code{zscore} and \code{interval}.
#' Moreover, the standardization parameters can be given instead.
#' @param spline.knots equals the number of internal knots of the spline basis.
#'    When the number of knots exceeds the number of (categorical) values of
#'    an explanatory variable, the duplicate knots will be removed using
#'  \code{\link[base]{unique}}. For no splines, use \code{spline.knots = 0}.
#' @param spline.degree equals the polynomial degree of the splines,
#'    for no splines:\code{spline.degree = 1}.
#' @param kernel Specifies which kernel function to be used (see
#'  \code{\link[kernlab]{dots}} of package \pkg{kernlab}).
#'  Default kernel is the linear kernel.
#' @param kernel.sigma additional parameters used for the kernel function
#'   (see \code{\link[kernlab]{dots}})
#' @param kernel.scale additional parameters used for the kernel function
#'   (see \code{\link[kernlab]{dots}})
#' @param kernel.degree additional parameters used for the kernel function
#'   (see \code{\link[kernlab]{dots}})
#' @param kernel.offset additional parameters used for the kernel function
#'   (see \code{\link[kernlab]{dots}})
#' @param hinge Specifies with hinge function from
#'  \code{\link[SVMMaj]{getHinge}} should be used.
#' @param hinge.delta The parameter of the huber hinge
#'  (only if \code{hinge = 'huber'}).
#' @param verbose \code{TRUE} shows the progress of the
#' iteration.
#' @param initial.point Initial solution.
#' @param na.action Generic function for handling NA values.
#' @param options additional settings used in the \code{svmmaj} algorithm
#' @param ... Other arguments passed to methods.
#' @return Returns a svmmaj-class object,
#'   of which the methods \code{plot},
#'   \code{plotWeights}, \code{summary} and \code{predict} can be applied.
#'   (see also \code{\link[SVMMaj]{predict.svmmaj}} and
#'   \code{\link[SVMMaj]{print.svmmaj}})
#' @author Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov
#' @seealso
#'      \code{\link[kernlab]{dots}} for the computations of the kernels.
#'      \code{\link[SVMMaj]{predict.svmmaj}}
#'      \code{\link[SVMMaj]{normalize}}
#'      \code{\link[SVMMaj]{isb}}
#'      \code{\link[SVMMaj]{getHinge}}
#' @references
#'      P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008)
#'      \emph{SVM-Maj: a majorization approach to linear support vector machines
#'      with different hinge errors.}
#' @details
#' The following settings can be added as element in the \code{options}
#' parameter:
#' \code{decomposition} Specifies whether the QR decomposition should be used
#' for efficient updates. Possible values are \code{'svd'} for Singular value
#' decomposition (Eigenvalue decomposition for non-linear kernel) or
#' \code{'chol'} for Cholesky (or QR decomposition in case of linear kernel)
#'
#' \code{convergence} Specifies the convergence criterion of the algorithm.
#' Default is \code{1e-08}.
#' \code{increase.step} The iteration number from which relaxed update will be
#' used.
#' \code{eps} The relaxation of the majorization function for absolute hinge:
#' \code{.25 * eps^-1} is the maximum steepness of the majorization function.
#'
#' \code{check.positive} Specifies whether a check has to be made for positive
#' input values.
#' \code{max.iter} maximum number of iterations to use
#'
#' @examples
#'
#' ## using default settings
#' model1 <- svmmaj(
#'   diabetes$X, diabetes$y,
#'   hinge = "quadratic", lambda = 1
#' )
#' summary(model1)
#'
#' weights.obs <- list(positive = 2, negative = 1)
#' ## using radial basis kernel
#' library(kernlab)
#' model2 <- svmmaj(
#'   diabetes$X, diabetes$y,
#'   hinge = "quadratic", lambda = 1,
#'   weights.obs = weights.obs, scale = "interval",
#'   kernel = rbfdot,
#'   kernel.sigma = 1
#' )
#' summary(model2)
#' ## I-spline basis
#' library(ggplot2)
#' model3 <- svmmaj(
#'   diabetes$X, diabetes$y,
#'   weight.obs = weight.obs,
#'   spline.knots = 3, spline.degree = 2
#' )
#' plotWeights(model3, plotdim = c(2, 4))
#' @importFrom stats na.omit
#' @importFrom kernlab vanilladot
#' @importFrom kernlab rbfdot
#' @importFrom kernlab polydot
#' @export
svmmaj <- function(
    X, y, lambda = 1,
    weights.obs = 1, weights.var = 1,
    scale = c("interval", "zscore", "none"),
    spline.knots = 0, spline.degree = 1L,
    kernel = vanilladot,
    kernel.sigma = 1, kernel.scale = 1, kernel.degree = 1, kernel.offset = 1,
    hinge = c("absolute", "quadratic", "huber", "logitistic"),
    hinge.delta = 1e-8,
    options = setSVMoptions(),
    initial.point = NULL,
    verbose = FALSE, na.action = na.omit,
    ...) {
  UseMethod("svmmaj")
}

#' @rdname svmmaj
#' @importFrom stats na.omit
#' @importFrom stats update.default
#' @importFrom kernlab vanilladot
#' @importFrom kernlab rbfdot
#' @importFrom kernlab polydot
#' @export
svmmaj.default <- function(
    X, y, lambda = 1,
    weights.obs = 1, weights.var = 1,
    scale = c("interval", "zscore", "none"),
    spline.knots = 0, spline.degree = 1L,
    kernel = vanilladot,
    kernel.sigma = 1, kernel.scale = 1, kernel.degree = 1, kernel.offset = 1,
    hinge = c("absolute", "quadratic", "huber", "logitistic"),
    hinge.delta = 1e-8,
    options = setSVMoptions(),
    initial.point = NULL,
    verbose = FALSE, na.action = na.omit,
    ...) {
  # ==================
  # INITIALISE DATA
  #------------------
  options <- setSVMoptions(options)

  n <- length(y)
  if (!is.factor(y)) y <- factor(y)

  if (n != NROW(X)) stop("Number of observations in y does not match X")

  # OBTAIN THE DIMENSION OF X
  if (!is.data.frame(X)) {
    if (!is.matrix(X)) X <- matrix(X, nrow = n)
    X <- data.frame(X = X)
  } else {
    X <- data.frame(X)
  }
  data <- cbind(y = y, X)

  # HANDLE NA VALUES
  data2 <- na.action(data)
  na.output <- attr(data2, "na.action")
  y <- data2$y
  X <- data.frame(data2[-1])
  n <- length(y)

  # TRANSFORM LABELS TO 1 AND -1
  classes <- sort(levels(y))
  if (length(classes) != 2) stop("Number of classes must be equal to 2")
  y <- sign((y == classes[2]) - .5)

  # INITIALISING WEIGHTS OF OBJECTS
  if (is.null(weights.obs) || length(weights.obs) == 1) {
    w <- rep(1, n)
  } else if (length(weights.obs) == 2) {
    weights.obs <- weights.obs[sort(names(weights.obs))]
    if (!identical(classes, names(weights.obs))) {
      warning(paste(
        "Class labels mismatch - weights.obs:",
        "labels do not match labels in y, match sorting order instead.\n",
        "classes of data:", classes, "\n",
        "classes of weights:", weights.obs, "\n"
      ))
    }
    w <- rep(1, n)
    w[y == -1] <- weights.obs[[1]]
    w[y == 1] <- weights.obs[[2]]
  } else if (length(weights.obs) == n) {
    w <- weights.obs
  } else if (length(weights.obs) > 1) {
    warning("Dimension mismatch - weights.obs, using standard weights instead")
    w <- rep(1, n)
  }

  # CHECK FOR NONPOSITIVE VALUES (IF NEEDED)
  if (options$check.positive) {
    if (any(weights.obs < 0)) {
      stop("weights should be nonnegative")
    }
    if (lambda < 0) {
      stop("lambda should be nonnegative")
    }
    if (spline.knots < 0) {
      stop("number of spline knots should be nonnegative")
    }
    if (spline.degree < 0) {
      stop("polynomial degree of spline should be nonnegative")
    }
    if (spline.degree == 0) {
      stop("spline degree of zero found, use majority vote")
    }
  }

  # INITIALISE DATA TRANSFORMATION
  X <- lapply(X, transformdata, scale, spline.knots, spline.degree)
  prop.data <- lapply(X, attributes)

  expansion <- sapply(prop.data, `[[`, "dim")[2, ]
  if (!is.null(unlist(sapply(prop.data, `[[`, "splineDegree")))) {
    spline.knots <- max(0, sapply(
      X,
      function(x) length(attr(x, "splineInterval"))
    ) - 1)
    spline.degree <- max(unlist(sapply(prop.data, `[[`, "splineDegree")))
  } else {
    spline.knots <- 0
    spline.degree <- 1
  }

  if (length(weights.var) == length(X)) {
    X <- mapply(`*`, X, weights.var)
  }
  X <- data.frame(X)
  X <- data.matrix(X)

  # DETERMINE EFFICIENT UPDATE
  kernel <- kernel()
  call <- match.call()
  kernel.param <- sapply(
    ls()[grepl("^kernel\\.", ls())],
    function(x) get(x)
  ) %>% as.list()
  names(kernel.param) <- gsub("^kernel\\.", "", names(kernel.param))
  attr(kernel, "kpar") <- kernel.param

  method <- getUpdate(
    X, kernel,
    decomposition = options$decomposition
  )


  # DEFINE MATRICES TO BE USED
  Z <- cbind("(constant)" = 1, method$Z)
  J <- diag(c(0, rep(1, method$x)), nrow = method$x + 1)

  # DEFINE HINGE FUNCTION
  hinge <- match.arg(
    hinge,
    c("absolute", "quadratic", "huber", "logitistic"),
    several.ok = TRUE
  )[1]
  eps <- options$eps
  newHinge <- getHinge(hinge, eps = eps)
  fixed.matrix <- attr(newHinge, "fixed.a")

  # ==============================
  # CREATE RANDOM STARTING POINT
  #------------------------------

  if (is.null(initial.point) || !(length(initial.point) == method$x + 1)) {
    theta <- rep(0, method$x + 1)
  } else {
    theta <- initial.point
  }

  q <- drop(Z %*% theta)

  # CALCULATE LOSS FUNCTION VALUE OF GIVEN POINT
  major.hinge <- newHinge(q, y)
  new.loss <- sum(major.hinge$loss * w) + lambda * sum(theta[-1]^2)
  old.loss <- Inf

  # =======================================================
  # CALCULATE MAJORIZATION MATRIX (X'AX)^-1*X' IF FIXED
  #-------------------------------------------------------
  # CHECK IF MATRIX (X'AX) IS FIXED
  if (fixed.matrix) {
    major.matrix <- solve(
      crossprod(sqrt(major.hinge$a * w) * Z) + lambda * J, t(Z)
    )
  }

  # gc()
  # =============================
  # PERFORMING ITERATION STEP
  #-----------------------------
  t <- 0
  endloop <- FALSE
  while (
    (new.loss == Inf ||
       abs(old.loss - new.loss) >= options$convergence * old.loss) &&
      !endloop &&
      t <= options$max.iter
  ) {
    t <- t + 1
    old.loss <- new.loss
    old.theta <- theta

    if (fixed.matrix) {
      theta <- major.matrix %*% (major.hinge$b * w)
    } else {
      matrixpart <- crossprod(sqrt(major.hinge$a * w) * Z) + lambda * J
      tZbw <- crossprod(Z, major.hinge$b * w)

      # IF MATRIXPART IS NEAR SINGULAR VALUE, INCREASE EPS VALUE
      while (rcond(matrixpart) <= .Machine$double.eps * 10) {
        endloop <- TRUE
        eps <- eps * 10
        newHinge <- getHinge(hinge, hinge.delta, eps = eps)
        major.hinge <- newHinge(q, y)
        matrixpart <- crossprod(sqrt(major.hinge$a * w) * Z) + lambda * J
        tZbw <- crossprod(Z, major.hinge$b * w)
        warning(
          paste(
            "Hinge function near singular value",
            eps
          )
        )
      }
      theta <- solve(matrixpart, tZbw)
    }

    if (t > options$increase.step) theta <- 2 * theta - old.theta
    q <- drop(Z %*% theta)

    # CALCULATE LOSS FUNCTION VALUE OF GIVEN POINT
    major.hinge <- update.default(major.hinge)
    new.loss <- sum(major.hinge$loss * w) + lambda * sum(theta[-1]^2)

    # PRINT ITERATION STEP
    if (verbose) {
      cat(sprintf(
        "iteration %5i, loss = %12.8f, relative difference = %10.8e\n",
        t, new.loss, (old.loss - new.loss) / old.loss
      ))
    }
  }


  # ==================================
  # CREATE Q.SVMMAJ-OBJECT
  #----------------------------------
  q <- structure(drop(q), class = "q.svmmaj", y = data2$y, classes = classes)
  attr(q, "yhat") <- factor(q > 0, levels = c(FALSE, TRUE), labels = classes)

  # ===========================================
  # OUTPUT:
  # call       = call function
  # lambda     = penalty regularization
  # loss       = optimal loss function
  # iterations = number of iterations
  # &
  # DATA PROPERTIES
  # DATA TRANSFORMATIONS
  # UPDATE METHODS
  # HINGE FUNCTION
  # OPTIMAL PARAMETERS
  #-------------------------------------------
  output <- list(
    call            = match.call(),
    lambda          = lambda,
    loss            = new.loss,
    iteration       = t,
    # data properties
    data            = data,
    classes         = classes,
    # data transformations
    Xnew            = X,
    y               = y,
    weights.obs     = w,
    na.output       = na.output,
    weights.var     = weights.var,
    propData        = prop.data,
    splineKnots     = spline.knots,
    splineDegree    = spline.degree,
    # update methods
    method          = method,
    # hinge function
    hinge           = newHinge,
    # optimal parameters
    theta           = theta,
    beta            = beta.theta(method, theta),
    q               = drop(q),
    nSV             = sum((q * y - 1) <= eps)
  )

  variables <- ls()
  rm(list = variables[variables != "output"])
  # gc()
  class(output) <- "svmmaj"
  return(output)
}


setSVMoptions <- function(x = NULL) {
  if (is.null(x)) {
    x <- list()
  }

  if (is.null(x$convergence)) {
    x$convergence <- 1e-8
  }

  if (is.null(x$eps)) {
    x$eps <- 1e-8
  }

  if (is.null(x$increase.step)) {
    x$increase.step <- 20L
  }

  if (is.null(x$check.positive)) {
    x$check.positive <- TRUE
  }

  if (is.null(x$decomposition)) {
    x$decomposition <- "svd"
  }

  if (is.null(x$max.iter)) {
    x$max.iter <- 5e4
  }

  return(x)
}
