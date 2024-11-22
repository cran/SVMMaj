#' Out-of-Sample Prediction from Unseen Data.
#'
#' This function predicts the predicted value (including intercept), given a
#' previous trained model which has been returned by
#' \code{\link[SVMMaj]{svmmaj}}.
#'
#' @importFrom scales hue_pal
#' @param object Model which has been trained beforehand using
#' \code{\link[SVMMaj]{svmmaj}}.
#' @param X.new Attribute matrix of the objects to be predicted, which has the
#' same number of attributes as the untransformed attribute matrix in
#' \code{model}.
#' @param y The actual class labels (only if \code{show.plot==TRUE}).
#' @param weights The weight of observation as the relative importance of the
#' prediction error of the observation.
#' @param show.plot If \code{show.plot=TRUE}, it plots the density of the
#' predicted value for both class labels, if \code{y} is not specified, the
#' density of all objects will be plotted.
#' @param ...  Arguments to be passed to methods.
#' @return The predicted value (including intercept) of class \code{q.svmmaj},
#' with attributes: \item{y}{The observed class labels of each object.}
#' \item{yhat}{ he predicted class labels of each object.} \item{classes}{The
#' class labels.}
#' @author Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov
#' @seealso \code{\link[SVMMaj]{svmmaj}}
#' @references P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008)
#' \emph{SVM-Maj: a majorization approach to linear support vector machines with
#' different hinge errors.}
#' @method predict svmmaj
#' @examples
#'
#' attach(AusCredit)
#'
#' ## model training
#' model <- svmmaj(X[1:400, ], y[1:400], hinge = "quadratic", lambda = 1)
#' ## model prediction
#' q4 <- predict(model, X[-(1:400), ], y[-(1:400)], show.plot = TRUE)
#' q4
#' @export
predict.svmmaj <- function(
    object,
    X.new, y = NULL, weights = NULL,
    show.plot = FALSE, ...) {
  k <- NCOL(object$data) - 1
  if (!is.data.frame(X.new)) {
    if (!is.matrix(X.new)) X.new <- matrix(X.new, ncol = k)
    X.new <- data.frame(X = X.new)
  }
  # TRANSFORM DATA X, IF NEEDED
  Xnew <- mapply(predict.transDat, X.new,
    attrib = object$propData, SIMPLIFY = FALSE
  )
  if (length(object$weights.var) == k) {
    Xnew <- mapply(`*`, Xnew, object$weights.var)
  }
  Xnew <- data.matrix(data.frame(Xnew))

  # INITIALISING WEIGHTS OF OBJECTS
  if (!is.null(names(weights)) && length(weights) == 2) {
    weights <- weights[sort(names(weights))]
    if (!identical(object$classes, names(weights))) {
      warning(paste(
        "Class labels mismatch - weights.obs:",
        "labels do not match labels in y, match sorting order instead.\n",
        "classes of data:", object$classes, "\n",
        "classes of weights:", weights, "\n"
      ))
    }
  }

  if (!is.null(y)) {
    classes <- sort(levels(factor(y)))
    if (length(classes) > 2) {
      warning("Number of classes must not equal to 2")
      y2 <- rep(-1, length(y))
    }
    if (length(y) != NROW(Xnew)) {
      stop("Number of observations in y does not match with X")
    }
    y2 <- sign((y == classes[2]) - .5)

    w <- rep(1, length(y))
    if (length(weights) == 2) {
      w[y2 == -1] <- weights[[1]]
      w[y2 == 1] <- weights[[2]]
    } else if (length(weights) == length(y)) {
      w <- weights
    } else if (length(weights) > 1) {
      warning(paste(
        "Dimension mismatch - weights.obs,",
        "using standard weights instead"
      ))
    }
  } else {
    w <- NULL
  }

  # PREDICT THE VALUES OF THE PREDICTED VALUE INCLUDING THE INTERCEPT (Q-TILDE)
  q <- structure(
    qhat.theta(object$method, object$theta, Xnew),
    class = "q.svmmaj",
    y = y, classes = object$classes, weights = w
  )
  attr(q, "yhat") <- factor(q > 0,
    levels = c(FALSE, TRUE),
    labels = object$classes
  )

  if (show.plot && !is.null(y)) {
    print(plot.svmmaj(list(q = q, y = y, classes = object$classes)))
  } else if (show.plot && is.null(y)) {
    ggplot(data.frame(q = unclass(q))) +
      geom_density(
        aes(x = q),
        colour = hue_pal()(1),
        fill = hue_pal()(1),
        alpha = 0.5
      ) +
      ggtitle("Density of predicted values") +
      theme_light()
  }
  return(q)
}

#' Returns transformed attributes
#'
#' For efficiency use in svmmajcrossval
#'
#' @param object Model which has been trained beforehand using
#' \code{\link[SVMMaj]{svmmaj}}.
#' @param X.new Attribute matrix of the objects to be predicted, which has the
#' same number of attributes as the untransformed attribute matrix in
#' \code{model}.
#' @param weights The weight of observation as the relative importance of the
#' prediction error of the observation.
X.svmmaj <- function(object, X.new, weights = NULL) {
  k <- NCOL(object$data) - 1
  if (!is.data.frame(X.new)) {
    if (!is.matrix(X.new)) X.new <- matrix(X.new, ncol = k)
    X.new <- data.frame(X = X.new)
  }
  # TRANSFORM DATA X, IF NEEDED
  Xnew <- mapply(predict.transDat, X.new,
    attrib = object$propData, SIMPLIFY = FALSE
  )
  if (length(object$weights.var) == k) {
    Xnew <- mapply(`*`, Xnew, object$weights.var)
  }
  Xnew <- data.matrix(data.frame(Xnew))

  z <- Z.theta(object$method, Xnew)

  return(z)
}
