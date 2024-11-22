#' Show the classification performance
#'
#' Given the predicted value \code{q} and the observed classes \code{y},
#' it shows an overview of the prediction performances with hit rates,
#' misclassification rates, true positives (TP), false positives (FP)
#' and precision.
#'
#' @param q the predicted values
#' @param y a list of the actual classes of \code{q}
#' @param classes a character vector with the labels of the two classes
#' @param weights an optional parameter to specify a weighted hit rate and
#'   misclassification rate
#' @return a list with three elements, \code{matrix} equals the confusion
#'   matrix,\code{overall} equals the overall prediction performance and
#'   in \code{measures} the measures per class is stored.
#' @export
classification <- function(q, y, classes = c("-1", "1"), weights = NULL) {
  # Compute confusion matrix
  y_obs <- cbind(y == classes[[1]], y == classes[[2]])
  y_pred <- cbind(q <= 0, q > 0)
  class_tab <- crossprod(y_obs, y_pred)

  conf_tab <- array(0, dim = c(3, 3))
  conf_tab[-3, -3] <- class_tab
  conf_tab[3, ] <- colSums(conf_tab)
  conf_tab[, 3] <- rowSums(conf_tab)
  dimnames(conf_tab) <- list(
    "Observed (y)"    = c(classes, "Total"),
    "Predicted(yhat)" = c(classes, "Total")
  )

  # Compute the overall rates
  hr <- sum(diag(class_tab) / sum(class_tab))
  if (is.null(weights)) {
    overall <- matrix(c(hr, 1 - hr), ncol = 1)
    dimnames(overall) <- list(
      list(
        "   hit rate                          ",
        "   misclassification rate            "
      ),
      list("")
    )
  } else {
    hr.w <- (q <= 0) == (y == classes[[1]])
    hr.w <- sum(hr.w * weights) / sum(weights)
    overall <- matrix(c(hr, hr.w, 1 - hr, 1 - hr.w), ncol = 1)
    dimnames(overall) <- list(
      list(
        "   hit rate                          ",
        "   weighted hit rate                 ",
        "   misclassification rate            ",
        "   weighted missclassification rate  "
      ),
      list("")
    )
  }
  # Compute classification measures per class
  positive <- diag(class_tab) / rowSums(class_tab)
  measures <- cbind(
    "       TP" = positive,
    "       FP" = 1 - positive,
    "Precision" = diag(class_tab) / colSums(class_tab)
  )
  rownames(measures) <- paste("   ", classes)

  return(list(matrix = conf_tab, overall = overall, measures = measures))
}
