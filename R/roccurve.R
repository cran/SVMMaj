#' Plot the ROC curve of the predicted values
#' 
#' Given the predicted values \code{q} and its corresponding
#' observed classes \code{y}, it shows its separation performances
#' by showing the roc-curve.
#' 
#' @param q the predicted values
#' @param y a list of the actual classes of \code{q}
#' @param class the base class to show the roc-curve
#' @param ... additional parameters given as input to the \code{plot} function
#' @examples
#' model <- svmmaj(diabetes$X, diabetes$y)
#' roccurve(model$q)
#' @export
#' 
roccurve <- function(q, y = attr(q, 'y'), class = 1, ...){
	if(is.null(y)) stop('Input y not found')
	if(!is.factor(y)) y <- factor(y)
	
	neg     <- sum(q < 0)
	classes <- levels(y)
	obs     <- summary(y)

	ys    <- y[sort.int(q * (-1) ^ (class != 1), index.return = TRUE)$ix]
	c1    <- (ys == classes[-class]) / obs[-class]
	c2	  <- (ys == classes[ class]) / obs[ class]

  df <- data.frame(
    x = c(0, cumsum(c1)),
    y = c(0, cumsum(c2))
  )
  
  ggplot(df) +
    geom_line(aes_string(
      x = "x", y = "y")) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.4) +
    ggtitle(
      paste(
        'ROC-curve\n',
        'Class =', classes[class],',',
        'AUC ='  , formatC(sum(c1 * cumsum(c2)), digits = 4)
      )
    ) +
    xlab("false positive rate") +
    ylab("true positive rate") +
    theme_light() + coord_equal()
}

#' Returns the area under the curve value
#' 
#' Returns the area under the curve value as a fraction.
#' 
#' @param q the predicted values
#' @param y a list of the actual classes of \code{q}
#' @return the area under the curve value
#' @examples 
#' df   <- with(diabetes, cbind(y, X))
#' lm.y <- glm(y ~ ., data = df, family = binomial())
#' print(with(lm.y, auc(fitted.values, y)))
#' 
#' @export
#' 
auc <- function(q, y = attr(q, 'y')){
	if(is.null(y)) stop('Input y not found')
	if(!is.factor(y)) y <- factor(y)
	neg     <- sum(q < 0)
	classes <- levels(y)
  obs     <- summary(y)

	ys    <- y[sort.int(q, index.return = TRUE)$ix]
	c1    <- (ys == classes[2]) / obs[2]
	c2	  <- (ys == classes[1]) / obs[1]
	auc	  <- sum(c1 * cumsum(c2))
	return(auc)
}

