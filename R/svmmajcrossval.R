#' k-fold Cross-Validation of SVM-Maj
#'
#' This function performs a gridsearch of k-fold cross-validations using SVM-Maj
#' and returns the combination of input values which has the best forecasting
#' performance.
#'
#' @param X A data frame (or object coercible by
#' \code{\link[base]{as.data.frame}} to a data frame) consisting the attributes.
#' @param y A factor (or object coercible by \code{\link[base]{factor}} to a
#' factor) consisting the class labels.
#' @param search.grid A list with for each factor the range of values to search
#' for.
#' @param ...  Other arguments to be passed through \code{svmmaj}.
#' @param convergence Specifies the convergence criterion for \code{svmmaj}.
#' Default is \code{1e-08}.
#' @param weights.obs Weights for the classes.
#' @param mc.cores the number of cores to be used (for parallel computing)
#' @param options additional settings used in the \code{svmmaj} algorithm
#' @param check.positive Specifies whether a check should be performed for
#' positive \code{lambda} and \code{weights.obs}.
#' @param verbose \code{=TRUE} shows the progress of the
#' cross-validation.
#' @param ngroup The number of groups to be divided into.
#' @param groups A predetermined group division for performing the cross
#' validation.
#' @param return.model \code{=TRUE} estimates the model with the optimal
#' parameters.
#' @return \item{loss.opt}{ The minimum (weighted) missclassification rate
#' found in out-of-sample training along the search grid. } \item{param.opt}{
#' The level of the factors which gives the minimum loss term value.}
#' \item{loss.grp}{A list of missclassification rates per hold-out sample}
#' \item{groups}{A vector defining the cross-validation groups which has been
#' used.}
#' \item{qhat}{The estimated out-of-sample predicted values in the
#' cross-validation.}
#' \item{qhat.in}{The trained predicted values}
#' \item{param.grid}{ The matrix of all gridpoints which has been performed
#' during the cross-validation, with its corresponding weighted out-of-sample
#' missclassification rate.} \item{model}{ The \code{svmmaj}-object with the
#' estimated model using the optimal parameters found in the cross-validation.}
#' @author Hok San Yip, Patrick J.F. Groenen, Georgi Nalbantov
#' @seealso \code{\link[SVMMaj]{svmmaj}}
#' @references P.J.F. Groenen, G. Nalbantov and J.C. Bioch (2008)
#' \emph{SVM-Maj: a majorization approach to linear support vector machines
#' with different hinge errors.}
#' @examples
#'
#' Xt <- diabetes$X
#' yt <- diabetes$y
#'
#' ## performing gridsearch with k-fold cross-validation
#' results <- svmmajcrossval(
#'   Xt, yt,
#'   scale = "interval",
#'   mc.cores = 2,
#'   ngroup = 5,
#'   return.model = TRUE
#' )
#'
#' summary(results$model)
#' results
#' plot(results)
#' plot(results, "profile")
#' @import parallel
#' @export
svmmajcrossval <- function(
    X, y, search.grid = list(lambda = 2^seq(5, -5, length.out = 19)),
    ...,
    convergence = 1e-4, weights.obs = 1, check.positive = TRUE,
    mc.cores = getOption("mc.cores"), options = NULL,
    verbose = FALSE, ngroup = 5, groups = NULL, return.model = FALSE) {
  # PERFORMING CROSS VALIDATION USING A LIST OF PARAMETER SETTINGS
  # INITIALISES ALL COMBINATION OF PARAMETER SETTINGS AND
  # CALLS THE FUNCTION CROSSVAL.SVMMAJ

  if (!is.factor(y)) y <- factor(y)
  classes <- sort(levels(y))
  y <- sign((y == classes[2]) - .5)
  n <- length(y)

  if (ngroup > n) {
    warning("ngroup is larger than n, use jacknife cross-validation")
    ngroup <- n
  }

  if (!is.data.frame(X)) {
    # OBTAIN THE DIMENSION OF X
    if (!is.matrix(X)) X <- matrix(X, nrow = n)
    X <- data.frame(X = X)
  }
  # ==================================================
  # INITIALISE THE LEVELS OF ALL PARAMETERS
  # AND TRANSFORM INTO GRIDPOINTS
  #--------------------------------------------------
  params <- names(search.grid)
  nparam <- length(search.grid)

  params.fixed <- as.list(match.call(expand.dots = FALSE)[["..."]])
  param.kernel <- c(
    "scale",
    "kernel", "kernel.scale", "kernel.degree", "kernel.sigma",
    "kernel.offset", "weights.var", "spline.knots", "spline.degree"
  )

  params <- c(
    intersect(params, param.kernel),
    setdiff(params, param.kernel)
  )

  if ("lambda" %in% params) {
    search.grid$lambda <- sort(search.grid$lambda, decreasing = TRUE)
  }

  svmmajinput <- formals(svmmaj.default)
  G <- expand.grid(search.grid)

  # ===================================================
  # CHECKING FOR NONNEGATIVE VALUES
  #---------------------------------------------------
  # CHECK FOR NONPOSITIVE VALUES (IF NEEDED)
  if (check.positive) {
    if (any(weights.obs < 0)) {
      stop("weights should be nonnegative")
    }
    if (any(search.grid[["lambda"]] < 0)) {
      stop("lambda should be nonnegative")
    }
  }

  if (is.null(mc.cores)) {
    mc.cores <- detectCores() - 1
  }

  if (verbose) {
    verbose.group <- (mc.cores == 1)
    cat("Number of observations: ", NROW(X), "\n")
    cat("Varying parameters    : ", NCOL(G), "\n")
    cat("Number of gridpoints  : ", NROW(G), "\n")
  } else {
    verbose.group <- FALSE
  }


  # ====================================================
  # INITIALISING WEIGHTS OF OBJECTS
  #----------------------------------------------------
  if (!is.null(names(weights.obs)) && length(weights.obs) == 2) {
    weights.obs <- weights.obs[sort(names(weights.obs))]
  }

  w <- rep(1, length(y))
  if (length(weights.obs) == 2) {
    w[y == -1] <- weights.obs[[1]]
    w[y == 1] <- weights.obs[[2]]
  } else if (length(weights.obs) == length(y)) {
    w[] <- weights.obs
  }

  # ===================================================
  # INITIALIZE GROUP SIZE
  #---------------------------------------------------
  ngroup <- trunc(ngroup)
  if (is.null(groups)) {
    if (ngroup < 2) {
      stop("ngroup should be greater than or equal to 2")
    }
    if (ngroup > n) {
      stop("ngroup should be less than or equal to the number of observations")
    }

    if (ngroup >= n) {
      # Jackknife cross validation
      groups <- 1:n
      leave.out <- 1
    } else if (ngroup < n) {
      # Fixed number of groups
      leave.out <- trunc(n / ngroup)
      groups <- rep(1:ngroup, ceiling(n / ngroup))[seq_len(n)]
      groups <- sample(groups)
    }
  } else {
    # Prespecified groups
    ngroup <- max(groups)
    leave.out <- trunc(n / ngroup)
  }
  # ===================================================
  # PERFORM CROSS-VALIDATION
  #---------------------------------------------------
  if (verbose) cat("Start cross validation ... \r\n")
  res.cv <- parallelLapply(0:ngroup,
    # PER GROUP
    function(i) {
      outputs <- NULL
      if (verbose.group) {
        cat("   group", i, "of", ngroup, ": ")
      }

      svmmaj.fun <- svmmaj.default
      initial.point <- NULL
      lMat <- vector("numeric", NROW(G))
      qhat <- vector("list", NROW(G))
      iter <- vector("numeric", NROW(G))

      # look for parameters which influence the kernel matrix
      param.Z <- intersect(param.kernel, params)

      if (is.null(options) || !inherits(options, "list")) {
        options <- setSVMoptions()
      }
      options.old.kernel <- options
      options.old.kernel$decomposition <- "none"


      for (j in seq_len(NROW(G))) {
        svmmaj.fun <- svmmaj.default
        params.extended <- c(
          as.list(G[j, params]),
          params.fixed
        )
        names(params.extended)[seq_len(ncol(G))] <- params

        new.kernel <- TRUE
        #  !(j > 1 && isTRUE(all.equal(unlist(G[j, param.Z]),
        #                              unlist(G[j - 1, param.Z]))))

        if (!new.kernel) {
          params_other <- setdiff(names(params.extended), param.kernel)
          formals(svmmaj.fun)[params_other] <- params.extended[params_other]
          formals(svmmaj.fun)$scale <- "none"
          options.kernel <- options.old.kernel

          Z_i <- outputs$method$Z
          y_i <- y[groups != i]
        } else {
          formals(svmmaj.fun)[names(params.extended)] <- params.extended
          options.kernel <- options
          Z_i <- X[groups != i, ]
          y_i <- y[groups != i]
        }

        outputs <- svmmaj.fun(
          Z_i, y_i,
          weights.obs = w[groups != i],
          initial.point = initial.point, check.positive = FALSE,
          options = options.kernel,
          convergence = convergence
        )

        if (new.kernel) Z_ni <- X[groups == i | i == 0, ]
        # if(new.kernel) Z_ni = X.svmmaj(outputs, X[groups == i | i == 0,])

        # Predict out-of-sample
        qhat[[j]] <- predict.svmmaj(
          outputs,
          Z_ni, y[groups == i | i == 0],
          weights = w[groups == i | i == 0]
        )

        # Calculating number of correctly predicted objects
        lMat[j] <- sum(w[groups == i | i == 0] *
                         (qhat[[j]] * y[groups == i | i == 0] < 0)) / sum(w)
        iter[j] <- outputs$iteration

        if (verbose.group) cat("*")
      }
      if (verbose.group) cat("\n")
      return(list(
        losses = lMat,
        qhat   = qhat,
        iter   = iter
      ))
    },
    mc.cores = mc.cores,
    ...
  )
  if (verbose) cat("Getting optimal parameters ... \r\n")

  # =================================================
  # SAVE THE OPTIMAL VALUES
  #-------------------------------------------------
  loss_p_grp <- sapply(res.cv[-1], `[[`, "losses")

  if (nrow(G) > 1) {
    losses <- rowSums(sapply(res.cv[-1], `[[`, "losses"))
    index <- which.min(losses)
  } else {
    losses <- sapply(res.cv[-1], `[[`, "losses")
    index <- 1
  }

  param.opt <- G[index, , drop = FALSE]
  names(param.opt) <- params
  G <- cbind(G, loss = losses)

  qhat <- do.call(
    cbind,
    lapply(res.cv[-1], function(x) do.call(rbind, x$qhat))
  )
  qhat.in <- do.call(rbind, res.cv[[1]]$qhat)
  attr(qhat, "y") <- attr(res.cv[[1]]$qhat[[1]], "y")[order(groups)]
  attr(qhat.in, "y") <- attr(res.cv[[1]]$qhat[[1]], "y")

  iter.in <- mean(res.cv[[1]]$iter)
  gc()

  # =================================================
  # RETURN THE RESULTS
  #-------------------------------------------------
  results <- list(
    param.grid = G,
    iter = iter.in,
    loss.opt = losses[index],
    loss.grp = loss_p_grp,
    param.opt = param.opt,
    groups = groups,
    qhat = qhat,
    qhat.in = qhat.in,
    classes = classes
  )
  class(results) <- "svmmajcrossval"
  if (return.model) {
    formals(svmmaj.default)[params] <- as.list(param.opt)
    results$model <- svmmaj.default(
      X, y,
      weights.obs = w,
      initial.point = NULL, check.positive = FALSE,
      convergence = 1e-8, ...
    )
  }
  if (verbose) cat("Done \r\n")
  return(results)
}

#' Print SVMMaj cross validation results
#'
#' Prints the result from the cross validation procedure in
#' \code{\link{svmmajcrossval}}.
#'
#' @param x the cross-validation output from \code{\link{svmmajcrossval}}
#' @param ... ignored
#' @export
print.svmmajcrossval <- function(x, ...) {
  cat("Search grid: \r\n")
  search.grid <- lapply(
    x$param.grid[, !names(x$param.grid) %in% c("losses"), drop = FALSE],
    unique
  )
  print(search.grid)
  cat(
    "Optimal value ( missclassification rate = ",
    x$loss.opt, "): \r\n"
  )
  print(x$param.opt)

  if (!is.null(x$model)) {
    cat("Optimal model: \r\n")
    print(x$model)
  }
}

#' @rdname print.svmmajcrossval
#' @param object the output object from \code{svmmajcrossval}
#' @export
summary.svmmajcrossval <- function(object, ...) {
  x <- object
  cat("Search grid: \r\n")
  search.grid <- lapply(
    x$param.grid[, !names(x$param.grid) %in% c("losses"), drop = FALSE],
    unique
  )
  print(search.grid)
  cat(
    "Optimal value ( missclassification rate = ",
    x$loss.opt, "): \r\n"
  )
  print(x$param.opt)

  if (!is.null(x$model)) {
    cat("Optimal model: \r\n")
    print(x$model)
  }
}



#' Plot the cross validation output
#'
#' Shows the results of the cross validation graphically.
#' Possible graphics are among others the distribution of
#' the predicted values \code{q} per class per lambda value
#' and the misclassification rate per lambda.
#'
#' @import dplyr
#' @import ggplot2
#' @param x the \code{svmmajcrossval} object
#' @param type the type of graph being shown, possible values are
#'   \code{'grid'} for the missclassification rate per lambda value,
#'   \code{'profile'} the distribution of predicted values
#'   of the classes per lambda value
#' @param ... Further arguments passed to or from other methods.
#' @method plot svmmajcrossval
#' @importFrom stats as.formula
#' @importFrom stats quantile
#' @export
plot.svmmajcrossval <- function(x, type = "grid", ...) {
  params <- setdiff(colnames(x$param.grid), c("lambda", "loss"))

  if (type == "grid") {
    # plot loss function over search grid

    grps <- sapply(unique(x$groups), function(x, grp) sum(x == grp), grp = x$groups)
    grps <- sum(grps) / grps
    loss.grp <- x$loss.grp * rep(grps, each = NROW(x$param.grid))

    rng <- cbind(
      x$param.grid,
      min = apply(loss.grp, 1, min),
      max = apply(loss.grp, 1, max)
    )


    if ("lambda" %in% colnames(x$param.grid)) {
      p <- ggplot(rng) +
        geom_line(aes_string(x = "lambda", y = "loss")) +
        geom_ribbon(
          aes_string(x = "lambda", ymin = "min", ymax = "max"),
          fill = "gray", alpha = .3
        ) +
        scale_x_log10() +
        theme_light() +
        ggtitle("Cross-validation performance per grid value")
      if (length(params) > 0) {
        p <- p + facet_wrap(
          as.formula(paste("~", paste(params, collapse = "+")))
        )
      }

      p <- p + ylim(0, max(rng$max))
    } else {
      p <- ggplot(x$param.grid) +
        geom_line(aes_string(x = names(x$param.grid)[1], y = "loss")) +
        theme_light() +
        ggtitle("Cross-validation performance per grid value")

      if (length(params) > 1) {
        p <- p + facet_wrap(
          as.formula(paste("~", paste(params[-1], collapse = "+")))
        )
      }
    }

    p
  } else if (type == "profile") {
    if (!"lambda" %in% names(x$param.grid)) {
      stop("No lambda found")
    }

    qhat <- x$qhat
    qhat.in <- x$qhat.in
    df <- data.frame(
      lambda = x$param.grid$lambda,
      qhat = as.numeric(qhat),
      sample = "out_of_sample",
      y = rep(
        factor(attr(qhat, "y"), levels = c(-1, 1)),
        each = nrow(x$param.grid)
      )
    )

    for (i in params) df[i] <- x$param.grid[i]

    df.ins <- data.frame(
      lambda = x$param.grid$lambda,
      qhat = as.numeric(qhat.in),
      sample = "in_sample",
      y = rep(
        factor(attr(qhat.in, "y"), levels = c(-1, 1)),
        each = nrow(x$param.grid)
      )
    )
    for (i in params) df.ins[i] <- x$param.grid[i]
    df <- rbind(df, df.ins)

    levels(df$y) <- x$classes

    df_grp <- do.call(
      group_by_,
      c(list(df, "lambda", "y", "sample"), as.list(params))
    ) %>%
      summarize(
        q_low  = quantile(qhat, .20),
        q_high = quantile(qhat, .80),
        qhat   = quantile(qhat, .50)
      ) %>%
      ungroup()

    p <- ggplot(df_grp, aes_string(x = "lambda")) +
      scale_x_log10() +
      geom_line(aes_string(y = "qhat", color = "y"), size = 1.5) +
      geom_ribbon(
        aes_string(ymin = "q_low", ymax = "q_high", fill = "y"),
        alpha = .3
      ) +
      ggtitle("Distribution of predicted values per class per lambda") +
      ylab("Predicted value")
    if (length(params) > 0) {
      p <- p + facet_grid(
        as.formula(paste("sample ~", paste(params, collapse = "+"))),
        labeller = "label_both"
      )
    } else {
      p <- p + facet_grid(. ~ sample)
    }

    p
  }
}


parallelLapply <- function(X, FUN, ..., mc.cores = detectCores()) {
  if (.Platform$OS.type == "windows") {
    cl <- makeCluster(mc.cores)
    result <- parLapply(cl = cl, X = X, fun = FUN)
    stopCluster(cl)
  } else {
    result <- mclapply(X = X, FUN = FUN, mc.cores = mc.cores)
  }
  return(result)
}
