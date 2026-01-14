#' Plot the weights of all attributes from the trained SVM model
#'
#' Shows, one graph per attribute, the weights of all
#' attributes. The type of graph depends on the type of the attribute: the
#' spline line of the corresponding attribute in case a spline has been used, a
#' bar plot for categorical and logical values, and a linear line for all other
#' type of the attribute values. This function cannot be used in a model with a
#' non-linear kernel.
#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @param object The model returned from \code{svmmaj}.
#' @param plotdim A vector of the form \code{c(nr, nc)}. Subsequent figures
#' will be drawn in an \code{nr}-by-\code{nc} array on the device.
#' @param ... other parameters given to the \code{plot} function
#' @export
#'
plotWeights <- function(object, plotdim = c(3, 3), ...) {
  nplot <- prod(plotdim)


  if (!object$method$linear) stop("nonlinear kernel is used")
  Xold <- data.matrix(object$data[-1])
  splineLength <- sapply(object$propData, `[[`, "dim")[2, ]
  beta <- object$beta
  k <- NCOL(object$data) - 1

  # GENERATE GRIDPOINTS
  grid <- matrix(rep(seq(0, 1, length.out = 100), k), ncol = k)
  grid <- t(t(grid) * as.vector(diff(apply(Xold, 2, range))) +
              apply(Xold, 2, min))
  grid <- data.frame(grid)

  X <- mapply(predict.transDat, grid,
    attrib = object$propData, SIMPLIFY = FALSE
  )

  l <- sapply(object$propData, `[[`, "dim")[2, ] # dim...
  at <- unlist(mapply(rep, 1:k, l))
  bspl <- lapply(1:k, function(i) beta[at == i])


  Splinesplus <- mapply(`%*%`, X, lapply(bspl, pmax, 0))
  Splinesmin <- mapply(`%*%`, X, lapply(bspl, pmin, 0))
  Splinessum <- mapply(`%*%`, X, bspl)
  ylim <- range(c(Splinesplus, Splinesmin, Splinessum))
  y_at <- (ylim[2] - ylim[1]) * .2
  ylim[1] <- ylim[1] - y_at

  if (is.matrix(Splinessum)) {
    Splinesplus <- data.frame(Splinesplus)
    Splinesmin <- data.frame(Splinesmin)
    Splinessum <- data.frame(Splinessum)
  }

  plot_pages <- vector(mode = "list", length = ceiling(k / nplot))
  for (j in 1:ceiling(k / nplot)) {
    # DETERMINE PLOT LAYOUT
    mini <- nplot * (j - 1) + 1
    maxi <- min(k, j * nplot)
    noi <- maxi - mini + 1
    plotlayout <- rep(0, nplot)
    plotlayout[1:noi] <- 1:noi
    plotlayout <- rbind(
      matrix(plotlayout, byrow = TRUE, nrow = plotdim[1], ncol = plotdim[2]),
      noi + 1
    )

    plots <- vector("list", length = noi)
    for (i in mini:maxi) {
      if (!is.null(object$propData[[i]]$values)) {
        # FACTOR
        plots[[i]] <- ggplot(data.frame(
          effect = bspl[[i]], treatment = object$propData[[i]]$values
        )) +
          geom_bar(aes(x = .data[["treatment"]], y = .data[["effect"]]),
            stat = "identity", position = "identity"
          ) +
          ylim(ylim) +
          ggtitle(colnames(Xold)[i])
      } else if (!is.null(object$propData[[i]]$type)) {
        # LOGICAL
        plots[[i]] <- ggplot(data.frame(
          effect = c(0, bspl[[i]]), treatment = c("FALSE", "TRUE")
        )) +
          geom_bar(aes(x = .data[["treatment"]], y = .data[["effect"]]),
            stat = "identity", position = "identity"
          ) +
          ylim(ylim) +
          ggtitle(colnames(Xold)[i])
      } else {
        # NUMERIC / SPLINES
        splines_dat <- data.frame(
          x = grid[, i],
          `Total` = Splinessum[[i]]
        )
        if (!is.null(object$propData[[i]]$splineDegree)) {
          splines_dat$`positive weights` <- Splinesplus[[i]]
          splines_dat$`negative weights` <- Splinesmin[[i]]
        }

        splines_dat <- melt(splines_dat,
          id.vars = "x",
          value.name = "value", variable.name = "Splines"
        )
        splits_col <- c(
          "Total" = "black", "negative weights" = "red", "positive weights" = "blue"
        )
        splits_lty <- c(
          "Total" = 1, "negative weights" = 2, "positive weights" = 3
        )

        plots[[i]] <- ggplot(splines_dat) +
          geom_line(aes(
            x = .data[["x"]], y = .data[["value"]],
            colour = .data[["Splines"]], linetype = .data[["Splines"]]
          )) +
          theme_light() +
          ylim(ylim) +
          xlab("") +
          ggtitle(colnames(Xold)[i]) +
          scale_colour_manual(name = "Splines", values = splits_col) +
          scale_linetype_manual(name = "Splines", values = splits_lty)

        if (!is.null(object$propData[[i]]$splineDegree)) {
          plots[[i]] <- plots[[i]] + ylab("I-spline")
        } else {
          plots[[i]] <- plots[[i]] + ylab(expression(x * beta))
        }
        X.bxp <- Xold[, i]

        plots[[i]] <- plots[[i]] + geom_rug(
          data = data.frame(
            x = X.bxp,
            y = predict.transDat(Xold[, i], attrib = object$propData[[i]]) %*% bspl[[i]]
          ),
          aes(x = .data[["x"]], y = .data[["y"]]),
          position = "jitter", sides = "b", alpha = 0.25
        )

        plots[[i]] <- plots[[i]] + theme(legend.position = "none")
      }
    }
    plot_page <- plots[mini:maxi]
    plot_page[[length(plot_page) + 1]] <- get_legend()
    plot_pages[[j]] <- do.call(grid.arrange, plot_page)
  }
  return(invisible(plot_pages))
}

get_legend <- function() {
  splits <- c("Total", "negative weights", "positive weights")
  splits_col <- structure(c("black", "red", "blue"), names = splits)
  splits_lty <- structure(1:3, names = splits)

  myggplot <- ggplot(expand.grid(x = 0:1, Splines = splits)) +
    geom_line(aes(
      x = .data[["x"]], y = .data[["x"]],
      colour = .data[["Splines"]], linetype = .data[["Splines"]]
    )) +
    scale_colour_manual(name = "Splines", values = splits_col) +
    scale_linetype_manual(name = "Splines", values = splits_lty)

  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
