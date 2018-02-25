#' @importFrom kernlab kernelMatrix
getUpdate <- function(X,
                      kernel =  vanilladot(),
                      decomposition = c('svd', 'chol', 'none')) {
  X <- data.matrix(X)
  decomposition <- match.arg(decomposition)
  #===================================================================
  #DETERMINE WHICH METHOD TO USE
  #-------------------------------------------------------------------
  #LINEAR KERNEL: QR-DECOMPOSITION
  # [ X = Z Q' ]
  if (class(kernel) == 'vanillakernel' & decomposition == 'chol') {
    RQ   <- qr(t(X))
    
    method <- list(
      type          = 'QR',
      x             = RQ$rank,
      X             = X,
      Z             = t(qr.R(RQ))[order(RQ$pivot), seq_len(RQ$rank)],
      RQ            = RQ,
      decomposition = decomposition
    )
  } else if (class(kernel) == 'vanillakernel' &
             decomposition == 'svd') {
    PLQ <- svd(X)
    k   <- NROW(X)
    
    method <- list(
      type          = 'svd',
      x             = length(PLQ$d),
      X             = X,
      Z             = PLQ$u * rep(PLQ$d, each = k),
      PLQ           = PLQ,
      decomposition = decomposition
    )
  } else if (class(kernel) == 'vanillakernel' &
             decomposition == 'none') {
    #LINEAR KERNEL: NO DECOMPOSITION
    # [ X = X ]
    x <- NCOL(X)
    method <- list(
      type          = 'no decomposition',
      x             = x,
      X             = X,
      Z             = X,
      decomposition = decomposition
    )
    
  } else if (class(kernel) != 'vanillakernel' &
             decomposition == 'chol') {
    #NONLINEAR KERNEL: PIVOTED CHOLESKY DECOMPOSITION
    # [ K   = Phi Phi' = Z Z' = P R R' P' ]
    # [ Phi = P R Q', Q' = UNDEFINED  ]
    options(warn = -1)
    K    <- kernelMatrix(kernel, X)
    Rp   <- chol(K, pivot = TRUE)
    options(warn = 0)
    r    <- sum(diag(Rp) / Rp[1] > 1e-3)
    pivot <- order(attr(Rp, 'pivot'))
    
    method <- list(
      type = 'Cholesky',
      x    = r,
      X    = X,
      Z    = t(Rp[seq_len(r) , order(attr(Rp, 'pivot'))])
    )
  } else if (class(kernel) != 'vanillakernel' &
             decomposition == 'svd') {
    K    <- kernelMatrix(kernel, X)
    PL2P <- eigen(K)
    n    <- NROW(X)
    k    <- sum(PL2P$values > 1e-3)
    
    PL2P$vectors <- PL2P$vectors[, seq_len(k)]
    PL2P$values  <- PL2P$values[seq_len(k)]
    
    method <- list(
      type = 'Eigen',
      x    = k,
      X    = X,
      PL2P = PL2P,
      Z    = PL2P$vectors * rep(sqrt(PL2P$values), each = n)
    )
  } else {
    stop('Cannot find perform decomposition on matrix')
  }
  #gc()
  class(method) <- 'theta'
  method$kernel <- kernel
  method$linear <- class(kernel) == 'vanillakernel'
  return(method)
}

#===================================================================
#CALCULATE THE INTERCEPT alpha GIVEN THE model AND theta
#-------------------------------------------------------------------
alpha.theta <- function(object, theta)
  return(theta[1,])

#===================================================================
#CALCULATE THE WEIGHTS beta GIVEN THE model AND theta
#-------------------------------------------------------------------
beta.theta <- function(object, theta) {
  if (!object$linear)
    return(NULL)
  b <- switch(
    object$decomposition,
    none = matrix(theta[-1,], ncol = 1),
    chol = qr.Q(object$RQ)[, seq_len(object$x)] %*% theta[-1,],
    svd = object$PLQ$v %*% theta[-1,]
  )
  #row.names(b) <- colnames(object$X)
  return(b)
}

#===================================================================
#CALCULATE THE PREDICTED VALUE q GIVEN THE model, theta AND x
#-------------------------------------------------------------------
qhat.theta <- function(object, theta, Xnew) {
  if (object$linear) {
    qhat <- theta[1] + data.matrix(Xnew) %*% beta.theta(object, theta)
  } else if (object$type == 'Cholesky') {
    QR   =  qr(object$Z)
    qhat <- theta[1,] +
      kernelMatrix(object$kernel, data.matrix(Xnew), object$X) %*%
      qr.Q(QR) %*% solve(t(qr.R(QR)), theta[-1,])
  } else if (object$type == 'Eigen') {
    qhat <- theta[1,] +
      kernelMatrix(object$kernel, data.matrix(Xnew), object$X) %*%
      object$PL2P$vectors %*% (sqrt(object$PL2P$values) * theta[-1,])
  }
  
  return(drop(qhat))
}

#===================================================================
#CALCULATE THE ADJUSTED z MATRIX GIVEN THE model AND x

Z.theta <- function(object, Xnew) {
  if (object$linear) {
    z <- Xnew
  } else if (object$type == 'Cholesky') {
    QR   =  qr(object$Z)
    z <-
      kernelMatrix(object$kernel, data.matrix(Xnew), object$X) %*%
      qr.Q(QR) %*% solve(t(qr.R(QR)))
  } else if (object$type == 'Eigen') {
    z <- kernelMatrix(object$kernel, data.matrix(Xnew), object$X) %*%
      object$PL2P$vectors %*% diag(sqrt(object$PL2P$values))
  }
  
  return(z)
}
