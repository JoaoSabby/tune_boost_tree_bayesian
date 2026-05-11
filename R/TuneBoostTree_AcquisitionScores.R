#' Pontuar conjunto de candidatos com posterior gaussiano
#' @noRd

TuneBoostTree_AcquisitionScores <- function(history, pool, bounds, acq = "ucb", kappa = 2.576, eps = 0) {

  parameterNames <- names(bounds)
  xTrain <- TuneBoostTree_ScaleUnit(history[, parameterNames, drop = FALSE], bounds)
  xPool <- TuneBoostTree_ScaleUnit(pool[, parameterNames, drop = FALSE], bounds)
  y <- as.numeric(history$Value)
  yMean <- mean(y)
  ySd <- stats::sd(y)
  if(!is.finite(ySd) || ySd <= 1e-12) {
    ySd <- 1
  }
  yScaled <- (y - yMean) / ySd
  lengthScale <- rep(0.35, length(parameterNames))
  kTrain <- TuneBoostTree_RbfKernel(xTrain, xTrain, lengthScale) + diag(1e-6, nrow(xTrain))
  cholK <- tryCatch(chol(kTrain), error = function(e) NULL)
  if(is.null(cholK)) {
    return(stats::runif(nrow(pool)))
  }
  alpha <- backsolve(cholK, backsolve(cholK, yScaled, transpose = TRUE))
  kPool <- TuneBoostTree_RbfKernel(xPool, xTrain, lengthScale)
  mu <- as.numeric(kPool %*% alpha) * ySd + yMean
  v <- backsolve(cholK, t(kPool), transpose = TRUE)
  sigma <- sqrt(pmax(1 - colSums(v * v), 1e-12)) * ySd
  acq <- tolower(as.character(acq)[1L])
  if(identical(acq, "ucb")) {
    return(mu + as.numeric(kappa)[1L] * sigma)
  }
  z <- (mu - max(y) - as.numeric(eps)[1L]) / pmax(sigma, 1e-12)
  if(identical(acq, "poi")) {
    return(stats::pnorm(z))
  }
  improvement <- (mu - max(y) - as.numeric(eps)[1L]) * stats::pnorm(z) + sigma * stats::dnorm(z)
  pmax(improvement, 0)
}
####
## Fim
#

