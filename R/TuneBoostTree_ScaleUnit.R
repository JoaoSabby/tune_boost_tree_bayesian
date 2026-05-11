#' Escalar parâmetros para hipercubo unitário
#' @noRd

TuneBoostTree_ScaleUnit <- function(parameterData, bounds) {

  parameterNames <- names(bounds)
  out <- as.data.frame(parameterData[, parameterNames, drop = FALSE], stringsAsFactors = FALSE)
  for(parameterName in parameterNames) {
    lower <- as.numeric(bounds[[parameterName]][1L])
    upper <- as.numeric(bounds[[parameterName]][2L])
    out[[parameterName]] <- (as.numeric(out[[parameterName]]) - lower) / max(upper - lower, .Machine$double.eps)
  }
  out
}
####
## Fim
#

