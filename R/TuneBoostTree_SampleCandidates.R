#' Amostrar candidatos do otimizador interno
#' @noRd

TuneBoostTree_SampleCandidates <- function(bounds, n) {

  parameterNames <- names(bounds)
  n <- as.integer(n)
  out <- as.data.frame(setNames(rep(list(numeric(n)), length(parameterNames)), parameterNames))
  if(n <= 0L) {
    return(out)
  }
  for(parameterName in parameterNames) {
    lower <- as.numeric(bounds[[parameterName]][1L])
    upper <- as.numeric(bounds[[parameterName]][2L])
    out[[parameterName]] <- stats::runif(n, lower, upper)
  }
  TuneBoostTree_NormalizeParams(out, parameterNames)
}
####
## Fim
#

