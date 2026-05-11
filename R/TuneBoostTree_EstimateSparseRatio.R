#' Estimar proporção de zeros sem materializar toda a matriz
#' @noRd

TuneBoostTree_EstimateSparseRatio <- function(xData, sampleCols = 30L) {

  if(ncol(xData) == 0L) {
    return(0)
  }
  sampleCols <- min(as.integer(sampleCols), ncol(xData))
  sampledIndexes <- unique(as.integer(round(seq(1L, ncol(xData), length.out = sampleCols))))
  sampledMatrix <- data.matrix(xData[, sampledIndexes, drop = FALSE])
  storage.mode(sampledMatrix) <- "double"
  mean(sampledMatrix == 0, na.rm = TRUE)
}
####
## Fim
#

