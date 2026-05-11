#' Otimizar limiar de decisão a partir de predições de validação cruzada
#' @noRd

TuneBoostTree_OptimizeThresholdCv <- function(
  balancedFolds,
  hyperparameters,
  nRounds,
  seed,
  nThreads,
  nWorkersFolds,
  evalMetric,
  engine,
  prAucBackend = "auto"
) {

  predictionSummary <- TuneBoostTree_RunCvPredictions(
    balancedFolds,
    hyperparameters,
    nRounds,
    seed,
    nThreads,
    nWorkersFolds,
    evalMetric,
    engine,
    prAucBackend
  )
  TuneBoostTree_OptimizeThreshold(predictionSummary$actual, predictionSummary$predicted)
}
####
## Fim
#

